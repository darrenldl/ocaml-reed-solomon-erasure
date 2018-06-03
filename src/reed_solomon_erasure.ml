open Tables
open Errors
open Ops

type data = [ `String of string | `Bytes of bytes ]

type reed_solomon = {
  data_shard_count   : int;
  parity_shard_count : int;
  total_shard_count  : int;
  matrix             : Matrix.t;
  tree               : Inversion_tree.t;
}

let build_matrix (data_shards : int) (total_shards : int) : Matrix.t =
  let vandermonde = Matrix.vandermonde total_shards data_shards in

  let top = Matrix.sub_matrix vandermonde 0 0 data_shards data_shards in

  Matrix.multiply vandermonde top

let make (data_shards : int) (parity_shards : int) : (reed_solomon, error) result =
  if      data_shards   = 0 then
    Error TooFewDataShards
  else if parity_shards = 0 then
    Error TooFewParityShards
  else if data_shards + parity_shards > 256 then
    Error TooManyShards
  else (
    let total_shards = data_shards + parity_shards in
    let matrix       = build_matrix data_shards total_shards in

    Ok { data_shard_count   = data_shards;
         parity_shard_count = parity_shards;
         total_shard_count  = total_shards;
         matrix;
         tree               = Inversion_tree.make data_shards parity_shards; }
  )

let data_shard_count (r : reed_solomon) : int =
  r.data_shard_count

let parity_shard_count (r : reed_solomon) : int =
  r.parity_shard_count

let total_shard_count (r : reed_solomon) : int =
  r.total_shard_count

let get_parity_rows (r : reed_solomon) : bytes array =
  let parity_rows = Array.make r.parity_shard_count Bytes.empty in

  for i = r.data_shard_count to (r.total_shard_count) - 1 do
    parity_rows.(i) <- Matrix.get_row r.matrix i
  done;

  parity_rows

let code_single_slice
    (matrix_rows : bytes array)
    (i_input     : int)
    (input       : string)
    (outputs     : bytes array)
  : unit =
  let code_single_output matrix_rows input i_row output =
    let matrix_row_to_use = matrix_rows.(i_row).%(i_input) in

    if i_input = 0 then
      Galois.mul_slice     matrix_row_to_use input output
    else
      Galois.mul_slice_xor matrix_row_to_use input output
  in

  let code_single_output_partial = code_single_output matrix_rows input in

  Array.iteri
    code_single_output_partial
    outputs

let code_some_slices
    (r           : reed_solomon)
    (matrix_rows : bytes array)
    (inputs      : string array)
    (outputs     : bytes array)
  : unit =
  for i_input = 0 to (r.data_shard_count) - 1 do
    code_single_slice matrix_rows i_input inputs.(i_input) outputs
  done

let check_some_slices_with_buffer
    (r           : reed_solomon)
    (matrix_rows : bytes array)
    (inputs      : string array)
    (to_check    : data array)
    (buffer      : bytes array)
  : bool =
  code_some_slices r matrix_rows inputs buffer;

  let check_single i expected_parity_shard =
    Data.eq (`Bytes expected_parity_shard) to_check.(i)
  in

  let at_least_one_mismatch_present =
    Array.mem false
      (Array.mapi
         check_single
         buffer)
  in

  not at_least_one_mismatch_present

module Helper = struct
  let bytes_array_to_string_array (arr : bytes array) : string array =
    Array.map Bytes.unsafe_to_string arr
end

module Checker = struct
  type check_slice_index_op = All | Data | Parity
  type check_piece_count_op = All | Data | Parity | ParityBuf

  let check_option_shards_and_get_size (slices : bytes option array) : (int, error) result =
    let update_size (size : int option ref) (slice : bytes option) =
      match slice with
      | None -> ()
      | Some b ->
        match !size with
        | None   -> size := Some (Bytes.length b)
        | Some _ -> ()
    in

    let size                = ref None in
    let update_size_partial = update_size size in

    (* record size of first shard *)
    Array.iter
      update_size_partial
      slices;

    let check_size_same (size : int) (acc : bool) (slice : bytes option) =
      match slice with
      | None   -> true
      | Some b -> acc && size = Bytes.length b
    in

    match !size with
    | None      -> Error TooFewShardsPresent
    | Some size ->
      if size = 0 then
        Error EmptyShard
      else (
        let check_size_same_partial = check_size_same size in
        let all_sizes_same =
          Array.fold_left
            check_size_same_partial
            true
            slices
        in
        if all_sizes_same then
          Ok size
        else
          Error IncorrectShardSize
      )

  let check_slice_index
      (r     : reed_solomon)
      (op    : check_slice_index_op)
      (index : int)
    : (unit, error) result =
    let ub = match op with
      | All    -> r.total_shard_count
      | Data   -> r.data_shard_count
      | Parity -> r.parity_shard_count
    in

    if 0 <= index && index < ub then
      Ok ()
    else
      Error InvalidIndex

  let check_piece_count
      (r      : reed_solomon)
      (op     : check_piece_count_op)
      (pieces : 'a array)
    : (unit, error) result =
    let exact_match = match op with
      | All       -> r.total_shard_count
      | Data      -> r.data_shard_count
      | Parity    -> r.parity_shard_count
      | ParityBuf -> r.parity_shard_count
    in

    let too_few_error = match op with
      | All       -> TooFewShards
      | Data      -> TooFewDataShards
      | Parity    -> TooFewParityShards
      | ParityBuf -> TooFewBufferShards
    in

    let too_many_error = match op with
      | All       -> TooManyShards
      | Data      -> TooManyDataShards
      | Parity    -> TooManyParityShards
      | ParityBuf -> TooManyBufferShards
    in

    let piece_count = Array.length pieces in

    if      piece_count < exact_match then
      Error too_few_error
    else if piece_count > exact_match then
      Error too_many_error
    else
      Ok ()

  let check_if_slice_is_of_size (size : int) (acc : bool) (slice : string) : bool =
    acc && size = String.length slice

  let check_slices_multi
      (r      : reed_solomon)
      (slices : string array)
    : (unit, error) result =
    let size = Array.length slices in
    if size = 0 then
      Error EmptyShard
    else
      let check_if_slice_is_of_size_partial =
        check_if_slice_is_of_size size
      in

      let all_sizes_same =
        Array.fold_left
          check_if_slice_is_of_size_partial
          false
          slices
      in

      if all_sizes_same then
        Ok ()
      else
        Error IncorrectShardSize

  let check_slices_single
      (slice_left  : string)
      (slice_right : string)
    : (unit, error) result =
    if String.length slice_left = String.length slice_right then
      Ok ()
    else
      Error IncorrectShardSize

  let check_slices_multi_single
      (r      : reed_solomon)
      (slices : string array)
      (slice  : string)
    : (unit, error) result =
    match check_slices_multi r slices with
    | Error _ as e -> e
    | Ok _         -> check_slices_single slices.(0) slice

  let check_slices_multi_multi
      (r            : reed_solomon)
      (slices_left  : string array)
      (slices_right : string array)
    : (unit, error) result =
    match check_slices_multi r slices_left with
    | Error _ as e -> e
    | Ok _ ->
      match check_slices_multi r slices_right with
      | Error _ as e -> e
      | Ok _ ->
        check_slices_single slices_left.(0) slices_right.(0)
end

module Encode = struct
  module StringInput = struct
    let encode_single_sep
        (r           : reed_solomon)
        (i_data      : int)
        (single_data : string)
        (parity      : bytes array)
      : (unit, error) result =
      let index_check_result       = Checker.check_slice_index r Checker.Data   i_data in
      let piece_count_check_result = Checker.check_piece_count r Checker.Parity parity in
      let slices_check_result      = Checker.check_slices_multi_single r (Helper.bytes_array_to_string_array parity) single_data in
      match index_check_result with
      | Error _ as e -> e
      | Ok _ ->
        match piece_count_check_result with
        | Error _ as e -> e
        | Ok _ ->
          match slices_check_result with
          | Error _ as e -> e
          | Ok _ ->
            begin
              let parity_rows = get_parity_rows r in

              Ok (code_single_slice
                    parity_rows
                    i_data
                    single_data
                    parity)
            end
  end

  module ByteInput = struct
    let encode_single_sep
        (r           : reed_solomon)
        (i_data      : int)
        (single_data : bytes)
        (parity      : bytes array)
      : (unit, error) result =
      StringInput.encode_single_sep r i_data (Bytes.unsafe_to_string single_data) parity
  end
end

module Verify = struct
  module StringInput = struct
  end

  module ByteInput = struct
  end
end

module Reconstruct = struct
  module StringInput = struct
  end

  module ByteInput = struct
  end
end
