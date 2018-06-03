(* AUDIT
 *
 * Use of `Bytes.unsafe_to_string`
 *
 *   Some functions make use of `Bytes.unsafe_to_string`
 *
 *   This is safe, however, as all use of Bytes.unsafe_to_string
 *   are used internall and used in one direction only
 *   i.e. only from bytes to string, never from string to bytes.
 *
 *   This means all functions already possess the mutable ownership
 *   of data prior to generating an immutable ownership of data.
 *
 *   The overall scheme is heavily based on the ownership scheme in
 *   the Rust version which makes use of immutable and mutable
 *   references extensively to enforce safety.
 *
 *   The rough correspondence of types to the Rust version is shown below
 *     - string       -> &[u8]
 *     - string array -> &[&[u8]]
 *     - bytes        -> &mut [u8]
 *     - bytes  array -> &mut [&mut [u8]]
 *
 *     It should be clear that the correspondence is not perfect as
 *     OCaml does not have borrow checker, and thus cannot enforce
 *     mutability constraints.
 *
 * Thread safety
 *
 *   The codec has a component, `tree : Inversion_tree.t`, that may
 *   mutate during operation, the component is a cache of reconstruction
 *   matrices.
 *
 *   The component is currently NOT protected by any locks,
 *   this the entire codec is not thread safe.
 *
 *   Protection will be added when OCaml is multicore enabled
 *   and then it can be decided which concurrency primitives
 *   are appropriate.
 *
 * Use of `Array.sub`
 *
 *   `Array.sub` is similar to mutable slicing in Rust,
 *   and when used with `bytes array`, it equates to shared
 *   mutable ownership to the data.
 *
 *   This is not safe in general, but these calls are only
 *   used to generate parameters for tail calls to other
 *   functions in this library. So the biggest risk would be
 *   incorrect indexing, this is addressed by `Helper.array_split_at` *)

open Tables
open Ops

type data = [ `String of string | `Bytes of bytes ]

type reed_solomon = {
  data_shard_count   : int;
  parity_shard_count : int;
  total_shard_count  : int;
  matrix             : Matrix.t;
  tree               : Inversion_tree.t;
}

type error = TooFewShards
           | TooManyShards
           | TooFewDataShards
           | TooManyDataShards
           | TooFewParityShards
           | TooManyParityShards
           | TooFewBufferShards
           | TooManyBufferShards
           | IncorrectShardSize
           | TooFewShardsPresent
           | EmptyShard
           | InvalidShardFlags
           | InvalidIndex

type sbs_error = TooManyCalls
               | LeftoverShards
               | RSError of error

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
    (to_check    : string array)
    (buffer      : bytes array)
  : bool =
  code_some_slices r matrix_rows inputs buffer;

  let check_single
      (to_check              : string array)
      (i                     : int)
      (expected_parity_shard : bytes)
    : bool =
    Bytes.unsafe_to_string expected_parity_shard = to_check.(i)
  in

  let check_single_partial = check_single to_check in

  let at_least_one_mismatch_present =
    Array.mem false
      (Array.mapi
         check_single_partial
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
  (* AUDIT
   *
   * Error detection responsibilities
   * Terminologies and symbols :
   *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
   *   X := A, B, C   : X needs to handle responsibilities A, B, C
   *
   * Encode methods
   *
   * `encode_single_shard`     =ALL=> `encode_single`
   * `encode_single_shard_sep` =ALL=> `encode_single_sep`
   * `encode_shards`           =ALL=> `encode`
   * `encode_shards_sep`       =ALL=> `encode_sep`
   * `encode_single` :=
   *   - check index `i_data` within range [0, data shard count)
   *   - check length of `slices` matches total shard count exactly
   *   - check consistency of length of individual slices
   * `encode_single_sep` :=
   *   - check index `i_data` within range [0, data shard count)
   *   - check length of `parity` matches parity shard count exactly
   *   - check consistency of length of individual parity slices
   *   - check length of `single_data` matches length of first parity slice
   * `encode` :=
   *   - check length of `slices` matches total shard count exactly
   *   - check consistency of length of individual slices
   * `encode_sep` :=
   *   - check length of `data` matches data shard count exactly
   *   - check length of `parity` matches parity shard count exactly
   *   - check consistency of length of individual data slices
   *   - check consistency of length of individual parity slices
   *   - check length of first parity slice matches length of first data slice *)

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

    let encode_single
        (r           : reed_solomon)
        (i_data      : int)
        (slices      : bytes array)
      : (unit, error) result =
      let index_check_result       = Checker.check_slice_index r Checker.Data i_data in
      let piece_count_check_result = Checker.check_piece_count r Checker.All  slices in
      let slices_check_result      = Checker.check_slices_multi r (Helper.bytes_array_to_string_array slices) in
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
              let output = Array.sub slices r.data_shard_count (Array.length slices) in
              let input  = Bytes.unsafe_to_string slices.(i_data) in
              encode_single_sep r i_data input output
            end

    let encode_sep
        (r      : reed_solomon)
        (data   : string array)
        (parity : bytes array)
      : (unit, error) result =
      let piece_count_check_result_data   = Checker.check_piece_count r Checker.Data   data in
      let piece_count_check_result_parity = Checker.check_piece_count r Checker.Parity parity in
      let slices_check_result             = Checker.check_slices_multi_multi r data (Helper.bytes_array_to_string_array parity) in
      match piece_count_check_result_data with
      | Error _ as e -> e
      | Ok _ ->
        match piece_count_check_result_parity with
        | Error _ as e -> e
        | Ok _ ->
          match slices_check_result with
          | Error _ as e -> e
          | Ok _ ->
            begin
              let parity_rows = get_parity_rows r in
              Ok(code_some_slices
                   r
                   parity_rows
                   data
                   parity)
            end

    let encode
        (r      : reed_solomon)
        (slices : bytes array)
      : (unit, error) result =
      let piece_count_check_result = Checker.check_piece_count r Checker.All slices in
      let slices_check_result      = Checker.check_slices_multi_multi r slices (Helper.bytes_array_to_string_array slices) in
      match piece_count_check_result with
      | Error _ as e -> e
      | Ok _ ->
        match slices_check_result with
        | Error _ as e -> e
        | Ok _ ->
          begin
            let input = Array.sub 
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

    let encode_single = StringInput.encode_single
  end
end

module Verify = struct
  (* AUDIT
   *
   * Error detection responsibilities
   * Terminologies and symbols :
   *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
   *   X := A, B, C   : X needs to handle responsibilities A, B, C
   *
   * Verify methods
   *
   * `verify_shards`             =ALL=> `verify_shards_with_buffer`
   * `verify_shards_with_buffer` =ALL=> `verify_with_buffer`
   * `verify` :=
   *   - check length of `slices` matches total shard count exactly
   *   - check consistency of length of individual slices
   *
   *   Generates buffer then passes control to verify_with_buffer *)

  module StringInput = struct
  end

  module ByteInput = struct
  end
end

module Reconstruct = struct
  (* AUDIT
   *
   * Error detection responsibilities
   * Terminologies and symbols :
   *   X =A, B, C=> Y : X delegates error checking responsibilities A, B, C to Y
   *   X := A, B, C   : X needs to handle responsibilities A, B, C
   *
   * Reconstruct methods
   *
   * `reconstruct`             =ALL=> `reconstruct_internal`
   * `reconstruct_data`        =ALL=> `reconstruct_internal`
   * `reconstruct_shards`      =ALL=> `reconstruct_shards_internal`
   * `reconstruct_data_shards` =ALL=> `reconstruct_shards_internal`
   * `reconstruct_shards_internal` :=
   *   - check length of `shards` matches total shard count exactly
   *   - check at least one option shard is not `None`
   *   - check consistency of length of individual option shards if exist
   * `reconstruct_internal` :=
   *   - check length of `slices` matches total shard count exactly
   *   - check consistency of length of individual slices
   *   - check length of `slice_present` matches length of `slices` *)

  module StringInput = struct
  end

  module ByteInput = struct
  end
end
