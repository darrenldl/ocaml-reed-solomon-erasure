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

let code_single_slice
    (matrix_rows : bytes array)
    (i_input     : int)
    (input       : data)
    (outputs     : bytes array)
  : unit =
  let code_single_output matrix_rows input i_row output =
    let matrix_row_to_use = matrix_rows.(i_row).%(i_input) in

    if i_input = 0 then
      Galois.mul_slice    matrix_row_to_use input output
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
    (inputs      : data array)
    (outputs     : bytes array)
  : unit =
  for i_input = 0 to (r.data_shard_count) - 1 do
    code_single_slice matrix_rows i_input inputs.(i_input) outputs
  done

let check_some_slices_with_buffer
    (r           : reed_solomon)
    (matrix_rows : bytes array)
    (inputs      : data array)
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

type check_slices_op      = Single | Multi
type check_slice_index_op = All | Data | Parity
type check_piece_count_op = All | Data | Parity | ParityBuf

let check_slices
    (slices : data array)

let check_slice_index
    (op          : check_slice_index_op)
    (r           : reed_solomon)
    (i_data      : int)
    (single_data : data)
    (parity      : bytes array)
  : (unit, error) result =

module Encode = struct
end

module Verify = struct
end

module Reconstruct = struct
end
