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

  Array.mem true
    (Array.mapi
       check_single
       buffer)
