open Tables
open Ops
open Data_type

let add (a : char) (b : char) : char =
  ((int_of_char a) lxor (int_of_char b)) |> char_of_int

let sub (a : char) (b : char) : char =
  ((int_of_char a) lxor (int_of_char b)) |> char_of_int

let mul (a : char) (b : char) : char =
  mul_table.(int_of_char a).%(int_of_char b)

let div (a : char) (b : char) : char =
  if      (int_of_char a) = 0 then
    char_of_int 0
  else if (int_of_char b) = 0 then
    raise (Failure "Divisor is 0")
  else (
    let log_a = log_table.%(a |> int_of_char) |> int_of_char in
    let log_b = log_table.%(b |> int_of_char) |> int_of_char in
    let log_result = ref (log_a - log_b) in
    if !log_result < 0 then
      log_result := !log_result + 255;
    exp_table.%(!log_result)
  )

let exp (a : char) (n : int) : char =
  if      n               = 0 then
    1 |> char_of_int
  else if (int_of_char a) = 0 then
    0 |> char_of_int
  else (
    let log_a = log_table.%(a |> int_of_char) |> int_of_char in
    let log_result = ref (log_a * n) in
    while 255 <= !log_result do
      log_result := !log_result - 255;
    done;
    exp_table.%(!log_result)
  )

let pure_ocaml_unroll = 4

(* let mul_slice_pure_ocaml (c : char) (input : bytes)  *)
