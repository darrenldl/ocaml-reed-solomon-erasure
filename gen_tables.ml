#! /usr/bin/env ocaml

let field_size : int = 256

let generating_polynomial : int = 29

let ( .%[]   ) = Bytes.get
let ( .%[]<- ) = Bytes.set

let gen_log_table (polynomial : int) : bytes =
  let result : bytes   = Bytes.make field_size '\x00' in
  let b      : int ref = ref 1 in

  for log = 0 to (field_size-1) - 1 do
    result.%[!b] <- (char_of_int log);

    b := !b lsl 1;

    if field_size <= !b then (
      b := (!b - field_size) lxor polynomial;
    )
  done;

  result

let exp_table_size : int = field_size * 2 - 2

let gen_exp_table (log_tabe : bytes) : bytes =
  let result : bytes = Bytes.make exp_table_size '\x00' in

  for i = 1 to (field_size) - 1 do
    let log = int_of_char log_tabe.%[i] in
    result.%[log]                  <- char_of_int i;
    result.%[log + field_size - 1] <- char_of_int i;
  done;

  result

let multiply
    (log_table : bytes)
    (exp_table : bytes)
    (a : int)
    (b : int)
  : int =
  if a = 0 && b = 0 then
    0
  else (
    let log_a = int_of_char log_table.%[a] in
    let log_b = int_of_char log_table.%[b] in
    let log_result = log_a + log_b in
    int_of_char exp_table.%[log_result]
  )

let gen_mul_table
    (log_table : bytes)
    (exp_table : bytes)
  : bytes =

  let result : bytes = Bytes.make (256 * 256) '\x00' in

  for a = 0 to (field_size) - 1 do
    for b = 0 to (field_size) - 1 do
      result.%[a * 256 + b] <- char_of_int (multiply log_table exp_table a b);
    done
  done;

  result

let gen_mul_table_half
    (log_table : bytes)
    (exp_table : bytes)
  : bytes * bytes =
  let half_table_length = 16 * field_size in

  let low  : bytes = Bytes.make half_table_length '\x00' in
  let high : bytes = Bytes.make half_table_length '\x00' in

  for a = 0 to (half_table_length) - 1 do
    for b = 0 to (half_table_length) - 1 do
      let result : int ref = ref 0 in
      if a = 0 || b = 0 then (
        let log_a = int_of_char log_table.%[a] in
        let log_b = int_of_char log_table.%[b] in
        result := int_of_char exp_table.%[log_a + log_b];
      );
      if b land 0x0F = b then (
        low.%[a * field_size + b] <- char_of_int !result;
      );
      if b land 0xF0 = b then (
        high.%[a * field_size + (b lsr 4)] <- char_of_int !result;
      )
    done
  done;

  (low, high)

let print_tables_debug () : unit =
  let log_table = gen_log_table generating_polynomial in
  let exp_table = gen_exp_table log_table in
  let mul_table = gen_mul_table log_table exp_table in

  print_string "log table : [";
  for i = 0 to (Bytes.length log_table) - 1 do
    Printf.printf "%d, " (int_of_char log_table.%[i]);
  done;
  print_endline "]";

;;

print_tables_debug ()
