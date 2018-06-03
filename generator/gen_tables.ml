open Printf

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
    result.%[log]                  <- i |> char_of_int;
    result.%[log + field_size - 1] <- i |> char_of_int;
  done;

  result

let multiply
    (log_table : bytes)
    (exp_table : bytes)
    (a : int)
    (b : int)
  : int =
  if a = 0 || b = 0 then
    0
  else (
    let log_a = int_of_char log_table.%[a] in
    let log_b = int_of_char log_table.%[b] in
    let log_result = log_a + log_b in
    exp_table.%[log_result] |> int_of_char
  )

let gen_mul_table
    (log_table : bytes)
    (exp_table : bytes)
  : bytes array =

  let result : bytes array = Array.make field_size (Bytes.make 0 '\x00') in

  for a = 0 to (field_size) - 1 do
    result.(a) <- Bytes.make field_size '\x00';
    for b = 0 to (field_size) - 1 do
      result.(a).%[b] <- multiply log_table exp_table a b |> char_of_int;
    done
  done;

  result

let gen_mul_table_half
    (log_table : bytes)
    (exp_table : bytes)
  : bytes array * bytes array =
  let half_table_row_count  = field_size in
  let half_table_col_count  = 16 in

  let low  : bytes array = Array.make half_table_row_count (Bytes.make 0 '\x00') in
  let high : bytes array = Array.make half_table_row_count (Bytes.make 0 '\x00') in

  for a = 0 to (half_table_row_count) - 1 do
    low .(a) <- Bytes.make half_table_col_count '\x00';
    high.(a) <- Bytes.make half_table_col_count '\x00';

    for b = 0 to (half_table_row_count) - 1 do
      let result : int ref = ref 0 in
      if not (a = 0 || b = 0) then (
        let log_a = int_of_char log_table.%[a] in
        let log_b = int_of_char log_table.%[b] in
        result := exp_table.%[log_a + log_b] |> int_of_char;
      );
      if b land 0x0F = b then (
        low .(a).%[b]       <- !result |> char_of_int;
      );
      if b land 0xF0 = b then (
        high.(a).%[b lsr 4] <- !result |> char_of_int;
      )
    done
  done;

  (low, high)

let print_tables_debug () : unit =
  let log_table = gen_log_table generating_polynomial in
  let exp_table = gen_exp_table log_table in
  let mul_table = gen_mul_table log_table exp_table in
  let (mul_table_low, mul_table_high) = gen_mul_table_half log_table exp_table in

  print_string "log table : [";
  for i = 0 to (Bytes.length log_table) - 1 do
    printf "%d, " (log_table.%[i] |> int_of_char);
  done;
  print_endline "]";

  print_string "exp table : [";
  for i = 0 to (Bytes.length exp_table) - 1 do
    printf "%d, " (exp_table.%[i] |> int_of_char);
  done;
  print_endline "]";

  print_string "mul table : [";
  for i = 0 to (Array.length mul_table) - 1 do
    print_string "[";
    for j = 0 to (Bytes.length mul_table.(i)) - 1 do
      printf "%d, " (mul_table.(i).%[j] |> int_of_char);
    done;
    print_endline "],";
  done;
  print_endline "]";

  print_string "mul table low : [";
  for i = 0 to (Array.length mul_table_low) - 1 do
    print_string "[";
    for j = 0 to (Bytes.length mul_table_low.(i)) - 1 do
      printf "%d, " (mul_table_low.(i).%[j] |> int_of_char);
    done;
    print_endline "],";
  done;
  print_endline "]";

  print_string "mul table high : [";
  for i = 0 to (Array.length mul_table_high) - 1 do
    print_string "[";
    for j = 0 to (Bytes.length mul_table_high.(i)) - 1 do
      printf "%d, " (mul_table_high.(i).%[j] |> int_of_char);
    done;
    print_endline "],";
  done;
  print_endline "]"

let write_1D_table (oc : out_channel) (name : string) (table : bytes) : unit =
  fprintf oc "let %s : string = \"" name;
  for i = 0 to (Bytes.length table) - 1 do
    fprintf oc "\\%03d" (table.%[i] |> int_of_char)
  done;
  fprintf oc "\"\n"

let write_2D_table (oc : out_channel) (name : string) (table : bytes array) : unit =
  let rows = Array.length table in
  let cols = Bytes.length table.(0) in

  fprintf oc "let %s : string array =[|" name;
  for i = 0 to rows - 1 do
    fprintf oc "\"";
    for j = 0 to cols - 1 do
      fprintf oc "\\%03d" (table.(i).%[j] |> int_of_char);
    done;
    fprintf oc "\";\n";
  done;
  fprintf oc "|]\n"

let main () =
  let log_table = gen_log_table generating_polynomial in
  let exp_table = gen_exp_table log_table in
  let mul_table = gen_mul_table log_table exp_table in
  let (mul_table_low, mul_table_high) = gen_mul_table_half log_table exp_table in

  let oc = open_out "tables.ml" in

  write_1D_table oc "log_table"      log_table;
  write_1D_table oc "exp_table"      exp_table;
  write_2D_table oc "mul_table"      mul_table;
  write_2D_table oc "mul_table_low"  mul_table_low;
  write_2D_table oc "mul_table_high" mul_table_high;

  close_out oc
;;

main ()
