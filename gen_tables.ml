#! /usr/bin/env ocaml

let field_size : int = 256

let generating_polynomial : int = 29

let gen_log_table (polynomial : int) : bytes =
  let result : bytes   = Bytes.make field_size '\x00' in
  let b      : int ref = ref 1 in

  for log = 0 to field_size-2 do
    Bytes.set result !b (Char.chr log);

    b := !b lsl 1;

    if field_size <= !b then (
      b := (!b - field_size) land polynomial;
    )
  done;

  result

