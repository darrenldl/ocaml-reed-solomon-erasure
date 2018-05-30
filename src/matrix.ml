open Ops

type error = SingularMatrix

type matrix = {
  row_count : int;
  col_count : int;
  data      : bytes;
}

let ( .%{}   ) = fun m (x,y)   -> m.data.%[x * m.col_count + y]
let ( .%{}<- ) = fun m (x,y) v -> m.data.%[x * m.col_count + y] <- v

let calc_row_start_end (m : matrix) (col_count : int) (row : int) : int * int =
  let start = row * m.col_count in
  let e     = start + m.col_count in

  (start, e)

let make (rows : int) (cols : int) : matrix =
  let data = Bytes.make (rows * cols) '\x00' in

  { row_count = rows;
    col_count = cols;
    data              }

let make_with_data (init_data : bytes array) : matrix =
  let rows = Array.length init_data in
  let cols = Bytes.length init_data.(0) in

  for r = 0 to (rows) - 1 do
    if Bytes.length init_data.(r) <> cols then
      failwith "Inconsistent row sizes"
  done;

  let data = Bytes.make (rows * cols) '\x00' in

  for r = 0 to (rows) - 1 do
    for c = 0 to (cols) - 1 do
      data.%[r * cols + c] <- init_data.(r).%[c]
    done
  done;

  { row_count = rows;
    col_count = cols;
    data              }

let identity (size : int) : matrix =
  let result = make size size in

  for i = 0 to (size) - 1 do
    result.%{i,i} <- 1 |> char_of_int;
  done;

  result
