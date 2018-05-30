open Ops

type error = SingularMatrix

type matrix = {
  row_count : int;
  col_count : int;
  data      : bytes array;
}

let ( .%{}   ) = fun m (x,y)   -> m.(x).%[y]
let ( .%{}<- ) = fun m (x,y) v -> m.(x).%[y] <- v

let ( .&{}   ) = fun m (x,y)   -> m.data.%{x,y}
let ( .&{}<- ) = fun m (x,y) v -> m.data.%{x,y} <- v

let make_bytes_array (rows : int) (cols : int) : bytes array =
  let data = Array.make rows (Bytes.empty) in
  Array.map (fun _ -> Bytes.make cols '\x00') data

let make (rows : int) (cols : int) : matrix =
  let data = make_bytes_array rows cols in

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

  let data = make_bytes_array rows cols in

  for r = 0 to (rows) - 1 do
    for c = 0 to (cols) - 1 do
      data.%{r,c} <- init_data.(r).%[c]
    done
  done;

  { row_count = rows;
    col_count = cols;
    data              }

let identity (size : int) : matrix =
  let result = make size size in

  for i = 0 to (size) - 1 do
    result.&{i,i} <- 1 |> char_of_int;
  done;

  result

let col_count (m : matrix) : int =
  m.col_count

let row_count (m : matrix) : int =
  m.row_count

let get (m : matrix) (r : int) (c : int) : char =
  m.&{r,c}

let set (m : matrix) (r : int) (c : int) (v : char) : unit =
  m.&{r,c} <- v

let multiply (lhs : matrix) (rhs : matrix) : matrix =
  if lhs.col_count <> rhs.col_count then
    failwith (Printf.sprintf "Colomn count on left is different from row count on right, lhs : %d, rhs : %d" lhs.col_count rhs.col_count);

  let result = make lhs.row_count rhs.col_count in

  for r = 0 to (lhs.row_count) - 1 do
    for c = 0 to (rhs.col_count) - 1 do
      let v = ref 0 in
      for i = 0 to (lhs.col_count) - 1 do
        v := (Galois.mul lhs.&{r,i} rhs.&{i,c} |> int_of_char) lxor !v;
      done
    done
  done;

  result

let augment (lhs : matrix) (rhs : matrix) : matrix =
  if lhs.row_count <> rhs.row_count then
    failwith (Printf.sprintf "Matrices do not have the same row count, lhs : %d, rhs : %d" lhs.row_count rhs.row_count);

  let result = make lhs.row_count (lhs.col_count + rhs.col_count) in

  for r = 0 to (lhs.row_count) - 1 do
    for c = 0 to (lhs.col_count) - 1 do
      result.&{r,c} <- lhs.&{r,c};
    done;
    let lhs_col_count = lhs.col_count in
    for c = 0 to (rhs.col_count) - 1 do
      result.&{r,lhs_col_count + c} <- rhs.&{r,c};
    done
  done;

  result

let sub_matrix
    (m    : matrix)
    (rmin : int)
    (cmin : int)
    (rmax : int)
    (cmax : int)
  : matrix =
  let result = make (rmax - rmin) (cmax - cmin) in

  for r = rmin to (rmax) - 1 do
    for c = cmin to (cmax) - 1 do
      result.&{r - rmin, c - cmin} <- m.&{r,c};
    done
  done;

  result

let get_row (m : matrix) (row : int) : bytes =
  m.data.(row)

let swap_rows (m : matrix) (r1 : int) (r2 : int) : unit =
  if r1 = r2 then
    ()
  else (
    let row1 = m.data.(r1) in
    let row2 = m.data.(r2) in

    m.data.(r1) <- row2;
    m.data.(r2) <- row1
  )
