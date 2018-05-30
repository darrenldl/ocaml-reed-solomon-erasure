type matrix

type error = SingularMatrix

val ( .%{}   ) : bytes array -> int * int -> char
val ( .%{}<- ) : bytes array -> int * int -> char -> unit

val ( .&{}   ) : matrix -> int * int -> char
val ( .&{}<- ) : matrix -> int * int -> char -> unit

val make_bytes_array : int -> int -> bytes array

val make : int -> int -> matrix

val make_with_data : bytes array -> matrix

val identity : int -> matrix

val col_count : matrix -> int

val row_count : matrix -> int

val get : matrix -> int -> int -> char

val set : matrix -> int -> int -> char -> unit

val multiply : matrix -> matrix -> matrix

val augment : matrix -> matrix -> matrix

val sub_matrix : matrix -> int -> int -> int -> int -> matrix

val get_row : matrix -> int -> bytes

val swap_rows : matrix -> int -> int -> unit

val is_square : matrix -> bool

val gaussian_elim : matrix -> (unit, error) result

val invert : matrix -> (matrix, error) result

val vandermonde : int -> int -> matrix
