type error = AlreadySet
           | NotSquare

type t

val make : int -> int -> t

val get_inverted_matrix : t -> int list -> Matrix.t option

val insert_inverted_matrix : t -> int list -> Matrix.t -> (unit, error) result
