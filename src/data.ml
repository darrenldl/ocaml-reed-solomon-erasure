open Ops

type t = [ `String of string | `Bytes of bytes ]

let length (data : t) : int =
  match data with
  | `String x -> String.length x
  | `Bytes  x -> Bytes.length  x

let get (data : t) (i : int) : char =
  match data with
  | `String x -> x.[i]
  | `Bytes  x -> x.%(i)

let ( .%{} ) = get
