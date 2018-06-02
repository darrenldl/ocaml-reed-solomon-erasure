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

let unsafe_to_string (data : t) : string =
  match data with
  | `Bytes  b -> Bytes.unsafe_to_string b
  | `String s -> s

let eq (a : t) (b : t) : bool =
  unsafe_to_string a = unsafe_to_string b
