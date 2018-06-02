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

let eq (a : t) (b : t) : bool =
  let length_a = length a in

  length_a = length b
  &&
  begin
    match a with
    | `String s_a ->
      (match b with
       | `String s_b -> s_a = s_b
       | `Bytes  b_b ->
         let s_b = Bytes.unsafe_to_string b_b in
         s_a = s_b
      )
    | `Bytes  b_a ->
      (match b with
       | `String s_b ->
         let s_a = Bytes.unsafe_to_string b_a in
         s_a = s_b
       | `Bytes  b_b -> b_a = b_b
      )
  end
