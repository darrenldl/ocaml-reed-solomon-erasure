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
    let result  = ref true in
    (match a with
     | `String s_a ->
       (match b with
        | `String s_b -> result := s_a = s_b
        | `Bytes  b_b ->
          let counter = ref 0 in
          while !counter < length_a && !result do
            if s_a.[!counter] <> b_b.%(!counter) then
              result := false
          done
       )
     | `Bytes  b_a ->
       (match b with
        | `String s_b ->
          let counter = ref 0 in
          while !counter < length_a && !result do
            if b_a.%(!counter) <> s_b.[!counter] then
              result := false
          done
        | `Bytes  b_b -> result := b_a = b_b
       )
    );
    !result
  end
