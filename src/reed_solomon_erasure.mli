open Errors

type reed_solomon

module Encode : sig
  module StringInput : sig
    val encode_single_sep : reed_solomon -> int -> string -> bytes array -> (unit, error) result
    (** [encode_single_sep r i_data single_data parity] *)
  end

  module ByteInput : sig
    val encode_single_sep : reed_solomon -> int -> bytes -> bytes array -> (unit, error) result
  end
end
