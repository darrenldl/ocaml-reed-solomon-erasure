type reed_solomon

type error = TooFewShards
           | TooManyShards
           | TooFewDataShards
           | TooManyDataShards
           | TooFewParityShards
           | TooManyParityShards
           | TooFewBufferShards
           | TooManyBufferShards
           | IncorrectShardSize
           | TooFewShardsPresent
           | EmptyShard
           | InvalidShardFlags
           | InvalidIndex

type sbs_error = TooManyCalls
               | LeftoverShards
               | RSError of error

module Encode : sig
  module StringInput : sig
    val encode_single_sep : reed_solomon -> int -> string -> bytes array -> (unit, error) result
    (** [encode_single_sep r i_data single_data parity] *)

    val encode_single : reed_solomon -> int -> bytes array -> (unit, error) result

    val encode_sep : reed_solomon -> string array -> bytes array -> (unit, error) result

    val encode : reed_solomon -> bytes array -> (unit, error) result
  end

  module ByteInput : sig
    val encode_single_sep : reed_solomon -> int -> bytes -> bytes array -> (unit, error) result
    (** [encode_single_sep r i_data single_data parity]
        This is a wrapper of {! StringInput.encode_single_sep } *)

    val encode_single : reed_solomon -> int -> bytes array -> (unit, error) result

    val encode_sep : reed_solomon -> bytes array -> bytes array -> (unit, error) result

    val encode : reed_solomon -> bytes array -> (unit, error) result
  end
end

module Verify : sig
  module StringInput : sig
  end

  module ByteInput : sig
  end
end

module Reconstruct : sig
  module StringInput : sig
  end

  module ByteInput : sig
  end
end
