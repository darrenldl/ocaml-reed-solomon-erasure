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
  module String : sig
    val encode_single_sep : reed_solomon -> int -> string -> bytes array -> (unit, error) result
    (** [encode_single_sep r i_data single_data parity] *)

    val encode_single : reed_solomon -> int -> bytes array -> (unit, error) result

    val encode_sep : reed_solomon -> string array -> bytes array -> (unit, error) result

    val encode : reed_solomon -> bytes array -> (unit, error) result
  end

  module Bytes : sig
    val encode_single_sep : reed_solomon -> int -> bytes -> bytes array -> (unit, error) result
    (** [encode_single_sep r i_data single_data parity]
        This is a wrapper of {! String.encode_single_sep } *)

    val encode_single : reed_solomon -> int -> bytes array -> (unit, error) result

    val encode_sep : reed_solomon -> bytes array -> bytes array -> (unit, error) result

    val encode : reed_solomon -> bytes array -> (unit, error) result
  end
end

module Verify : sig
  module String : sig
  end

  module Bytes : sig
  end
end

module Reconstruct : sig
  module String : sig
  end

  module Bytes : sig
  end
end
