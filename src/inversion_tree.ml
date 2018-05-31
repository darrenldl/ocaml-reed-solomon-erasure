type tree = { mutable matrix   : Matrix.t option;
              children         : tree option array;
            }

type t = {  root         : tree;
            total_shards : int;
         }

let make (data_shards : int) (parity_shards : int) : t =
  { root         = { matrix   = Some (Matrix.identity data_shards);
                     children = Array.make (data_shards + parity_shards) None; };
    total_shards = data_shards + parity_shards; }

module Tree : sig
  val get_child : tree -> int -> int -> int -> tree

  val get_inverted_matrix : tree -> int list -> int -> int -> Matrix.t option

  val insert_inverted_matrix : tree -> Matrix.t -> int list -> int -> int -> unit
end = struct
  let make (matrix : Matrix.t option) (children_count : int) : tree =
    { matrix;
      children = Array.make children_count None }

  let get_child
      (tree            : tree)
      (offset          : int)
      (requested_index : int)
      (total_shards    : int )
    : tree =
    let node_index = requested_index - offset in

    match tree.children.(node_index) with
    | None -> (
        let child = make None (total_shards - offset) in
        tree.children.(node_index) <- Some child;
        child
      )
    | Some x -> x

  let rec get_inverted_matrix
      (tree            : tree)
      (invalid_indices : int list)
      (total_shards    : int)
      (offset          : int)
    : Matrix.t option =
    match invalid_indices with
    | [] -> tree.matrix
    | requested_index :: remaining_indices -> (
        let child = get_child tree offset requested_index total_shards in
        get_inverted_matrix
          child
          remaining_indices
          total_shards
          (requested_index + 1)
      )

  let rec insert_inverted_matrix
      (tree            : tree)
      (matrix          : Matrix.t)
      (invalid_indices : int list)
      (total_shards    : int)
      (offset          : int)
    : unit =
    match invalid_indices with
    | [] -> tree.matrix <- Some matrix
    | requested_index :: remaining_indices -> (
        let child = get_child tree offset requested_index total_shards in
        insert_inverted_matrix
          child
          matrix
          remaining_indices
          total_shards
          (requested_index + 1)
      )
end
