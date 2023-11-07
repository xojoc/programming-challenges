open Core

type point = {x: int; y: int}

module PointAscending = struct
  type t = point

  include Comparator.Make (struct
    type t = point

    let compare x y = Int.compare x.x y.x

    let sexp_of_t = sexp_of_opaque
  end)
end

let channel_to_sequence ic =
  Sequence.unfold ~init:(In_channel.input_char ic) ~f:(fun s ->
      match s with
      | Some ch ->
          Some (ch, In_channel.input_char ic)
      | None ->
          None )

let string_to_sequence s =
  Sequence.unfold ~init:0 ~f:(fun i ->
      if i >= String.length s then None else Some (String.get s i, i + 1) )
