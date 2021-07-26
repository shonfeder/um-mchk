module type S = sig
  type t [@@deriving sexp]

  type state

  type transition = state -> state option

  val to_string : t -> string

  val apply : t -> transition

  val name : t -> string

  val def : string -> transition -> t

  (** (s --> s') is a transition from [s] to [`s].

      It is desabled for any state [t <> s]. *)
  val (-->) : state -> state -> transition

  module Set : Set.S with type elt = t
end

module Make (St : State.S) : S with type state = St.t = struct
  open Sexplib.Std

  type state = St.t

  type transition = St.t -> St.t option

  module T = struct
    type t =
      { name : string
      ; act : (transition[@sexp.opaque])
      }
    [@@deriving sexp]

    let compare a b = String.compare a.name b.name
  end

  module Set = Set.Make (T)
  include T

  let to_string s = sexp_of_t s |> Sexplib.Sexp.to_string_hum

  let apply : t -> transition = fun { act; _ } s -> act s

  let name : t -> string = fun { name; _ } -> name

  let def name act = { name; act }

  let ( --> ) s s' st =
    if Int.equal (St.compare s st) 0  then
      Some s'
    else
      None
end
