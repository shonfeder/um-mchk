module type S = sig
  include Set.OrderedType

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end
