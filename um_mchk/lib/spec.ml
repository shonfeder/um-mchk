module type S = sig
  module State : State.S

  module Action : Action.S with type state = State.t

  val init : State.t

  val actions : Action.t list

  val invariant : State.t -> bool
end
