module Spec : Spec.S = struct
  module State = struct
    open Base

    type drink =
      | Soda
      | Beer
    [@@deriving compare, equal, sexp]

    type kind =
      | Pay
      | Soda
      | Select
      | Beer
    [@@deriving compare, equal, sexp]

    type t =
      { kind : kind
      ; paid : bool
      ; drink : drink option
      }
    [@@deriving compare, equal, sexp]
  end

  module Action = Action.Make (State)

  open State

  let actions =
    let open Action in
    [ def
        "insert_coin"
        ({ kind = Pay; paid = false; drink = None }
        --> { kind = Select; paid = true; drink = None })
    ; def
        "pick_soda"
        ({ kind = Select; paid = true; drink = None }
        --> { kind = Soda; paid = true; drink = Some Soda })
    ; def
        "pick_beer"
        ({ kind = Select; paid = true; drink = None }
        --> { kind = Beer; paid = true; drink = Some Beer })
    ; def
        "get_soda"
        ({ kind = Soda; paid = true; drink = Some Soda }
        --> { kind = Pay; paid = false; drink = None })
    ; def
        "get_beer"
        ({ kind = Soda; paid = true; drink = Some Beer }
        --> { kind = Pay; paid = false; drink = None })
    ]

  let init = { kind = Pay; paid = false; drink = None }

  let invariant { paid; drink; _ } = (not (Option.is_some drink)) || paid
end
