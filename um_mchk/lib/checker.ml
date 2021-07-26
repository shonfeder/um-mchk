open Logs

module Make (Spec : Spec.S) = struct
  module State = Spec.State
  module Action = Spec.Action
  module StringSet = Set.Make (String)

  let state_to_string s = State.sexp_of_t s |> Sexplib.Sexp.to_string_hum

  let action_to_string a = Action.sexp_of_t a |> Sexplib.Sexp.to_string_hum

  module Explored = struct
    include Set.Make (State)

    let sexp_of_t s =
      to_seq s |> List.of_seq |> Sexplib.Conv.sexp_of_list State.sexp_of_t
  end

  module Exploring = struct
    include MoreLabels.Map.Make (State)

    type nonrec t = Action.Set.t t
  end

  let actions = Action.Set.of_list Spec.actions

  open Sexplib.Conv

  type step =
    { action : Action.t
    ; state : State.t
    }
  [@@deriving sexp_of]

  type path = step list [@@deriving sexp_of]

  type t =
    { explored : Explored.t
    ; exploring : (Exploring.t[@sexp.opaque])
    ; path : path
    ; current : State.t
    }
  [@@deriving sexp_of]

  let start =
    { explored = Explored.empty
    ; exploring = Exploring.empty
    ; path = []
    ; current = Spec.init
    }

  let to_string s = sexp_of_t s |> Sexplib.Sexp.to_string_hum

  let back_track : t -> t option =
   fun sts ->
    debug (fun l -> l "Backtracking");
    match sts.path with
    | []           -> None
    | step :: path -> Some { sts with path; current = step.state }

  let prev : t -> State.t option =
   fun { path; _ } ->
    match path with
    | []        -> None
    | step :: _ -> Some step.state

  let ( let* ) = Option.bind

  (* An action which has not be been explored for the current state *)
  let find_new_path : t -> (t * Action.t) option =
   fun sts ->
    let actions_tried =
      Exploring.find_opt sts.current sts.exploring
      |> Option.value ~default:Action.Set.empty
    in
    let* next_action = Action.Set.(diff actions actions_tried |> choose_opt) in
    let exploring =
      Exploring.add
        ~key:sts.current
        ~data:(Action.Set.add next_action actions_tried)
        sts.exploring
    in
    Some ({ sts with exploring }, next_action)

  let act : t -> Action.t -> t option =
   fun sts action ->
    let* current = Action.apply action sts.current in
    let path = { action; state = sts.current } :: sts.path in
    Some { sts with current; path }

  let some_if v cond =
    if cond then
      Some v
    else
      None

  let rec explore : t -> (t, _) Result.t =
   fun sts ->
    if Explored.mem sts.current sts.explored then
      Error `Explored
    else
      match find_new_path sts with
      | None               -> Error `No_path
      | Some (sts, action) -> (
          debug (fun l -> l "Applying action:\n%s" (Action.to_string action));
          match act sts action with
          | Some sts -> Ok sts
          | None     ->
              debug (fun l -> l "Action disabled");
              explore sts)

  type outcome =
    | Counter of t
    | Terminates of t

  let check_inv : t -> bool = fun sts -> Spec.invariant sts.current

  let rec check : t -> outcome =
   fun sts ->
    debug (fun l -> l "Exploring:\n%s" (to_string sts));
    match explore sts with
    | Ok sts ->
        if check_inv sts then (
          debug (fun l -> l "Invariant holds");
          check sts
        ) else (
          warn (fun l -> l "Counter example found:\n%s" (to_string sts));
          Counter sts
        )
    | Error `No_path
    | Error `Explored ->
    match back_track sts with
    | None     ->
        info (fun l -> l "Terminated:\n%s" (to_string sts));
        Terminates sts
    | Some sts -> check sts

  let check () = check start

  let terminates : outcome -> bool = function
    | Terminates _ -> true
    | Counter _    -> false
end
