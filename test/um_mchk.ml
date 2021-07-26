open Lib
open Bos_setup

module Chk = Checker.Make (Vending_machine.Spec)

(* let () = assert false *)
let () = Logs.(set_level (Some Debug))
let () = assert Chk.(check () |> terminates)
