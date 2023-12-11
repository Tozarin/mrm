module type Foo = sig
  include Mrm.Uuid

  val x : int
  val z : string
end
[@@deriving mrm]

let () =
  match Mrm.connect () with
  | Error msg -> Mrm.pp_error msg
  | Ok conn -> (
      let open Foo_Db in
      let pp (module F : Foo) = Printf.printf "%d: %d %s\n" F.uuid F.x F.z in
      let ( >>= ) = Result.bind in
      let q =
        connect conn |> init
        >>= add (new_foo 1 "1")
        >>= add (new_foo 2 "2")
        >>= select_all
        >>= fun (fs, c) ->
        List.iter pp fs;
        drop c
      in
      match q with Ok _ -> () | Error err -> Mrm.pp_error err)
