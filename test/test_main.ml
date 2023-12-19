open Mrm

module type Foo = sig
  include Uuid

  val foo : string
end
[@@deriving mrm]

let connection =
  match connect_sqlite ~database:":memory:" with
  | Ok conn -> Some conn
  | Error _ -> None

let () =
  let open Foo_Db in
  match connection with
  | None -> print_endline "failed connection"
  | Some connection -> (
      ( connect_not_inited connection
      |> init
      >>= new_foo "Never gonna be let you down..."
      >>= fun (foo, con) -> add foo con >>= select_all )
      |> function
      | Ok (fs, _) -> List.iter (fun (module M : Foo) -> print_endline M.foo) fs
      | Error err -> pp_error err)
