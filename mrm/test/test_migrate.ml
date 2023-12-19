open Mrm

module type Foo = sig
  include Uuid

  val foo : string
end
[@@deriving mrm]

module type Bar = sig
  include Uuid

  val foo : (module Foo)
end
[@@deriving mrm]

let fst_conn =
  match connect_sqlite ~database:":memory:" with
  | Ok conn -> conn
  | Error _ -> failwith "connection failed"

let snd_conn =
  match connect_sqlite ~database:":memory:" with
  | Ok conn -> conn
  | Error _ -> failwith "connection failed"

let fill_q =
  let fs, foo_db =
    Foo_Db.connect_not_inited fst_conn |> Foo_Db.init |> function
    | Error _ -> failwith ""
    | Ok con -> (
        Array.make 10 "some data" |> Array.to_list
        |> List.map (fun data -> Foo_Db.new_foo data con)
        |> List.fold_left
             (fun acc e ->
               let* f, _ = e in
               let* acc = acc in
               Ok (f :: acc))
             (Ok [])
        |> function
        | Ok fs -> (fs, con)
        | Error _ -> failwith "fill fail")
  in
  let bar_db =
    Bar_Db.connect_not_inited fst_conn foo_db |> Bar_Db.init |> function
    | Error _ -> failwith ""
    | Ok con ->
        List.map
          (fun f ->
            Bar_Db.new_bar f con >>= fun (f, con) -> Bar_Db.add_deep f con)
          fs
        |> fun _ -> con
  in
  let snd_foo =
    Foo_Db.connect_not_inited snd_conn |> Foo_Db.init |> function
    | Error _ -> failwith ""
    | Ok con -> con
  in
  let snd_bar =
    Bar_Db.connect_not_inited snd_conn snd_foo |> Bar_Db.init |> function
    | Error _ -> failwith ""
    | Ok con -> con
  in
  Bar_Db.migrate bar_db snd_bar >>= Bar_Db.select_all |> function
  | Error err -> pp_error err
  | Ok (bs, _) ->
      List.iter
        (fun (module B : Bar) ->
          let (module F : Foo) = B.foo in
          print_endline F.foo)
        bs
