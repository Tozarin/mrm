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

module type Baz = sig
  include Uuid

  val bar : (module Bar)
end
[@@deriving mrm]

let connection =
  match connect_sqlite ~database:":memory:" with
  | Ok conn -> Some conn
  | Error _ -> None

let () =
  match connection with
  | None -> print_endline "failed connection"
  | Some connection ->
      let foo, foo_con =
        Foo_Db.connect_not_inited connection
        |> Foo_Db.init
        >>= Foo_Db.new_foo "Chained foo"
        |> function
        | Ok f -> f
        | Error _ -> failwith "foo"
      in
      let bar, bar_con =
        Bar_Db.connect_not_inited connection foo_con
        |> Bar_Db.init >>= Bar_Db.new_bar foo
        |> function
        | Ok f -> f
        | Error _ -> failwith "bar"
      in
      let (module B : Baz), baz_con =
        Baz_Db.connect_not_inited connection bar_con
        |> Baz_Db.init >>= Baz_Db.new_baz bar
        |> function
        | Ok f -> f
        | Error _ -> failwith "baz"
      in
      B.bar |> fun (module B : Bar) ->
      B.foo |> fun (module F : Foo) -> print_endline F.foo
