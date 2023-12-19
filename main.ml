open Caqti_request.Infix
open Caqti_type.Std
open Result
open Mrm

module type Foo = sig
  include Mrm.Uuid

  val foo : string
end
[@@deriving mrm]

module type Bar = sig
  include Mrm.Uuid

  val some : int
  val foo : (module Foo)
end
[@@deriving mrm]

module type Point2D = sig
  include Mrm.Uuid

  val x : int
  val y : int
  val some_module : (module Bar)
end
[@@deriving mrm]

let foo =
  (module struct
    let uuid = 1
    let foo = "123"
  end : Foo)

let bar =
  (module struct
    let uuid = 42
    let some = 42
    let foo = foo
  end : Bar)

let point =
  (module struct
    let uuid = 1
    let x = 1
    let y = 2
    let some_module = bar
  end : Point2D)

let () =
  match connect_sqlite ~database:"testing" with
  | Error err -> pp_error err
  | Ok conn -> (
      let foo_conn =
        Foo_Db.connect conn |> Foo_Db.init |> function
        | Ok conn -> conn
        | _ -> failwith "foo"
      in
      let bar_conn =
        Bar_Db.connect conn foo_conn |> Bar_Db.init |> function
        | Ok conn -> conn
        | _ -> failwith "bar"
      in
      let point_conn =
        Point2D_Db.connect conn bar_conn
        |> Point2D_Db.init >>= Point2D_Db.add_deep point
      in
      match point_conn with Ok _ -> () | Error err -> pp_error err)
