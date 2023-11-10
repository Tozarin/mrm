module type Point2D = sig
  val x : int
  val y : int
end

module type Point3D = sig
  val x : int
  val y : int
  val z : int
end

open Caqti_request.Infix
open Caqti_type.Std
open Result

let return x = Ok x
let error err = Error err
let ( >>= ) = bind
let ( let* ) = bind
let ( >>| ) x f = match x with Ok x -> f x |> return | Error err -> error err

module Db = struct
  type mode = [ `RO | `RW | `DR ]

  module Uuid = struct
    type 'db uuid = { uuid : int; m : 'db }

    let uuid { uuid; m = _ } = uuid
    let m { uuid = _; m } = m
    let uuid_m uuid m = { uuid; m }
  end

  type ('db, 'mode, 'err) t =
    | Point2D_Ok : ((module Point2D), 'mode, 'err) t
    | Point2D_Err : 'err -> ((module Point2D), 'mode, 'err) t
    | Point3D_Ok : ((module Point3D), 'mode, 'err) t
    | Point3D_Err : 'err -> ((module Point3D), 'mode, 'err) t

  module Connection = struct
    type 'db cache = Zero | Many of 'db Uuid.uuid list

    let zero = Zero
    let many us = Many us
    let uncache = function Zero -> [] | Many es -> es

    type ('db, 'mode, (*cache type*) 'err) conn = {
      conn : (module Caqti_blocking.CONNECTION);
      cache : 'db cache;
      db : ('db, 'mode, 'err) t;
    }

    let conn { conn; cache = _; db = _ } = conn
    let cache { conn = _; cache; db = _ } = cache
    let db { conn = _; cache = _; db } = db
    let connection conn cache db = { conn; cache; db }

    let ( >>> ) :
        (('db, [< `RO | `RW ], 'err) conn as 'f_conn) ->
        ('f_conn -> (('db, [> ], 'err) conn as 's_conn)) ->
        's_conn =
     fun conn f ->
      match db conn with Point2D_Err _ | Point3D_Err _ -> conn | _ -> f conn
  end
end

module Point2DDb = struct
  open Db
  open Connection

  (* module mb *)
  let create_point x y =
    (module struct
      let x = x
      let y = y
    end : Point2D)

  let point_x (module P : Point2D) = P.x
  let point_y (module P : Point2D) = P.y

  let same (module P1 : Point2D) (module P2 : Point2D) =
    P1.x = P2.x && P1.y = P2.y

  let point =
    let open Uuid in
    product (fun uuid x y -> create_point x y |> uuid_m uuid)
    @@ proj int (fun p -> p.uuid)
    @@ proj int (fun p -> point_x p.m)
    @@ proj int (fun p -> point_y p.m)
    @@ proj_end

  module Q = struct
    let init =
      (unit ->. unit)
      @@ {eos|
      CREATE TEMPORARY TABLE point2d (
        point2d_id int NOT NULL,
        x int NOT NULL,
        y int NOT NULL,
        PRIMARY KEY (point2d_id)
        )
    |eos}

    let drop = (unit ->. unit) @@ {|DROP TABLE point2d|}

    let add =
      (t3 int int int ->. unit)
        {|INSERT INTO point2d (point2d_id, x, y) VALUES (?, ?, ?)|}

    let delete =
      (t2 int int ->. unit) {|DELETE FROM point2d WHERE x = ? AND y = ?|}

    let update_x =
      (t2 int int ->. unit) {|UPDATE point2d SET x = ? WHERE point2d_id = ?|}

    let update_y =
      (t2 int int ->. unit) {|UPDATE point2d SET y = ? WHERE point2d_id = ?|}

    let update_several es =
      let open Uuid in
      let q =
        List.fold_left
          (fun q { uuid; m = (module P : Point2D) } ->
            Printf.sprintf
              "%s\nUPDATE point2d SET x = %d, y = %d WHERE point2d_id = %d;" q
              P.x P.y uuid)
          "" es
      in
      (unit ->. unit) q

    let select_all = (unit ->* point) {|SELECT * FROM point2d|}
  end

  let _match_res { conn; cache; db = _ } = function
    | Ok _ -> { conn; cache; db = Point2D_Ok }
    | Error err -> { conn; cache; db = Point2D_Err err }

  let connect :
      (module Caqti_blocking.CONNECTION) ->
      ((module Point2D), [< `RW | `RO ], 'err) conn =
   fun (module Conn) -> { conn = (module Conn); cache = Zero; db = Point2D_Ok }

  let init :
      ((module Point2D), [ `RW ], 'err) conn ->
      ((module Point2D), [ `RW ], 'err) conn =
   fun ({ conn = (module Conn); cache = _; db = _ } as conn) ->
    Conn.exec Q.init () |> _match_res conn

  let drop :
      ((module Point2D), [ `RW ], 'err) conn ->
      ((module Point2D), [ `DR ], 'err) conn =
   fun ({ conn = (module Conn); cache = _; db = _ } as conn) ->
    Conn.exec Q.drop () |> _match_res conn

  let add :
      (module Point2D) ->
      ((module Point2D), [ `RW ], 'err) conn ->
      ((module Point2D), [ `RW ], 'err) conn =
   fun (module P) ({ conn = (module Conn); cache = _; db = _ } as conn) ->
    Conn.exec Q.add (Random.bits () (*TODO*), P.x, P.y) |> _match_res conn

  let delete :
      (module Point2D) ->
      ((module Point2D), [ `RW ], 'err) conn ->
      ((module Point2D), [ `RW ], 'err) conn =
   fun (module P) ({ conn = (module Conn); cache = _; db = _ } as conn) ->
    Conn.exec Q.delete (P.x, P.y) |> _match_res conn

  let select_all :
      ((module Point2D), ([< `RW | `RO ] as 'mode), 'err) conn ->
      (module Point2D) Db.Uuid.uuid list * ((module Point2D), 'mode, 'err) conn
      =
   fun { conn = (module Conn); cache; db } ->
    Conn.collect_list Q.select_all () |> function
    | Ok es ->
        let cache = uncache cache in
        let cache =
          match
            List.filter (fun e -> not @@ List.mem e cache) es
            |> List.append cache
          with
          | [] -> Zero
          | es -> Many es
        in
        (es, Connection.connection (module Conn) cache db)
    | Error err -> ([], connection (module Conn) cache (Point2D_Err err))

  let commit :
      (module Point2D) Uuid.uuid list ->
      (((module Point2D), [ `RW ], 'err) conn as 'conn) ->
      'conn =
   fun es { conn = (module Conn); cache; db } ->
    let open Db.Uuid in
    let cache = uncache cache in
    let to_commit =
      List.filter
        (fun { uuid; m } ->
          List.exists (fun { uuid = id; m = e } -> uuid = id && same m e) cache
          |> not)
        es
    in
    Conn.exec (Q.update_several to_commit) () |> function
    | Ok _ -> connection (module Conn) Zero db
    | Error err -> connection (module Conn) Zero (Point2D_Err err)
end
