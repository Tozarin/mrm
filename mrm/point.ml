(* module type Point2D = sig
     val x : int
     val y : int
   end *)

module type Point3D = sig
  val x : int
  val y : int
  val z : int
end

(*********************************************************)
open Caqti_request.Infix
open Caqti_type.Std
open Result

let return x = Ok x
let error err = Error err
let ( >>= ) = bind
let ( let* ) = bind
let ( >>| ) x f = match x with Ok x -> f x |> return | Error err -> error err
(*********************************************************)

module Db = struct
  type mode = [ `RO | `RW | `DROP ]
  type 'db rows = Zero | One of 'db | Many of 'db list

  let to_rows = function [] -> Zero | [ e ] -> One e | es -> Many es
  let from_rows = function Zero -> [] | One e -> [ e ] | Many es -> es

  module Connection = struct
    type ('db, 'mode) conn = {
      conn : (module Caqti_blocking.CONNECTION);
      rows : 'db rows;
    }

    let conn { conn; rows = _ } = conn
    let rows { conn = _; rows } = rows
    let connection conn rows = { conn; rows }
  end
end

module type Uuid = sig
  val uuid : int
end

module type Point2D = sig
  include Uuid

  val x : int
  val y : int
end

module Point2DDb = struct
  open Db
  open Connection

  (* module mb *)
  let _create_point uuid x y =
    (module struct
      let uuid = uuid
      let x = x
      let y = y
    end : Point2D)

  let new_point2d x y =
    (module struct
      let uuid = Random.bits () (* ???? *)
      let x = x
      let y = y
    end : Point2D)

  let point2d_uuid (module P : Point2D) = P.uuid
  let point2d_x (module P : Point2D) = P.x
  let point2d_y (module P : Point2D) = P.y

  module type Point2D = sig
    include Uuid

    val x : int
    val y : int
  end

  let point =
    product _create_point @@ proj int point2d_uuid @@ proj int point2d_x
    @@ proj int point2d_y @@ proj_end

  module Q = struct
    let init =
      (unit ->. unit)
        {eos|
        CREATE TEMPORARY TABLE point2d (
          point2d_id int NOT NULL,
          x int NOT NULL,
          y int NOT NULL,
          PRIMARY KEY (point2d_id)
          )
      |eos}

    let drop = (unit ->. unit) {|DROP TABLE point2d|}

    let add =
      (t3 int int int ->. unit)
        {|INSERT INTO point2d (point2d_id, x, y) VALUES (?, ?, ?)|}

    let delete =
      (* add filters *)
      (int ->. unit) {|DELETE FROM point2d WHERE point2d_id = ?|}

    let select_all = (unit ->* point) {|SELECT * FROM point2d|}

    let update_by_uuid =
      (t3 int int int ->. unit)
        {|UPDATE point2d SET x = ?, y = ? WHERE point2d_id = ?|}

    let update_many es =
      (* make one big q *)
      let q =
        List.fold_left
          (fun q (module P : Point2D) ->
            Printf.sprintf
              "%s UPDATE point2d SET x = %d, y = %d WHERE point2d_id = %d;" q
              P.x P.y P.uuid)
          "" es
      in
      (unit ->. unit) q
  end

  let _match_res conn = function
    | Ok _ -> Ok conn
    | Error err -> Error err (* error conver mb *)

  let connect :
      (module Caqti_blocking.CONNECTION) ->
      ((module Point2D), [< `RW | `RO ]) conn =
   fun (module Conn) -> { conn = (module Conn); rows = Zero }

  let init :
      ((module Point2D), [< `RW | `DROP ]) conn ->
      (((module Point2D), [ `RW ]) conn, 'err) Result.t =
   fun ({ conn = (module Conn); rows = _ } as conn) ->
    Conn.exec Q.init () |> _match_res conn

  let drop :
      ((module Point2D), [ `RW ]) conn ->
      (((module Point2D), [ `DROP ]) conn, 'err) Result.t =
   fun ({ conn = (module Conn); rows = _ } as conn) ->
    Conn.exec Q.drop () |> _match_res conn

  let add :
      (module Point2D) ->
      ((module Point2D), [ `RW ]) conn ->
      (((module Point2D), [ `RW ]) conn, 'err) Result.t =
   fun (module P) ({ conn = (module Conn); rows = _ } as conn) ->
    Conn.exec Q.add (P.uuid, P.x, P.y) |> _match_res conn

  let delete :
      (* adds filters to dels not by point*)
      (module Point2D) ->
      ((module Point2D), [ `RW ]) conn ->
      (((module Point2D), [ `RW ]) conn, 'err) Result.t =
   fun (module P) ({ conn = (module Conn); rows = _ } as conn) ->
    Conn.exec Q.delete P.uuid |> _match_res conn

  let select_all :
      ((module Point2D), ([< `RW | `RO ] as 'mode)) conn ->
      (((module Point2D), 'mode) conn, 'err) Result.t =
   fun { conn = (module Conn); rows = _ } ->
    Conn.collect_list Q.select_all () |> function
    | Ok es -> Ok { conn = (module Conn); rows = to_rows es }
    | Error err -> Error err

  let commit :
      (module Point2D) list ->
      ((module Point2D), [ `RW ]) conn ->
      (((module Point2D), [< `RW | `RO ]) conn, 'err) Result.t =
   fun es ({ conn = (module Conn); rows } as conn) ->
    let old_rows = from_rows rows in
    let diff =
      List.filter
        (fun (module NewP : Point2D) ->
          List.exists
            (fun (module OldP : Point2D) ->
              NewP.uuid = OldP.uuid && (NewP.x <> OldP.x || NewP.y <> OldP.y))
            old_rows)
        es
    in
    List.fold_left
      (fun conn (module P : Point2D) ->
        conn
        >>= fun ({ conn = (module Conn : Caqti_blocking.CONNECTION); rows = _ }
                 as conn) ->
        Conn.exec Q.update_by_uuid (P.x, P.y, P.uuid) |> _match_res conn)
      (return conn) diff
end
