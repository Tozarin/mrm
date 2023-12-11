open Result

let return x = Ok x
let error err = Error err
let ( >>= ) = bind
let ( let* ) = bind
let ( >>| ) x f = match x with Ok x -> f x |> return | Error err -> error err

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

let get_uri () =
  let env_vars =
    let ( let* ) = Option.bind in
    let* pg_host = Sys.getenv_opt "PGHOST" in
    let* pg_port = Sys.getenv_opt "PGPORT" in
    let* pg_database = Sys.getenv_opt "PGDATABASE" in
    Some (pg_host, pg_port, pg_database)
  in
  match env_vars with
  | Some (pg_host, pg_port, pg_database) ->
      Printf.sprintf "postgresql://%s:%s/%s" pg_host pg_port pg_database
  | None -> "postgresql://"

let connect () =
  let uri = get_uri () in
  Caqti_blocking.connect (Uri.of_string uri)

let pp_error : Caqti_error.t -> unit = Caqti_error.pp Format.std_formatter
