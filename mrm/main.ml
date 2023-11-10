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

let () =
  match connect () with
  | Error msg -> Caqti_error.pp Format.std_formatter msg
  | Ok conn -> (
      let open Point.Point2DDb in
      let open Point.Db.Uuid in
      let pp_point { uuid; m = (module P : Point.Point2D) } =
        Printf.printf "id: %d\tx: %d\t y: %d\n" uuid P.x P.y
      in
      let q =
        connect conn |> init
        |> add (create_point 1 1)
        |> add (create_point 1 2)
        |> add (create_point 2 1)
        |> add (create_point 1 1)
        |> delete (create_point 2 1)
        |> select_all
        |> fun (ps, conn) ->
        List.iter pp_point ps;
        conn
        |> commit
             (List.mapi
                (fun i { uuid; m = _ } -> { uuid; m = create_point i i })
                ps)
        |> select_all
        |> fun (ps, conn) ->
        List.iter pp_point ps;
        conn |> drop
      in
      let open Point.Db in
      match q.db with
      | Point2D_Ok | Point3D_Ok -> ()
      | Point2D_Err err | Point3D_Err err ->
          Caqti_error.pp Format.std_formatter err)
