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
      let pp_point (module P : Point.Point2D) =
        Printf.printf "id: %d\tx: %d\t y: %d\n" P.uuid P.x P.y
      in
      let ( >>= ) = Result.bind in
      let q =
        connect conn |> init
        >>= add (new_point2d 1 1)
        >>= add (new_point2d 1 2)
        >>= add (new_point2d 2 1)
        >>= add (new_point2d 1 1)
        >>= delete (new_point2d 2 1)
        >>= select_all
        >>= fun c ->
        List.iter pp_point (Point.Db.from_rows c.rows);
        print_endline "";
        Ok c >>= select_all >>= fun c ->
        Point.Db.Connection.rows c |> Point.Db.from_rows
        |> List.map (fun (module P : Point.Point2D) ->
               (module struct
                 let uuid = P.uuid
                 let x = 100
                 let y = 100
               end : Point.Point2D))
        |> fun es ->
        commit es c >>= select_all >>= fun c ->
        List.iter pp_point (Point.Db.from_rows c.rows);
        Ok c >>= drop
      in
      match q with
      | Ok _ -> ()
      | Error err -> Caqti_error.pp Format.std_formatter err)
