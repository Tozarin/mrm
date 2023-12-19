(** Copyright 2023-2024, Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ppxlib
module List = ListLabels
open Ast_builder.Default

let count_of_modes : (string, int) Hashtbl.t = Hashtbl.create 10

let submodes name =
  let count = Hashtbl.find count_of_modes name in
  Array.make count name |> Array.to_list
  |> List.mapi ~f:(fun i name -> Printf.sprintf "'%s_mode_%d" name i)

let is_include_check (signature : signature) =
  let sig_check { psig_desc; psig_loc = _ } =
    match psig_desc with
    | Psig_include { pincl_mod; pincl_loc = _; pincl_attributes = _ } -> (
        match pincl_mod.pmty_desc with
        | Pmty_ident id ->
            let id = Longident.flatten_exn id.txt |> String.concat "." in
            List.mem id ~set:[ "Uuid"; "Mrm.Uuid"; "Mrm.Mrm.Uuid" ]
        | _ -> false)
    | _ -> false
  in
  List.fold_left
    ~f:(fun acc ssig -> acc || sig_check ssig)
    ~init:false signature

let common_types = [ "int"; "float"; "string"; "bool" ]

let get_types (ssig : signature) =
  let match_type = function
    | Ptyp_constr (id, []) ->
        let id = Longident.flatten_exn id.txt |> String.concat "." in
        if List.mem id ~set:common_types then Some id else None
    | Ptyp_package (id, []) ->
        let id = Longident.flatten_exn id.txt |> String.concat "." in
        Some id
    | _ -> None
  in
  let get_type ({ psig_desc; psig_loc = _ } : signature_item) =
    match psig_desc with
    | Psig_value
        {
          pval_name;
          pval_type;
          pval_prim = _;
          pval_attributes = _;
          pval_loc = _;
        } ->
        let t = match_type pval_type.ptyp_desc in
        Option.bind t (fun t -> Some (pval_name.txt, t))
    | _ -> None
  in
  List.map ~f:get_type ssig |> List.filter_map ~f:(fun e -> e)

let uuid_types : (string * string) list -> (string * string) list =
  List.filter ~f:(fun (_, t) -> List.mem t ~set:common_types |> not)

let convert_uuid : (string * string) list -> (string * string) list =
  List.map ~f:(fun (name, t) ->
      if List.mem t ~set:common_types |> not then (name, "int") else (name, t))

let type_convert = function
  | "int" -> "INT"
  | "float" -> "REAL"
  | "bool" -> "BOOLEAN"
  | "string" -> "VARCHAR(256)"
  | _ -> "INT" (* uuid to other tables *)

let impl =
  List.map ~f:(fun code -> Lexing.from_string code |> Parse.implementation)

let interface =
  List.map ~f:(fun code -> Lexing.from_string code |> Parse.interface)

module Codegen_q = struct
  let codegen_q_sig _ types loc =
    let name = { txt = Some "Q"; loc } in
    let types = convert_uuid types in
    let ts = List.map ~f:snd types in
    let open Printf in
    let get_new_uuid_sig_code =
      {| val get_new_uuid : (unit, int, [ `One ]) Caqti_request.t |}
    in

    let init_sig_code =
      {| val init : (unit, unit, [ `Zero ]) Caqti_request.t |}
    in

    let drop_sig_code =
      {| val drop : (unit, unit, [ `Zero ]) Caqti_request.t |}
    in

    let add_sig_code =
      sprintf {| val add : (%s, unit, [ `Zero ]) Caqti_request.t |}
        (ts |> String.concat "* ")
    in

    let delete_sig_code =
      {| val delete : (int, unit, [ `Zero ]) Caqti_request.t |}
    in

    let select_by_uuid_sig_code =
      sprintf {| val select_by_uuid : (int, (%s)), [ `One ]) Caqti_request.t|}
        (ts |> String.concat " * ")
    in

    let select_all_sig_code =
      sprintf
        {|
          val select_all : (unit, (%s), [ `Many | `One | `Zero ]) Caqti_request.t
        |}
        (ts |> String.concat " * ")
    in

    let update_by_uuid_sig_code =
      sprintf {| val update_by_uuid : (%s, unit, [ `Zero]) Caqti_request.t |}
        (String.concat "* " (List.tl ts @ [ List.hd ts ]))
    in

    let modsig =
      Pmty_signature
        (interface
           [
             get_new_uuid_sig_code;
             init_sig_code;
             drop_sig_code;
             add_sig_code;
             delete_sig_code;
             select_by_uuid_sig_code;
             select_all_sig_code;
             update_by_uuid_sig_code;
           ]
        |> List.flatten)
    in
    [
      module_declaration ~loc ~name
        ~type_:{ pmty_desc = modsig; pmty_loc = loc; pmty_attributes = [] }
      |> psig_module ~loc;
    ]

  let codegen_q_str modname types loc =
    let name = { txt = Some "Q"; loc } in
    let lowname = String.lowercase_ascii modname in
    let uuid_types = uuid_types types in
    let types = convert_uuid types in
    let names = List.map ~f:fst types in
    let ts = List.map ~f:snd types in
    let open Printf in
    let get_new_uuid_str_code =
      sprintf
        {|let get_new_uuid = 
            let open Caqti_type.Std in
            let open Caqti_request.Infix in
            (unit ->! int)
            "SELECT RANDOM()"
        |}
    in

    let init_str_code =
      sprintf
        {|let init = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (unit ->. unit) 
            "CREATE TABLE %s (
              %s
              %s
            )"
        |}
        lowname
        (List.map
           ~f:(fun (name, t) ->
             sprintf "%s %s NOT NULL,\n" name (type_convert t))
           types
        |> String.concat "")
        ("PRIMARY KEY (uuid)"
         :: List.map
              ~f:(fun (name, t) ->
                sprintf "FOREIGN KEY (%s) REFERENCES %s(uuid)" name
                  (String.lowercase_ascii t))
              uuid_types
        |> String.concat ",\n")
    in

    let drop_str_code =
      sprintf
        {|let drop =
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (unit ->. unit) "DROP TABLE %s" 
        |}
        lowname
    in

    let add_str_code =
      sprintf
        {|let add = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (%s ->. unit) 
            "INSERT INTO %s (%s) VALUES (%s)"
        |}
        (List.fold_right ~f:(sprintf "(t2 %s %s)")
           (List.filteri ~f:(fun i _ -> i != List.length ts - 1) ts)
           ~init:(List.length ts - 1 |> List.nth ts))
        lowname
        (names |> String.concat ", ")
        (List.map ~f:(fun _ -> "?") types |> String.concat ", ")
    in

    let delete_str_code =
      sprintf
        {|let delete = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (int ->. unit)
            "DELETE FROM %s WHERE uuid = ?"
        |}
        lowname
    in

    let select_by_uuid_str_code =
      sprintf
        {|let select_by_uuid = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (int ->! %s)
            "SELECT * FROM %s WHERE uuid = ?"
        |}
        lowname lowname
    in

    let select_all_str_code =
      sprintf
        {|let select_all = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (unit ->* %s)
            "SELECT * FROM %s"
        |}
        lowname lowname
    in

    let update_by_uuid_str_code =
      sprintf
        {|let update_by_uuid = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (%s ->. unit)
            "UPDATE %s SET %s WHERE uuid = ?"
        |}
        (List.fold_right ~f:(sprintf "(t2 %s %s)") (List.tl ts)
           ~init:(List.hd ts))
        lowname
        (List.tl types
        |> List.map ~f:(fun (name, _) -> sprintf "%s = ?" name)
        |> String.concat ", ")
    in

    let modstr =
      Pmod_structure
        (impl
           [
             get_new_uuid_str_code;
             init_str_code;
             drop_str_code;
             add_str_code;
             delete_str_code;
             select_by_uuid_str_code;
             select_all_str_code;
             update_by_uuid_str_code;
           ]
        |> List.flatten)
    in
    [
      module_binding ~loc ~name
        ~expr:{ pmod_desc = modstr; pmod_loc = loc; pmod_attributes = [] }
      |> pstr_module ~loc;
    ]
end

let codegen_db_sig_by_sig modname types loc =
  let name = { txt = Some (String.cat modname "_Db"); loc } in
  let open Printf in
  let uuid_types = uuid_types types in
  let all_modes =
    [ "'mainmodes" ] :: List.map ~f:(fun (_, t) -> submodes t) uuid_types
    |> List.flatten
  in
  let not_converted_types = types in
  let types = convert_uuid types in
  let ts = List.map ~f:snd types in
  let lowname = String.lowercase_ascii modname in

  (* type ('mainmods, 'submods1, 'submods2, ...) conns = ((module ModName, 'mainmods) conn * 'submods1 SubModule1.conns * 'submods2 SubModule2.conns * ...) *)
  let conns_type_sig_code =
    let bodycode =
      sprintf "((module %s), '%smodes) Mrm.Db.Connection.conn" modname lowname
      :: List.map
           ~f:(fun (_, t) ->
             let submodes = submodes t in
             sprintf "(%s) %s_Db.conns" (String.concat ", " submodes) t)
           uuid_types
      |> String.concat " * "
    in
    let phantomcode =
      [ sprintf "'%smodes" lowname ]
      :: List.map ~f:(fun (_, t) -> submodes t) uuid_types
      |> List.flatten |> String.concat ", "
    in
    sprintf "type (%s) conns = (%s)" phantomcode bodycode
  in

  let conns_with_mod_sig_code mods =
    mods :: List.tl all_modes |> String.concat ", " |> sprintf "(%s) conns"
  in

  let conns_with_single_mod_sig_code mods =
    let count_of_modes = Hashtbl.find count_of_modes modname in
    Array.make count_of_modes mods
    |> Array.to_list |> String.concat ", " |> sprintf "(%s) conns"
  in

  (* nths element of tuple *)
  let accessors_sig_code =
    List.map
      ~f:(fun (name, typ) ->
        Printf.sprintf "val %s_%s : (%s) -> %s" lowname name
          (ts |> String.concat " * ")
          typ)
      types
    |> String.concat "\n"
  in

  (* tuple creation *)
  let create_ojb_sig_code =
    sprintf "val _create_%s : %s -> (%s)" lowname
      (ts |> String.concat " ->")
      (ts |> String.concat " * ")
  in

  let connect_sig_code conname mode =
    sprintf
      {|val %s :
          %s -> %s
      |}
      conname
      ("(module Caqti_blocking.CONNECTION)"
       :: List.map
            ~f:(fun (_, t) ->
              let submodes = submodes t |> String.concat ", " in
              sprintf "((%s) %s_Db.conns)" submodes t)
            uuid_types
      |> String.concat " ->")
      (conns_with_mod_sig_code mode)
  in

  let connect_inited_sig_code =
    connect_sig_code "connect_inited" "[< `RW | `RO ]"
  in

  let connect_not_inited_sig_code =
    connect_sig_code "connect_not_inited" "[ `NOTINITED ]"
  in

  let get_new_uuid_sig_code =
    sprintf
      {|val get_new_uuid :
          %s -> ((int * %s), 'err) Result.t
      |}
      (conns_with_mod_sig_code "[< `RW | `RO ]")
      (conns_with_mod_sig_code "[< `RW | `RO ]")
  in

  let new_obj_sig_code =
    sprintf
      {|val new_%s :
          %s -> %s -> (((module %s) * %s), 'err) Result.t
      |}
      lowname
      (List.tl not_converted_types
      |> List.map ~f:(fun (_, t) ->
             if List.mem t ~set:common_types then t else sprintf "(module %s)" t)
      |> String.concat "-> ")
      (conns_with_mod_sig_code "[< `RW | `RO ]")
      modname
      (conns_with_mod_sig_code "[< `RW | `RO ]")
  in

  let init_sig_code =
    sprintf
      {|var init :
          %s -> (%s, 'err) Result.t
      |}
      (conns_with_mod_sig_code "[< `NOTINITED | `DROP ]")
      (conns_with_mod_sig_code "[ `RW ]")
  in

  let drop_sig_code =
    sprintf
      {|val drop :
          %s -> (%s, 'err) Result.t
      |}
      (conns_with_mod_sig_code "[ `RW ]")
      (conns_with_mod_sig_code "[ `DROP ]")
  in

  let add_deep_sig_code =
    sprintf
      {|val add_deep :
        (module %s) -> %s -> (%s, 'err) Result.t
      |}
      modname
      (conns_with_single_mod_sig_code "[ `RW ]")
      (conns_with_single_mod_sig_code "[ `RW ]")
  in

  let add_sig_code =
    sprintf
      {|val add :
        (module %s) -> %s -> (%s, 'err) Result.t
      |}
      modname
      (conns_with_single_mod_sig_code "[ `RW ]")
      (conns_with_single_mod_sig_code "[ `RW ]")
  in

  let delete_sig_code =
    sprintf
      {|val delete :
        (module %s) -> %s -> (%s, 'err) Result.t
      |}
      modname
      (conns_with_single_mod_sig_code "[ `RW ]")
      (conns_with_single_mod_sig_code "[ `RW ]")
  in

  let select_by_uuid_sig_code =
    sprintf
      {|val select_by_uuid :
          int ->
          %s -> ((module %s) * %s, 'err) Result.t
      |}
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
      modname
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
  in

  let select_all_sig_code =
    sprintf
      {|val select_all :
          %s -> ((module %s) list * %s, 'err) Result.t
      |}
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
      modname
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
  in

  let commit_sig_code =
    sprintf
      {|val commit :
             (module %s) list ->
             %s ->
             (%s, 'err) Result.t
      |}
      modname
      (conns_with_single_mod_sig_code "[ `RW ]")
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
  in

  let migrate_sig_code =
    sprintf "val migrate : %s -> %s -> (%s, 'err) Result.t"
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
      (conns_with_single_mod_sig_code "[ `RW ]")
      (conns_with_single_mod_sig_code "[< `RW | `RO ]")
  in

  let modsig =
    Pmty_signature
      (interface
         [ conns_type_sig_code; accessors_sig_code; create_ojb_sig_code ]
       @ [ Codegen_q.codegen_q_sig modname types loc ]
       @ interface
           [
             connect_inited_sig_code;
             connect_not_inited_sig_code;
             get_new_uuid_sig_code;
             new_obj_sig_code;
             init_sig_code;
             drop_sig_code;
             add_deep_sig_code;
             add_sig_code;
             delete_sig_code;
             select_by_uuid_sig_code;
             select_all_sig_code;
             commit_sig_code;
             migrate_sig_code;
           ]
      |> List.flatten)
  in
  [
    module_declaration ~loc ~name
      ~type_:{ pmty_desc = modsig; pmty_loc = loc; pmty_attributes = [] }
    |> psig_module ~loc;
  ]

let codegen_db_str_by_sig modname types loc =
  let name = { txt = Some (String.cat modname "_Db"); loc } in
  let open Printf in
  let names = List.map ~f:fst types in
  let uuid_types = uuid_types types in
  let all_modes =
    [ "'mainmodes" ] :: List.map ~f:(fun (_, t) -> submodes t) uuid_types
    |> List.flatten
  in
  let not_converted_types = types in
  let types = convert_uuid types in
  let normal_types =
    List.filter
      ~f:(fun (name, _) ->
        List.mem name ~set:(List.map ~f:fst uuid_types) |> not)
      types
  in
  let lowname = String.lowercase_ascii modname in

  let conns_type_str_code =
    let bodycode =
      sprintf "((module %s), '%smodes) Mrm.Db.Connection.conn" modname lowname
      :: List.map
           ~f:(fun (_, t) ->
             let submodes = submodes t in
             sprintf "(%s) %s_Db.conns" (String.concat ", " submodes) t)
           uuid_types
      |> String.concat " * "
    in
    let phantomcode =
      [ sprintf "'%smodes" lowname ]
      :: List.map ~f:(fun (_, t) -> submodes t) uuid_types
      |> List.flatten |> String.concat ", "
    in
    Hashtbl.add count_of_modes modname
      (1
      + List.fold_left
          ~f:(fun acc (_, t) -> acc + Hashtbl.find count_of_modes t)
          ~init:0 uuid_types);
    sprintf "type (%s) conns = (%s)" phantomcode bodycode
  in

  let conns_with_mod_sig_code mods =
    mods :: List.tl all_modes |> String.concat ", " |> sprintf "(%s) conns"
  in

  let conns_with_single_mod_sig_code mods =
    let count_of_modes = Hashtbl.find count_of_modes modname in
    Array.make count_of_modes mods
    |> Array.to_list |> String.concat ", " |> sprintf "(%s) conns"
  in

  let match_conns_code =
    sprintf
      {|let { Mrm.Db.Connection.conn = (module Conn : Caqti_blocking.CONNECTION); Mrm.Db.Connection.rows = _ } = %s in|}
      (if List.length uuid_types = 0 then "con" else "fst con")
  in

  let unpack_conn_code rows =
    sprintf {|{ conn = (module Conn : Caqti_blocking.CONNECTION); %s }|} rows
    :: List.map ~f:(fun (name, _) -> sprintf "%s_conn" name) uuid_types
    |> String.concat ", "
  in

  let unpack_conns_code_with_rows = unpack_conn_code "rows" in

  let unpack_conn_code_without_rows = unpack_conn_code "rows = _" in

  let accessors_str_code =
    List.map
      ~f:(fun (name, _) ->
        sprintf "let %s_%s (%s) = %s" lowname name
          (names |> String.concat ", ")
          name)
      types
    |> String.concat "\n"
  in

  let create_ojb_str_code =
    sprintf "let _create_%s %s = (%s)" lowname
      (names |> String.concat " ")
      (names |> String.concat ", ")
  in

  let obj_str_code =
    sprintf
      {|let %s = Caqti_type.Std.product _create_%s %s @@ Caqti_type.Std.proj_end|}
      lowname lowname
      (List.map
         ~f:(fun (name, t) ->
           Printf.sprintf "@@ Caqti_type.Std.proj Caqti_type.Std.%s %s_%s" t
             lowname name)
         types
      |> String.concat " ")
  in

  let match_res_str_code =
    {|let _match_res con = function
        | Ok _ -> Ok con
        | Error err -> Error err
    |}
  in

  let connect_str_code conname mode =
    let sigg =
      sprintf "%s -> %s"
        ("(module Caqti_blocking.CONNECTION)"
         :: List.map
              ~f:(fun (_, t) ->
                let submodes = submodes t |> String.concat ", " in
                sprintf "((%s) %s_Db.conns)" submodes t)
              uuid_types
        |> String.concat " ->")
        (conns_with_mod_sig_code mode)
    in
    sprintf
      {|let %s : %s =
         fun (module Conn : Caqti_blocking.CONNECTION) %s -> Random.self_init(); (%s)
      |}
      conname sigg
      (List.map ~f:fst uuid_types |> String.concat " ")
      ({|{ Mrm.Db.Connection.conn = (module Conn); Mrm.Db.Connection.rows = Mrm.Db.Zero }|}
       :: List.map ~f:fst uuid_types
      |> String.concat ", ")
  in

  let connect_inited_str_code =
    connect_str_code "connect_inited" "[< `RO | `RW ]"
  in

  let connect_not_inited_str_code =
    connect_str_code "connect_not_inited" "[ `NOTINITED ]"
  in

  let get_new_uuid_str_code =
    let sigg =
      sprintf "%s -> ((int * %s), 'err) Result.t"
        (conns_with_mod_sig_code "[< `RW | `RO ]")
        (conns_with_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let get_new_uuid : %s = 
         fun con ->
          let open Mrm.Db.Connection in
          let (%s) = con in
          Conn.find Q.get_new_uuid () |> function
            | Error err -> Error err
            | Ok uuid -> Ok (uuid, (%s))
      |}
      sigg unpack_conns_code_with_rows unpack_conns_code_with_rows
  in

  let new_obj_str_code =
    let sigg =
      sprintf "%s -> %s -> (((module %s) * %s), 'err) Result.t"
        (List.tl not_converted_types
        |> List.map ~f:(fun (_, t) ->
               if List.mem t ~set:common_types then t
               else sprintf "(module %s)" t)
        |> String.concat "-> ")
        (conns_with_mod_sig_code "[< `RW | `RO ]")
        modname
        (conns_with_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let new_%s : %s = 
         fun %s con ->
          let* (uuid, con) = get_new_uuid con in
          let new_obj = 
            (module struct
              %s
            end : %s) 
          in
          Ok (new_obj, con)
      |}
      lowname sigg
      (List.tl names |> String.concat " ")
      (List.map ~f:(fun name -> sprintf "let %s = %s" name name) names
      |> String.concat "\n")
      modname
  in

  let init_str_code =
    let sigg =
      sprintf "%s -> (%s, 'err) Result.t"
        (conns_with_mod_sig_code "[< `NOTINITED | `DROP ]")
        (conns_with_mod_sig_code "[ `RW ]")
    in
    sprintf
      {|let init : %s =
         fun con ->
          let open Mrm.Db.Connection in
          let (%s) = con in
          Conn.exec Q.init () |> _match_res (%s)
      |}
      sigg unpack_conns_code_with_rows unpack_conns_code_with_rows
  in

  let drop_str_code =
    let sigg =
      sprintf "%s -> (%s, 'err) Result.t"
        (conns_with_mod_sig_code "[ `RW ]")
        (conns_with_mod_sig_code "[ `DROP ]")
    in
    sprintf
      {|let drop : %s =
         fun con -> 
          let open Mrm.Db.Connection in
          let (%s) = con in
          Conn.exec Q.drop () |> _match_res (%s)
      |}
      sigg unpack_conns_code_with_rows unpack_conns_code_with_rows
  in

  let add_deep_str_code =
    let sigg =
      sprintf "(module %s) -> %s -> (%s, 'err) Result.t" modname
        (conns_with_single_mod_sig_code "[ `RW ]")
        (conns_with_single_mod_sig_code "[ `RW ]")
    in
    sprintf
      {|let add_deep : %s =
           fun (module M : %s) con ->
            let open Mrm.Db.Connection in
            let (%s) = con in
            %s
            %s
            Conn.exec Q.add (%s) |> _match_res (%s)
        |}
      sigg modname unpack_conns_code_with_rows
      (List.map
         ~f:(fun (name, t) ->
           sprintf
             {|let (module SubM) = M.%s in 
                let* %s_conn = %s_Db.add_deep (module SubM) %s_conn in 
                let %s = SubM.uuid in|}
             name name t name name)
         uuid_types
      |> String.concat "\n")
      (List.map
         ~f:(fun (name, _) -> sprintf "let %s = M.%s in" name name)
         normal_types
      |> String.concat "\n")
      (List.fold_right ~f:(sprintf "(%s, %s)")
         (List.filteri ~f:(fun i _ -> i != List.length names - 1) names)
         ~init:(List.length names - 1 |> List.nth names))
      unpack_conns_code_with_rows
  in

  let add_str_code =
    let sigg =
      sprintf "(module %s) -> %s -> (%s, 'err) Result.t" modname
        (conns_with_single_mod_sig_code "[ `RW ]")
        (conns_with_single_mod_sig_code "[ `RW ]")
    in
    sprintf
      {|let add : %s = 
         fun (module M : %s) con ->
          %s
          %s
          %s
          Conn.exec Q.add (%s) |> _match_res con
      |}
      sigg modname match_conns_code
      (List.map
         ~f:(fun (name, _) ->
           sprintf "let (module SubM) = M.%s in let %s = SubM.uuid in" name name)
         uuid_types
      |> String.concat "\n")
      (List.map
         ~f:(fun (name, _) -> sprintf "let %s = M.%s in" name name)
         normal_types
      |> String.concat "\n")
      (List.fold_right ~f:(sprintf "(%s, %s)")
         (List.filteri ~f:(fun i _ -> i != List.length names - 1) names)
         ~init:(List.length names - 1 |> List.nth names))
  in

  let delete_str_code =
    let sigg =
      sprintf "(module %s) -> %s -> (%s, 'err) Result.t" modname
        (conns_with_single_mod_sig_code "[ `RW ]")
        (conns_with_single_mod_sig_code "[ `RW ]")
    in
    sprintf
      {|let delete : %s =
         fun (module M : %s) con ->
          %s
          Conn.exec Q.delete M.uuid |> _match_res con
      |}
      sigg modname match_conns_code
  in

  let select_by_uuid_str_code =
    let sigg =
      sprintf "int -> %s -> ((module %s) * %s, 'err) Result.t"
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
        modname
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let select_by_uuid : %s =
         fun uuid con ->
          let open Mrm.Db.Connection in
          let (%s) = con in
          Conn.find Q.select_by_uuid uuid |> function
          | Error err -> Error err
          | Ok (%s) -> 
            let open Mrm.Db.Connection in 
            %s
            let m = (module struct %s end : %s) in
            Ok (m, con)
      |}
      sigg unpack_conn_code_without_rows
      (names |> String.concat ", ")
      (List.map
         ~f:(fun (name, t) ->
           sprintf "let* %s, %s_conn = %s_Db.select_by_uuid %s %s_conn in" name
             name t name name)
         uuid_types
      |> String.concat "\n")
      (List.map ~f:(fun name -> sprintf "let %s = %s" name name) names
      |> String.concat "\n")
      modname
  in

  let select_all_str_code =
    let sigg =
      sprintf "%s -> ((module %s) list * %s, 'err) Result.t"
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
        modname
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let select_all : %s =
         fun con ->
          let open Mrm.Db.Connection in
          let (%s) = con in
          Conn.collect_list Q.select_all () |> function
          | Ok es -> 
            let open Mrm.Db.Connection in
            let* es = List.fold_right
              (fun (%s) acc -> 
                let* acc = acc in
                %s 
                (module struct %s end : %s) :: acc |> return) 
              es (return [])
            in
            Ok (es, (%s))
          | Error err -> Error err
      |}
      sigg unpack_conn_code_without_rows
      (names |> String.concat ", ")
      (List.map
         ~f:(fun (name, t) ->
           sprintf "let* %s, %s_conn = %s_Db.select_by_uuid %s %s_conn in" name
             name t name name)
         uuid_types
      |> String.concat "\n")
      (List.map ~f:(fun name -> sprintf "let %s = %s" name name) names
      |> String.concat "\n")
      modname
      (unpack_conn_code "rows = Mrm.Db.to_rows es")
  in

  let commit_str_code =
    let sigg =
      sprintf "(module %s) list -> %s -> (%s, 'err) Result.t" modname
        (conns_with_single_mod_sig_code "[ `RW ]")
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let commit : %s =
         fun es con ->
          let open Mrm.Db.Connection in
          let (%s) = con in
          %s
          List.fold_left 
            (fun con (module M : %s) ->
              let* con = con in
              con
              |> fun (%s) ->
              %s
              %s
              Conn.exec Q.update_by_uuid (%s) |> _match_res (%s)) 
            (Mrm.return (%s)) es
      |}
      sigg unpack_conns_code_with_rows
      (List.map
         ~f:(fun (name, t) ->
           sprintf
             {|let submodes = List.map (fun (module M : %s) -> M.%s) es in
              let* %s_conn = %s_Db.commit submodes %s_conn in|}
             modname name name t name)
         uuid_types
      |> String.concat "\n")
      modname
      ("{ conn = (module Conn : Caqti_blocking.CONNECTION); rows = _ }"
       :: List.map ~f:(fun _ -> "_") uuid_types
      |> String.concat ", ")
      (List.map
         ~f:(fun (name, _) -> sprintf "let %s = M.%s in" name name)
         normal_types
      |> String.concat "\n")
      (List.map
         ~f:(fun (name, t) ->
           sprintf
             {|let (module SubM : %s) = M.%s in 
              let %s = SubM.uuid in|}
             t name name)
         uuid_types
      |> String.concat "\n")
      (List.fold_right ~f:(sprintf "(%s, %s)") (List.tl names)
         ~init:(List.hd names))
      (unpack_conn_code "rows = Mrm.Db.Zero")
      unpack_conns_code_with_rows
  in

  let migrate_str_code =
    let sigg =
      sprintf "%s -> %s -> (%s, 'err) Result.t"
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
        (conns_with_single_mod_sig_code "[ `RW ]")
        (conns_with_single_mod_sig_code "[< `RW | `RO ]")
    in
    sprintf
      {|let migrate : %s = 
         fun con_from con_to ->
          let* (es, _) = select_all con_from in 
          List.fold_left
            (fun con e ->
              let* con = con in
              add_deep e con
              )
            (Mrm.return con_to)
            es
      |}
      sigg
  in

  let modstr =
    Pmod_structure
      (impl
         [
           conns_type_str_code;
           accessors_str_code;
           create_ojb_str_code;
           obj_str_code;
         ]
       @ [ Codegen_q.codegen_q_str modname types loc ]
       @ impl
           [
             match_res_str_code;
             connect_inited_str_code;
             connect_not_inited_str_code;
             get_new_uuid_str_code;
             new_obj_str_code;
             init_str_code;
             drop_str_code;
             add_deep_str_code;
             add_str_code;
             delete_str_code;
             select_by_uuid_str_code;
             select_all_str_code;
             commit_str_code;
             migrate_str_code;
           ]
      |> List.flatten)
  in
  [
    module_binding ~loc ~name
      ~expr:{ pmod_desc = modstr; pmod_loc = loc; pmod_attributes = [] }
    |> pstr_module ~loc;
  ]

let codegen_db codegen_f extension_f
    ({ pmtd_name; pmtd_type; pmtd_attributes = _; pmtd_loc } :
      module_type_declaration) =
  let ext loc txt =
    [ extension_f ~loc:pmtd_loc (Location.error_extensionf ~loc txt) [] ]
  in
  match pmtd_type with
  | Some { pmty_desc = Pmty_signature ssig; pmty_loc; pmty_attributes = _ } ->
      if is_include_check ssig then
        let types = get_types ssig in
        if
          List.length types = 0
          || List.length types != List.length ssig - 1 (* Uuid include *)
        then
          ext pmty_loc
            {|Cannot derive database access module for first-class module with not primitive types to store|}
        else codegen_f pmtd_name.txt (("uuid", "int") :: types) pmty_loc
      else
        ext pmty_loc
          {|Cannot derive database access module for first-class module without included Uuid module|}
  | _ ->
      ext pmtd_loc
        "Cannot derive database access module for non first-call module"

let codegen_db_str ~ctxt:_ =
  codegen_db codegen_db_str_by_sig Ast_builder.Default.pstr_extension

let codegen_db_sig ~ctxt:_ =
  codegen_db codegen_db_sig_by_sig Ast_builder.Default.psig_extension

let _ =
  Deriving.add "mrm"
    ~str_module_type_decl:(Deriving.Generator.V2.make_noarg codegen_db_str)
    ~sig_module_type_decl:(Deriving.Generator.V2.make_noarg codegen_db_sig)
