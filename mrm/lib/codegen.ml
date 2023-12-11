open Ppxlib
module List = ListLabels
open Ast_builder.Default

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

let get_types (ssig : signature) =
  let match_type = function
    | Ptyp_constr (id, []) ->
        let id = Longident.flatten_exn id.txt |> String.concat "." in
        if List.mem id ~set:[ "int"; "float"; "string"; "char"; "bool" ] then
          Some id
        else None
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

let type_convert = function "string" -> "VARCHAR(256)" | t -> t

let impl =
  List.map ~f:(fun code -> Lexing.from_string code |> Parse.implementation)

let interface =
  List.map ~f:(fun code -> Lexing.from_string code |> Parse.interface)

module Codegen_q = struct
  let codegen_q_sig modname types loc =
    let name = { txt = Some "Q"; loc } in
    let ts = List.map ~f:snd types in
    let open Printf in
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
    let select_all_sig_code =
      sprintf
        {|
          val select_all : (unit, (module %s), [ `Many | `One | `Zero ]) Caqti_request.t
        |}
        modname
    in
    let update_by_uuid_sig_code =
      sprintf {| val update_by_uuid : (%s, unit, [ `Zero]) Caqti_request.t |}
        (String.concat "* " (List.tl ts @ [ List.hd ts ]))
    in
    let modsig =
      Pmty_signature
        (interface
           [
             init_sig_code;
             drop_sig_code;
             add_sig_code;
             delete_sig_code;
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
    let names = List.map ~f:fst types in
    let ts = List.map ~f:snd types in
    let open Printf in
    let init_str_code =
      sprintf
        {|let init = 
            let open Caqti_type.Std in 
            let open Caqti_request.Infix in 
            (unit ->. unit) "CREATE TEMPORARY TABLE %s (
              %s
              PRIMARY KEY (%s)
            )"
        |}
        lowname
        (List.map
           ~f:(fun (name, t) ->
             sprintf "%s %s NOT NULL,\n" name (type_convert t))
           types
        |> String.concat "")
        (List.hd types |> fst)
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
            (t%d %s ->. unit) 
            "INSERT INTO %s (%s) VALUES (%s)"
          |}
        (List.length types)
        (ts |> String.concat " ")
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
            "DELETE FROM %s WHERE %s_uuid = ?"
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
            (t%d %s ->. unit)
            "UPDATE %s SET %s WHERE uuid = ?"
        |}
        (List.length types)
        (String.concat " " (List.tl ts @ [ List.hd ts ]))
        lowname
        (List.tl types
        |> List.map ~f:(fun (name, _) -> sprintf "%s = ?" name)
        |> String.concat ", ")
    in
    let modstr =
      Pmod_structure
        (impl
           [
             init_str_code;
             drop_str_code;
             add_str_code;
             delete_str_code;
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
  let ts = List.map ~f:snd types in
  let lowname = String.lowercase_ascii modname in
  let accessors_sig_code =
    List.map
      ~f:(fun (name, typ) ->
        Printf.sprintf "val %s_%s : (module %s) -> %s" lowname name modname typ)
      types
    |> String.concat "\n"
  in
  let create_ojb_sig_code =
    sprintf "val _create_%s : %s -> (module %s)" lowname
      (ts |> String.concat " ->")
      modname
  in
  let new_obj_sig_code =
    sprintf "val new_%s : %s -> (module %s)" lowname
      (List.tl types |> List.map ~f:snd |> String.concat " ->")
      modname
  in
  let connect_sig_code =
    sprintf
      {|val connect : 
          (module Caqti_blocking.CONNECTION) ->
          ((module %s), [< `RW | `RO ]) Mrm.Db.Connection.conn
      |}
      modname
  in
  let init_sig_code =
    sprintf
      {|val init : 
          ((module %s), [< `RW | `DROP ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW] ) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname
  in
  let drop_sig_code =
    sprintf
      {|val drop : 
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `DROP ]) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname
  in
  let add_sig_code =
    sprintf
      {|val add : 
          (module %s) ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW ]) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname modname
  in
  let delete_sig_code =
    sprintf
      {|val delete :
          (module %s) ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW ]) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname modname
  in
  let select_all_sig_code =
    sprintf
      {|val code : 
          ((module %s), ([< `RW | `RO ] as 'mode)) Mrm.Db.Connection.conn ->
          ((module %s) list * ((module %s), 'mode) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname modname
  in
  let commit_sig_code =
    sprintf
      {|val commit : 
          (module %s) list ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [< `RW | `RO ]) Mrm.Db.Connection.conn, 'err) Result.t
      |}
      modname modname modname
  in
  let modsig =
    Pmty_signature
      (interface [ accessors_sig_code; create_ojb_sig_code; new_obj_sig_code ]
       @ [ Codegen_q.codegen_q_sig modname types loc ]
       @ interface
           [
             connect_sig_code;
             init_sig_code;
             drop_sig_code;
             add_sig_code;
             delete_sig_code;
             select_all_sig_code;
             commit_sig_code;
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
  let lowname = String.lowercase_ascii modname in
  let accessors_str_code =
    List.map
      ~f:(fun (name, _) ->
        sprintf "let %s_%s (module M : %s) = M.%s" lowname name modname name)
      types
    |> String.concat "\n"
  in
  let create_ojb_str_code =
    sprintf "let _create_%s %s = (module struct %s end : %s)" lowname
      (names |> String.concat " ")
      (List.map
         ~f:(fun (name, _) -> Printf.sprintf "let %s = %s" name name)
         types
      |> String.concat "\n")
      modname
  in
  let new_obj_str_code =
    sprintf
      "let new_%s %s = (module struct let uuid = Random.bits () %s end : %s)"
      (String.lowercase_ascii modname)
      (List.tl types |> List.map ~f:fst |> String.concat " ")
      (List.tl types
      |> List.map ~f:(fun (name, _) -> Printf.sprintf "let %s = %s" name name)
      |> String.concat "\n")
      modname
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
    {|let _match_res conn = function
        | Ok _ -> Ok conn
        | Error err -> Error err
    |}
  in
  let connect_str_code =
    sprintf
      {|let connect :
          (module Caqti_blocking.CONNECTION) ->
          ((module %s), [< `RW | `RO ]) Mrm.Db.Connection.conn = 
         fun (module Conn) -> Random.self_init(); { conn = (module Conn); rows = Mrm.Db.Zero}
      |}
      modname
  in
  let init_str_code =
    sprintf
      {|let init :
          ((module %s), [< `RW | `DROP ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW ]) Mrm.Db.Connection.conn, 'err) Result.t = 
         fun ({ conn = (module Conn); rows = _ } as conn) ->
          Conn.exec Q.init () |> _match_res conn
      |}
      modname modname
  in
  let drop_str_code =
    sprintf
      {|let drop : 
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `DROP ]) Mrm.Db.Connection.conn, 'err) Result.t = 
         fun ({ conn = (module Conn); rows = _ } as conn) -> 
          Conn.exec Q.drop () |> _match_res conn
      |}
      modname modname
  in
  let add_str_code =
    sprintf
      {|let add:
          (module %s) ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW ]) Mrm.Db.Connection.conn, 'err) Result.t =
         fun (module M) ({ conn = (module Conn); rows = _ } as conn) ->
          Conn.exec Q.add (%s) |> _match_res conn
      |}
      modname modname modname
      (List.map ~f:(fun (name, _) -> String.cat "M." name) types
      |> String.concat ", ")
  in
  let delete_str_code =
    sprintf
      {|let delete :
          (module %s) ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [ `RW ]) Mrm.Db.Connection.conn, 'err) Result.t =
         fun (module M) ({ conn = (module Conn); rows = _ } as conn) ->
          Conn.exec Q.delete M.uuid |> _match_res conn
      |}
      modname modname modname
  in
  let select_all_str_code =
    sprintf
      {|let select_all:
          ((module %s), ([< `RW | `RO ] as 'mode)) Mrm.Db.Connection.conn ->
          ((module %s) list * ((module %s), 'mode) Mrm.Db.Connection.conn, 'err) Result.t = 
         fun { conn = (module Conn); rows = _ } ->
          Conn.collect_list Q.select_all () |> function
          | Ok es -> let open Mrm.Db.Connection in Ok (es, { conn = (module Conn); rows = Mrm.Db.to_rows es })
          | Error err -> Error err
      |}
      modname modname modname
  in
  let commit_str_code =
    sprintf
      {|let commit : 
          (module %s) list ->
          ((module %s), [ `RW ]) Mrm.Db.Connection.conn ->
          (((module %s), [< `RW | `RO ]) Mrm.Db.Connection.conn, 'err) Result.t =
         fun es ({ conn = (module Conn); rows } as conn) ->
            List.fold_left
            (fun con (module M : %s) ->
              let open Mrm.Db.Connection in
              let (>>=) = Result.bind in
              con
              >>= fun ({ conn = (module Conn : Caqti_blocking.CONNECTION); rows = _ }
                       as conn) ->
              Conn.exec Q.update_by_uuid (%s) |> _match_res conn)
            (Mrm.return conn) es
      |}
      modname modname modname modname
      ( List.map ~f:(fun (name, _) -> String.cat "M." name) types |> fun l ->
        Printf.sprintf "%s, %s" (List.tl l |> String.concat ", ") (List.hd l) )
  in
  let modstr =
    Pmod_structure
      (impl
         [
           accessors_str_code;
           create_ojb_str_code;
           new_obj_str_code;
           obj_str_code;
         ]
       @ [ Codegen_q.codegen_q_str modname types loc ]
       @ impl
           [
             match_res_str_code;
             connect_str_code;
             init_str_code;
             drop_str_code;
             add_str_code;
             delete_str_code;
             select_all_str_code;
             commit_str_code;
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
        if List.length types = 0 then
          ext pmty_loc
            {|Cannot derive database access module for first-class module without primitive types to store|}
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
