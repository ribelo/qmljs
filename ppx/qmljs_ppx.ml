open Ppxlib

module B = Ast_builder.Default

let loc_errorf ~loc fmt = Location.raise_errorf ~loc fmt
let lident ~loc name = { loc; txt = Longident.Lident name }
let pvar ~loc name = B.ppat_var ~loc { loc; txt = name }
let evar ~loc name = B.evar ~loc name
let estring ~loc value = B.estring ~loc value
let eint ~loc value = B.eint ~loc value

let encode_name type_name =
  if String.equal type_name "t" then "qmljs_encode"
  else "qmljs_encode_" ^ type_name

let decode_name type_name =
  if String.equal type_name "t" then "qmljs_decode"
  else "qmljs_decode_" ^ type_name

let expression_error ~loc message =
  [%expr Error (Qmljs.Error.conversion [%e estring ~loc message])]

let rec longident_to_string = function
  | Longident.Lident name -> name
  | Ldot (parent, name) -> longident_to_string parent ^ "." ^ name
  | Lapply _ -> "<functor-application>"

let attr_payload_string attr =
  match attr.attr_payload with
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ }
              , _ )
        ; _
        }
      ] -> Some value
  | _ -> None

let qmljs_name attrs default =
  attrs
  |> List.find_map (fun attr ->
    if String.equal attr.attr_name.txt "qmljs.name" then attr_payload_string attr
    else None)
  |> Option.value ~default

let is_option_type typ =
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ _ ]) -> true
  | _ -> false

let rec encoder_expr typ ~context ~value =
  let loc = typ.ptyp_loc in
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "unit"; _ }, []) ->
      [%expr
        let () = [%e value] in
        Qmljs.Value.undefined [%e context]]
  | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
      [%expr Qmljs.Value.bool [%e context] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
      [%expr Qmljs.Value.int [%e context] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
      [%expr Qmljs.Value.float [%e context] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
      [%expr Qmljs.Value.string [%e context] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ element ]) ->
      [%expr
        Qmljs.Value.of_list
          [%e encoder_fun element]
          [%e context]
          [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "array"; _ }, [ element ]) ->
      [%expr
        Qmljs.Value.of_array
          [%e encoder_fun element]
          [%e context]
          [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ element ]) ->
      [%expr
        Qmljs.Value.of_option
          [%e encoder_fun element]
          [%e context]
          [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "result"; _ }, [ ok; error ]) ->
      [%expr
        Qmljs.Value.of_result
          ~ok:[%e encoder_fun ok]
          ~error:[%e encoder_fun error]
          [%e context]
          [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident name; _ }, []) ->
      [%expr [%e evar ~loc (encode_name name)] [%e context] [%e value]]
  | Ptyp_constr ({ txt; _ }, []) when String.equal (longident_to_string txt) "Qmljs.Value.t" ->
      value
  | Ptyp_tuple elements ->
      let elements = List.map snd elements in
      let encoded_elements =
        List.mapi
          (fun index element ->
            encoder_expr element ~context
              ~value:(evar ~loc ("item_" ^ string_of_int index)))
          elements
      in
      let pattern =
        B.ppat_tuple ~loc
          (List.mapi
             (fun index _ -> pvar ~loc ("item_" ^ string_of_int index))
             elements)
      in
      B.pexp_let ~loc Nonrecursive
        [ B.value_binding ~loc ~pat:pattern ~expr:value ]
        [%expr
          Qmljs.Array.of_array [%e context]
            [%e B.pexp_array ~loc encoded_elements]]
  | _ ->
      loc_errorf ~loc "unsupported qmljs encoder type"

and encoder_fun typ =
  let loc = typ.ptyp_loc in
  [%expr
    fun context value ->
      [%e encoder_expr typ ~context:(evar ~loc "context") ~value:(evar ~loc "value")]]

let rec decoder_expr typ ~value =
  let loc = typ.ptyp_loc in
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "unit"; _ }, []) ->
      [%expr
        match Qmljs.Value.kind [%e value] with
        | Qmljs.Value.Undefined | Qmljs.Value.Null -> Ok ()
        | _ -> [%e expression_error ~loc "expected unit"]]
  | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) ->
      [%expr Qmljs.Value.as_bool [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) ->
      [%expr Qmljs.Value.as_int [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "float"; _ }, []) ->
      [%expr Qmljs.Value.as_float [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) ->
      [%expr Qmljs.Value.as_string [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ element ]) ->
      [%expr Qmljs.Value.to_list [%e decoder_fun element] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "array"; _ }, [ element ]) ->
      [%expr Qmljs.Value.to_array [%e decoder_fun element] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ element ]) ->
      [%expr Qmljs.Value.to_option [%e decoder_fun element] [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident "result"; _ }, [ ok; error ]) ->
      [%expr
        Qmljs.Value.to_result
          ~ok:[%e decoder_fun ok]
          ~error:[%e decoder_fun error]
          [%e value]]
  | Ptyp_constr ({ txt = Longident.Lident name; _ }, []) ->
      [%expr [%e evar ~loc (decode_name name)] [%e value]]
  | Ptyp_constr ({ txt; _ }, []) when String.equal (longident_to_string txt) "Qmljs.Value.t" ->
      [%expr Ok [%e value]]
  | Ptyp_tuple elements ->
      decode_tuple ~loc (List.map snd elements) value
  | _ ->
      loc_errorf ~loc "unsupported qmljs decoder type"

and decoder_fun typ =
  let loc = typ.ptyp_loc in
  [%expr fun value -> [%e decoder_expr typ ~value:(evar ~loc "value")]]

and decode_tuple ~loc elements value =
  let bind index element cont =
    let item_name = "item_" ^ string_of_int index in
    let item = evar ~loc item_name in
    [%expr
      match [%e decoder_expr element ~value:item] with
      | Ok [%p pvar ~loc item_name] -> [%e cont]
      | Error error -> Error error]
  in
  let tuple =
    B.pexp_tuple ~loc
      (List.mapi (fun index _ -> evar ~loc ("item_" ^ string_of_int index)) elements)
  in
  let body =
    List.mapi (fun index element -> index, element) elements
    |> List.rev
    |> List.fold_left
         (fun cont (index, element) -> bind index element cont)
         [%expr Ok [%e tuple]]
  in
  [%expr
    match Qmljs.Value.to_array (fun value -> Ok value) [%e value] with
    | Error error -> Error error
    | Ok values ->
        if Stdlib.Array.length values <> [%e eint ~loc (List.length elements)]
        then [%e expression_error ~loc "tuple arity mismatch"]
        else
          [%e
            List.mapi
              (fun index _ ->
                B.value_binding ~loc
                  ~pat:(pvar ~loc ("item_" ^ string_of_int index))
                  ~expr:[%expr values.([%e eint ~loc index])])
              elements
            |> fun bindings ->
            B.pexp_let ~loc Nonrecursive bindings body]]

let result_bind ~loc expr name cont =
  [%expr
    match [%e expr] with
    | Ok [%p pvar ~loc name] -> [%e cont]
    | Error error -> Error error]

let record_field_expr ~loc value field =
  B.pexp_field ~loc value (lident ~loc field.pld_name.txt)

let field_decode_expr ~loc field object_value =
  let js_name = qmljs_name field.pld_attributes field.pld_name.txt in
  match is_option_type field.pld_type with
  | true ->
      [%expr
        match Qmljs.Object.get_string [%e object_value] [%e estring ~loc js_name] with
        | None -> Ok None
        | Some value -> [%e decoder_expr field.pld_type ~value:(evar ~loc "value")]]
  | false ->
      [%expr
        match Qmljs.Object.get_string [%e object_value] [%e estring ~loc js_name] with
        | None ->
            [%e
              expression_error ~loc
                ("missing field " ^ field.pld_name.txt)]
        | Some value -> [%e decoder_expr field.pld_type ~value:(evar ~loc "value")]]

let encode_record ~loc fields type_name =
  let context = evar ~loc "context" in
  let value = evar ~loc "value" in
  let sets =
    List.map
      (fun field ->
        let js_name = qmljs_name field.pld_attributes field.pld_name.txt in
        let encoded =
          encoder_expr field.pld_type ~context
            ~value:(record_field_expr ~loc value field)
        in
        [%expr
          Qmljs.Object.set_string object_
            [%e estring ~loc js_name]
            [%e encoded]])
      fields
  in
  let body =
    List.fold_right
      (fun set acc -> [%expr [%e set]; [%e acc]])
      sets
      [%expr object_]
  in
  let expr =
    [%expr
      fun context value ->
        let object_ = Qmljs.Object.create context in
        [%e body]]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(pvar ~loc (encode_name type_name))
        ~expr
    ]

let decode_record ~loc fields type_name =
  let final_record =
    B.pexp_record ~loc
      (List.map
         (fun field ->
           lident ~loc field.pld_name.txt, evar ~loc field.pld_name.txt)
         fields)
      None
  in
  let body =
    List.fold_right
      (fun field acc ->
        result_bind ~loc
          (field_decode_expr ~loc field (evar ~loc "value"))
          field.pld_name.txt acc)
      fields
      [%expr Ok [%e final_record]]
  in
  let expr =
    [%expr
      fun value ->
        match Qmljs.Value.kind value with
        | Qmljs.Value.Object -> [%e body]
        | _ -> [%e expression_error ~loc "expected object"]]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(pvar ~loc (decode_name type_name))
        ~expr
    ]

let constructor_pattern ~loc decl =
  match decl.pcd_args with
  | Pcstr_tuple [] -> B.ppat_construct ~loc (lident ~loc decl.pcd_name.txt) None
  | Pcstr_tuple args ->
      let args = List.map (fun arg -> arg.pca_type) args in
      let pattern =
        match args with
        | [ _ ] -> pvar ~loc "arg_0"
        | _ ->
            B.ppat_tuple ~loc
              (List.mapi
                 (fun index _ -> pvar ~loc ("arg_" ^ string_of_int index))
                 args)
      in
      B.ppat_construct ~loc (lident ~loc decl.pcd_name.txt) (Some pattern)
  | Pcstr_record _ ->
      loc_errorf ~loc:decl.pcd_loc "record variants are not supported by deriving qmljs"

let encode_constructor ~loc decl =
  let context = evar ~loc "context" in
  let args =
    match decl.pcd_args with
    | Pcstr_tuple args ->
        let args = List.map (fun arg -> arg.pca_type) args in
        List.mapi
          (fun index typ ->
            encoder_expr typ ~context
              ~value:(evar ~loc ("arg_" ^ string_of_int index)))
          args
    | Pcstr_record _ ->
        loc_errorf ~loc:decl.pcd_loc "record variants are not supported by deriving qmljs"
  in
  let args_array = B.pexp_array ~loc args in
  let tag = qmljs_name decl.pcd_attributes decl.pcd_name.txt in
  [%expr
    let object_ = Qmljs.Object.create context in
    Qmljs.Object.set_string object_ "tag" (Qmljs.Value.string context [%e estring ~loc tag]);
    Qmljs.Object.set_string object_ "args" (Qmljs.Array.of_array context [%e args_array]);
    object_]

let encode_variant ~loc constructors type_name =
  let cases =
    List.map
      (fun decl ->
        B.case ~lhs:(constructor_pattern ~loc decl)
          ~guard:None
          ~rhs:(encode_constructor ~loc decl))
      constructors
  in
  let expr =
    [%expr fun context value -> [%e B.pexp_match ~loc (evar ~loc "value") cases]]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(pvar ~loc (encode_name type_name))
        ~expr
    ]

let decode_constructor ~loc decl =
  let tag = qmljs_name decl.pcd_attributes decl.pcd_name.txt in
  match decl.pcd_args with
  | Pcstr_record _ ->
      loc_errorf ~loc:decl.pcd_loc "record variants are not supported by deriving qmljs"
  | Pcstr_tuple args ->
      let args = List.map (fun arg -> arg.pca_type) args in
      let decode_arg index typ cont =
        let item = "arg_" ^ string_of_int index in
        result_bind ~loc
          (decoder_expr typ ~value:[%expr values.([%e eint ~loc index])])
          item cont
      in
      let constructed =
        match args with
        | [] -> B.pexp_construct ~loc (lident ~loc decl.pcd_name.txt) None
        | [ _ ] ->
            B.pexp_construct ~loc (lident ~loc decl.pcd_name.txt)
              (Some (evar ~loc "arg_0"))
        | _ ->
            B.pexp_construct ~loc (lident ~loc decl.pcd_name.txt)
              (Some
                 (B.pexp_tuple ~loc
                    (List.mapi
                       (fun index _ -> evar ~loc ("arg_" ^ string_of_int index))
                       args)))
      in
      let body =
        List.mapi (fun index typ -> index, typ) args
        |> List.rev
        |> List.fold_left
             (fun cont (index, typ) -> decode_arg index typ cont)
             [%expr Ok [%e constructed]]
      in
      B.case
        ~lhs:(B.ppat_constant ~loc (Pconst_string (tag, loc, None)))
        ~guard:None
        ~rhs:
          [%expr
            match Qmljs.Object.get_string value "args" with
            | None -> [%e expression_error ~loc ("missing args for " ^ tag)]
            | Some args_value ->
                (match Qmljs.Value.to_array (fun value -> Ok value) args_value with
                 | Error error -> Error error
                 | Ok values ->
                     if Stdlib.Array.length values <> [%e eint ~loc (List.length args)]
                     then [%e expression_error ~loc ("arity mismatch for " ^ tag)]
                     else [%e body])]

let decode_variant ~loc constructors type_name =
  let cases =
    List.map (decode_constructor ~loc) constructors
    @ [ B.case
          ~lhs:(B.ppat_any ~loc)
          ~guard:None
          ~rhs:[%expr [%e expression_error ~loc "unknown variant tag"]]
      ]
  in
  let expr =
    [%expr
      fun value ->
        match Qmljs.Value.kind value with
        | Qmljs.Value.Object ->
            (match Qmljs.Object.get_string value "tag" with
             | None -> [%e expression_error ~loc "missing variant tag"]
             | Some tag_value ->
                 (match Qmljs.Value.as_string tag_value with
                  | Error error -> Error error
                  | Ok tag -> [%e B.pexp_match ~loc (evar ~loc "tag") cases]))
        | _ -> [%e expression_error ~loc "expected variant object"]]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(pvar ~loc (decode_name type_name))
        ~expr
    ]

let str_for_type_decl decl =
  let loc = decl.ptype_loc in
  if decl.ptype_params <> [] then
    loc_errorf ~loc "deriving qmljs does not support type parameters";
  let type_name = decl.ptype_name.txt in
  match decl.ptype_kind with
  | Ptype_record fields ->
      [ encode_record ~loc fields type_name; decode_record ~loc fields type_name ]
  | Ptype_variant constructors ->
      [ encode_variant ~loc constructors type_name
      ; decode_variant ~loc constructors type_name
      ]
  | Ptype_abstract ->
      (match decl.ptype_manifest with
       | Some typ ->
           let expr =
             [%expr
               fun context value ->
                 [%e
                   encoder_expr typ ~context:(evar ~loc "context")
                     ~value:(evar ~loc "value")]]
           in
           let decode =
             [%expr fun value -> [%e decoder_expr typ ~value:(evar ~loc "value")]]
           in
           [ B.pstr_value ~loc Nonrecursive
               [ B.value_binding ~loc
                   ~pat:(pvar ~loc (encode_name type_name)) ~expr
               ]
           ; B.pstr_value ~loc Nonrecursive
               [ B.value_binding ~loc
                   ~pat:(pvar ~loc (decode_name type_name)) ~expr:decode
               ]
           ]
       | None ->
           loc_errorf ~loc "deriving qmljs requires a manifest, record, or variant type")
  | Ptype_record_unboxed_product _ ->
      loc_errorf ~loc "deriving qmljs does not support unboxed record products"
  | Ptype_open ->
      loc_errorf ~loc "deriving qmljs does not support open types"

let type_expr decl =
  let loc = decl.ptype_loc in
  B.ptyp_constr ~loc (lident ~loc decl.ptype_name.txt) []

let sig_for_type_decl decl =
  let loc = decl.ptype_loc in
  let typ = type_expr decl in
  let encode_type =
    [%type: Qmljs.Context.t -> [%t typ] -> Qmljs.Value.t]
  in
  let decode_type =
    [%type: Qmljs.Value.t -> ([%t typ], Qmljs.Error.t) result]
  in
  [ B.psig_value ~loc
      (B.value_description ~loc
         ~name:{ loc; txt = encode_name decl.ptype_name.txt }
         ~type_:encode_type ~prim:[])
  ; B.psig_value ~loc
      (B.value_description ~loc
         ~name:{ loc; txt = decode_name decl.ptype_name.txt }
         ~type_:decode_type ~prim:[])
  ]

let derive_str ~ctxt:_ (_rec_flag, decls) =
  List.concat_map str_for_type_decl decls

let derive_sig ~ctxt:_ (_rec_flag, decls) =
  List.concat_map sig_for_type_decl decls

let qmljs_global_attr attrs =
  attrs
  |> List.find_map (fun attr ->
    match attr.attr_name.txt, attr_payload_string attr with
    | "qmljs.global", Some name -> Some (false, name)
    | "qmljs.global_async", Some name -> Some (true, name)
    | _ -> None)

let strip_global_attrs attrs =
  List.filter
    (fun attr ->
      not
        (String.equal attr.attr_name.txt "qmljs.global"
         || String.equal attr.attr_name.txt "qmljs.global_async"))
    attrs

let rec arrow_types typ =
  match typ.ptyp_desc with
  | Ptyp_arrow (Nolabel, arg, rest, _, _) ->
      let args, result = arrow_types rest in
      arg :: args, result
  | Ptyp_arrow _ ->
      loc_errorf ~loc:typ.ptyp_loc "qmljs global exports do not support labelled arguments"
  | _ -> [], typ

let binding_name binding =
  match binding.pvb_pat.ppat_desc with
  | Ppat_constraint
      ( { ppat_desc = Ppat_var { txt = name; _ }; _ }
      , Some ({ ptyp_desc = Ptyp_arrow _; _ } as typ)
      , _ ) -> Some (name, Some typ)
  | Ppat_var { txt = name; _ } -> Some (name, None)
  | _ ->
      None

let function_param_type param =
  match param.pparam_desc with
  | Pparam_val
      ( Nolabel
      , None
      , { ppat_desc = Ppat_constraint (_, Some typ, _); _ } ) -> typ
  | Pparam_val (Labelled _, _, _) | Pparam_val (Optional _, _, _) ->
      loc_errorf ~loc:param.pparam_loc
        "qmljs global exports do not support labelled arguments"
  | _ ->
      loc_errorf ~loc:param.pparam_loc
        "qmljs global exports require every argument to have a type annotation"

let expr_function_signature expr =
  match expr.pexp_desc with
  | Pexp_function
      ( params
      , { ret_type_constraint = Some (Pconstraint result); _ }
      , Pfunction_body _ ) ->
      Some (List.map function_param_type params, result)
  | Pexp_function _ ->
      loc_errorf ~loc:expr.pexp_loc
        "qmljs global exports require a return type annotation"
  | _ -> None

let binding_signature binding =
  match binding_name binding with
  | Some (name, Some typ) ->
      let args, result = arrow_types typ in
      name, args, result
  | Some (name, None) ->
      (match expr_function_signature binding.pvb_expr with
       | Some (args, result) -> name, args, result
       | None ->
           loc_errorf ~loc:binding.pvb_loc
             "qmljs global exports require a value binding type annotation")
  | None ->
      loc_errorf ~loc:binding.pvb_loc
        "qmljs global exports require a value binding type annotation"

let host_error_expr ~loc function_name argument message =
  [%expr
    Qmljs.Error.host
      ~message:[%e estring ~loc message]
      ~metadata:
        [ "function", [%e estring ~loc function_name]
        ; "argument", [%e estring ~loc argument]
        ]]

let decode_arg ~loc function_name index typ cont =
  let arg_name = "arg_" ^ string_of_int index in
  [%expr
    match [%e decoder_expr typ ~value:[%expr args.([%e eint ~loc index])]] with
    | Ok [%p pvar ~loc arg_name] -> [%e cont]
    | Error error ->
        raise
          [%e
            host_error_expr ~loc function_name (string_of_int index)
              ("invalid argument " ^ string_of_int index)]]

let register_expr ~loc ~async function_name js_name args result =
  let arity = List.length args in
  let call =
    let call_args =
      List.mapi (fun index _ -> evar ~loc ("arg_" ^ string_of_int index)) args
    in
    List.fold_left
      (fun fn arg -> [%expr [%e fn] [%e arg]])
      (evar ~loc function_name)
      call_args
  in
  let encoded = encoder_expr result ~context:(evar ~loc "context") ~value:call in
  let body =
    List.mapi (fun index typ -> index, typ) args
    |> List.rev
    |> List.fold_left
         (fun cont (index, typ) -> decode_arg ~loc js_name index typ cont)
         encoded
  in
  let setter =
    if async then [%expr Qmljs.Function.set_global_async]
    else [%expr Qmljs.Function.set_global]
  in
  [%expr
    fun context ->
      [%e setter] context ~name:[%e estring ~loc js_name]
        ~arity:[%e eint ~loc arity]
        (fun args ->
          if Stdlib.Array.length args <> [%e eint ~loc arity] then
            raise
              (Qmljs.Error.host
                 ~message:"wrong argument count"
                 ~metadata:
                   [ "function", [%e estring ~loc js_name]
                   ; "argument", "count"
                   ]);
          [%e body])]

let register_item binding =
  match qmljs_global_attr binding.pvb_attributes with
  | None -> None
  | Some (async, js_name) ->
      let loc = binding.pvb_loc in
      let function_name, args, result = binding_signature binding in
      let expr = register_expr ~loc ~async function_name js_name args result in
      Some
        ( function_name
        , B.pstr_value ~loc Nonrecursive
            [ B.value_binding ~loc
                ~pat:(pvar ~loc ("qmljs_register_" ^ function_name))
                ~expr
            ] )

let strip_binding binding =
  { binding with pvb_attributes = strip_global_attrs binding.pvb_attributes }

let aggregate_register_item ~loc names =
  let calls =
    List.map
      (fun name -> [%expr [%e evar ~loc ("qmljs_register_" ^ name)] context])
      names
  in
  let body =
    List.fold_right (fun call acc -> [%expr [%e call]; [%e acc]]) calls [%expr ()]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc
        ~pat:(pvar ~loc "qmljs_register")
        ~expr:[%expr fun context -> [%e body]]
    ]

let transform_structure structure =
  let registrations = ref [] in
  List.concat_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_value (rec_flag, bindings) ->
          let generated =
            bindings
            |> List.filter_map register_item
            |> List.map (fun (name, item) ->
              registrations := !registrations @ [ name ];
              item)
          in
          let stripped =
            { item with pstr_desc = Pstr_value (rec_flag, List.map strip_binding bindings) }
          in
          if generated = [] then [ stripped ]
          else stripped :: generated @ [ aggregate_register_item ~loc:item.pstr_loc !registrations ]
      | _ -> [ item ])
    structure

let () =
  Deriving.add "qmljs"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg derive_str)
    ~sig_type_decl:(Deriving.Generator.V2.make_noarg derive_sig)
  |> Deriving.ignore;
  Driver.register_transformation "qmljs_ppx" ~impl:transform_structure
