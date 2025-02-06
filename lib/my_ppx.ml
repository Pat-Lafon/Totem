open Ppxlib

let generate_wrapper ~loc type_decl =
  let type_name = type_decl.ptype_name.txt in
  match type_decl.ptype_manifest with
  | Some { ptyp_desc = Ptyp_arrow (arg_label, arg_type, ret_type); _ } ->
      let wrapper_name = "wrap_" ^ type_name in
      let open Ast_builder.Default in
      pstr_value ~loc Nonrecursive
        [value_binding ~loc
          ~pat:(ppat_var ~loc {txt = wrapper_name; loc})
          ~expr:(pexp_fun ~loc Nolabel None
                  (ppat_var ~loc {txt = "f"; loc})
                  (pexp_fun ~loc Nolabel None
                    (ppat_var ~loc {txt = "x"; loc})
                    (pexp_sequence ~loc
                      (pexp_apply ~loc
                        (evar ~loc "Printf.printf")
                        [(Nolabel, estring ~loc "Calling function with arg: %d\n");
                         (Nolabel, evar ~loc "x")])
                      (pexp_let ~loc Nonrecursive
                        [value_binding ~loc
                          ~pat:(ppat_var ~loc {txt = "result"; loc})
                          ~expr:(pexp_apply ~loc
                            (evar ~loc "f")
                            [(Nolabel, evar ~loc "x")])]
                        (pexp_sequence ~loc
                          (pexp_apply ~loc
                            (evar ~loc "Printf.printf")
                            [(Nolabel, estring ~loc "Result: %d\n");
                             (Nolabel, evar ~loc "result")])
                          (evar ~loc "result"))))))]
  | _ -> Location.raise_errorf ~loc "Expected function type"

let impl_generator ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (fun type_decl -> [generate_wrapper ~loc type_decl]) type_declarations

let deriver =
  Deriving.add "trace"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg impl_generator)
