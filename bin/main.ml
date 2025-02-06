(* open Ppxlib
open Ast_builder.Default

let totem ~ctxt ast =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  (* print_endline (Astlib.Pprintast.string_of_structure [ ast ]); *)
  [%str [%ocaml.error "Ops, enum2 must be a type with variant without args"]]

let generator () :
    ( Ppxlib__.Import.structure,
      rec_flag * type_declaration list )
    Deriving.Generator.t =
  Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt -> totem ~ctxt)

let _ = Deriving.add "Totem" ~str_type_decl:(generator ()) *)
let is_non_empty l = List.length l > 0 [@@deriving totem]
let () = print_endline (string_of_bool (is_non_empty []))

type int_fn = int -> int [@@deriving trace]

let double x = x * 2

let () =
  let traced_double = wrap_int_fn double in
  ignore (traced_double 5)
