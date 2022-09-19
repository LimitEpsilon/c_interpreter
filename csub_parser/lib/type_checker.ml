open Csub

type ext_ty = RVALUE of ty | LVALUE of ty | FUNCTION of ty * ty list

exception Double_declaration of id
exception Invalid_field of id
exception Not_in_scope of id
exception Invalid_array_size of id
exception Type_mismatch of (ext_ty * ext_ty)

type value = VAR_IN_SCOPE of ty | FUN_IN_SCOPE
type env = (id, value) Hashtbl.t

type struct_tbl = (id, env option) Hashtbl.t
(** global struct definition table. None when declared but not defined *)

type function_tbl = (id, ty * ty list) Hashtbl.t
(** global function definition table. *)

let function_tbl : function_tbl = Hashtbl.create 10
let env_stack : env list ref = ref []
let push_env tbl = env_stack := tbl :: !env_stack
let pop_env () = env_stack := List.tl !env_stack
let top_env () = List.hd !env_stack

exception Found_var of ty
exception Found_fun

let get_type x =
  let iter tbl =
    match Hashtbl.find tbl x with
    | exception _ -> ()
    | VAR_IN_SCOPE t -> raise (Found_var t)
    | FUN_IN_SCOPE -> raise Found_fun
  in
  match List.iter iter !env_stack with
  | exception Found_var t -> LVALUE t
  | exception Found_fun ->
    let t, l = Hashtbl.find function_tbl x in
    FUNCTION (t, l)
  | () -> raise (Not_in_scope x)

exception Found_str of env

(* let get_field_type str fld = *)
(*   match Hashtbl.find struct_tbl str with *)
(*   | exception _ -> raise (Not_in_scope str) *)
(*   | None -> raise (Not_in_scope fld) *)
(*   | Some e -> ( try Hashtbl.find e fld with _ -> raise (Invalid_field fld)) *)

(** puts decl in current env and returns the type of decl *)

(* let rec update_env decl = *)
(*   let env = top_env () in *)
(*   match decl with *)
(*   | DECL_VAR (t, x, arr_o) -> ( *)
(*     let make_ext_ty = *)
(*       match arr_o with *)
(*       | Some _ -> fun ty -> RVALUE (PTR ty) *)
(*       | None -> fun ty -> LVALUE ty *)
(*     in *)
(*     match t with *)
(*     | STRUCT_DECLARED (name_o, flds) -> *)
(*       let field_env = Hashtbl.create 4 in *)
(*       let name = *)
(*         match name_o with Some s -> s | None -> new_anonymous_struct () *)
(*       in *)
(*       push_env field_env; *)
(*       List.iter update_env flds; *)
(*       pop_env (); *)
(*       if Hashtbl.mem env (STRUCT name) then raise (Double_declaration name); *)
(*       Hashtbl.add env (STRUCT name) (STR_ENV field_env); *)
(*       if Hashtbl.mem env (VAR x) then raise (Double_declaration x); *)
(*       Hashtbl.add env (VAR x) (VAR_TY (make_ext_ty (STRUCT_NAMED name))) *)
(*     | t -> *)
(*       if Hashtbl.mem env (VAR x) then raise (Double_declaration x); *)
(*       Hashtbl.add env (VAR x) (VAR_TY (make_ext_ty t))) *)
(*   | DECL_FUN (ret, f, arg_decls) -> *)
(*     let ret_type = *)
(*       match ret with *)
(*       | STRUCT_DECLARED (name_o, flds) -> *)
(*         let field_env = Hashtbl.create 4 in *)
(*         let name = *)
(*           match name_o with Some s -> s | None -> new_anonymous_struct () *)
(*         in *)
(*         push_env field_env; *)
(*         List.iter update_env flds; *)
(*         pop_env (); *)
(*         if Hashtbl.mem env (STRUCT name) then raise (Double_declaration name); *)
(*         Hashtbl.add env (STRUCT name) (STR_ENV field_env); *)
(*         STRUCT_NAMED name *)
(*       | t -> t *)
(*     in *)
(*     let fun_env = Hashtbl.create 4 in *)
(*     push_env fun_env; *)
(*     List.iter (fun d -> update_env (DECL_VAR d)) arg_decls; *)
(*     pop_env (); *)
(*     let arg_types = *)
(*       List.map *)
(*         (fun (_, x, _) -> *)
(*           match Hashtbl.find fun_env (VAR x) with *)
(*           | VAR_TY t -> t *)
(*           | _ -> assert false) *)
(*         arg_decls *)
(*     in *)
(*     if Hashtbl.mem env (FUN f) then raise (Double_declaration f); *)
(*     Hashtbl.add env (FUN f) (FUN_TY (arg_types, ret_type, fun_env)) *)

(* let rec check_pgm (p : pgm) = *)
(*   let top_env = Hashtbl.create 8 in *)
(*   env_stack := top_env :: !env_stack; *)
(*   List.iter check_toplevel p *)

(* and check_toplevel (top : toplevel) = () *)
