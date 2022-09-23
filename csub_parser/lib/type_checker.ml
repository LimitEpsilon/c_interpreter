open Csub

type ext_ty = RVALUE of ty | LVALUE of ty | FUNCTION of (ty * ty list)
(** rvalue : both the address and the value is known *)

exception Double_declaration of id
exception Invalid_field of id
exception Not_in_scope of id
exception Invalid_array_size of id
exception Type_mismatch of ext_ty * ext_ty

exception
  Return_type_mismatch of ext_ty (* current return type is stored by ref *)

type value = VAR_IN_SCOPE of ty | FUN_IN_SCOPE
type env = (id, value) Hashtbl.t

let env_stack : env list ref = ref []
let push_env tbl = env_stack := tbl :: !env_stack
let pop_env () = env_stack := List.tl !env_stack
let top_env () = List.hd !env_stack
let current_return_ty : ty ref = ref VOID

exception Found_var of ty
exception Found_fun

let get_var_type x =
  let iter tbl =
    match Hashtbl.find tbl x with
    | exception _ -> ()
    | VAR_IN_SCOPE t -> raise (Found_var t)
    | FUN_IN_SCOPE -> raise Found_fun
  in
  match List.iter iter !env_stack with
  | exception Found_var t -> ( match t with ARR _ -> RVALUE t | _ -> LVALUE t)
  | exception Found_fun ->
    let t, l = Hashtbl.find function_tbl x in
    FUNCTION (t, l)
  | () -> raise (Not_in_scope x)

let get_field_type str fld =
  let flds = Hashtbl.find struct_tbl str in
  match Hashtbl.find flds fld with
  | VAR_FLD t -> ( match t with ARR _ -> RVALUE t | _ -> LVALUE t)
  | FUN_FLD -> FUNCTION (Hashtbl.find function_tbl fld)
  | exception _ -> raise (Invalid_field fld)

let update_env decl =
  let env = top_env () in
  match decl with
  | DECL_VAR (t, x) ->
    if Hashtbl.mem env x then raise (Double_declaration x)
    else Hashtbl.add env x (VAR_IN_SCOPE t)
  | DECL_FUN (_, f, _) ->
    if Hashtbl.mem env f then raise (Double_declaration f)
    else Hashtbl.add env f FUN_IN_SCOPE

let enter_scope () =
  let env = Hashtbl.create 5 in
  push_env env

let rec check_pgm pgm =
  let top_env = Hashtbl.create 13 in
  push_env top_env;
  (try List.iter check_top pgm
   with Type_mismatch ((LVALUE t1 | RVALUE t1), (LVALUE t2 | RVALUE t2)) ->
     print_endline "Type mismatch :";
     print_ty t1;
     print_newline ();
     print_ty t2;
     print_newline ());
  pgm

and check_top = function
  | TOP_DECL d -> update_env d
  | FUN ((t, f, decls), local) ->
    update_env (DECL_FUN (t, f, decls));
    current_return_ty := t;
    enter_scope ();
    List.iter (fun d -> update_env (DECL_VAR d)) decls;
    check_local local;
    pop_env ()

and check_local (decl_list, stmt_list) =
  List.iter update_env decl_list;
  List.iter check_stmt stmt_list

and check_stmt stmt =
  let check_stmt_o = function Some s -> check_stmt s | None -> () in
  let check_expr_o = function Some e -> check_expr e | None -> RVALUE INT in
  match stmt with
  | SCOPE local ->
    enter_scope ();
    check_local local;
    pop_env ()
  | EXPR expr -> check_expr expr |> ignore
  | RETURN expr_o -> (
    let ret_t =
      match expr_o with Some expr -> check_expr expr | None -> RVALUE VOID
    in
    match ret_t with
    | (LVALUE t | RVALUE t) as ext_t ->
      if t = !current_return_ty then () else raise (Return_type_mismatch ext_t)
    | _ as ext_t -> raise (Return_type_mismatch ext_t))
  | IF (expr_o, stmt, stmt_o) ->
    (match check_expr_o expr_o with
    | RVALUE INT | LVALUE INT -> ()
    | _ -> failwith "Predicate of if is not an integer!");
    check_stmt stmt;
    check_stmt_o stmt_o
  | WHILE (expr_o, stmt) ->
    (match check_expr_o expr_o with
    | RVALUE INT | LVALUE INT -> ()
    | _ -> failwith "Predicate of while is not an integer!");
    check_stmt stmt
  | FOR (init, check, update, stmt) ->
    check_expr_o init |> ignore;
    (match check_expr_o check with
    | RVALUE INT | LVALUE INT -> ()
    | _ -> failwith "Predicate of for is not an integer!");
    check_expr_o update |> ignore;
    check_stmt stmt
  | BREAK | CONTINUE | EMPTY -> ()

and check_expr expr =
  let () =
    if !debug then (
      print_expr expr;
      print_newline ())
  in
  match expr with
  | VAR x -> get_var_type x
  | CONST_INT _ -> RVALUE INT
  | CONST_CHAR _ -> RVALUE CHAR
  | STRING _ -> RVALUE (PTR CHAR)
  | ASSIGN (lhs, rhs) -> (
    let lhs_t = check_expr lhs in
    let rhs_t = check_expr rhs in
    match (lhs_t, rhs_t) with
    | LVALUE (PTR t1), RVALUE (ARR (t2, _)) | LVALUE t1, (RVALUE t2 | LVALUE t2)
      ->
      if t1 = t2 then lhs_t else raise (Type_mismatch (lhs_t, rhs_t))
    | _ -> raise (Type_mismatch (lhs_t, rhs_t)))
  | REF e -> (
    let t = check_expr e in
    match t with
    | LVALUE t -> RVALUE (PTR t)
    | RVALUE _ -> failwith "Can't ref an rvalue"
    | FUNCTION _ -> failwith "We don't support function pointers")
  | DEREF (base, addr) -> (
    let base_t = check_expr base in
    let addr_t = check_expr addr in
    match (base_t, addr_t) with
    | LVALUE (PTR t), (LVALUE INT | RVALUE INT) -> LVALUE t
    | RVALUE (ARR (t, _) | PTR t), (LVALUE INT | RVALUE INT) -> LVALUE t
    | _ -> failwith "Dereference error")
  | FIELD (str, fld) -> (
    let str_t = check_expr str in
    match str_t with
    | LVALUE (STRUCT x) -> get_field_type x fld
    | _ -> failwith "Tried to access field of non-struct expr!")
  | CALL (f, args) -> (
    let f_t = check_expr f in
    match f_t with
    | FUNCTION (ret_t, params_t) ->
      let for_each_arg arg =
        match check_expr arg with
        | RVALUE (ARR (t, _)) -> PTR t
        | LVALUE t | RVALUE t -> t
        | _ -> failwith "Cannot pass function as a function argument!"
      in
      let args_t = List.map for_each_arg args in
      if params_t = args_t then RVALUE ret_t
      else failwith "Function called with invalid types!"
    | _ -> failwith "Tried to call a non-function expression!")
  | ARITH_BOP (arithop, e1, e2) -> (
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
    match (t1, t2) with
    | (LVALUE _ | RVALUE _), (LVALUE _ | RVALUE _) ->
      check_arith_bop (arithop, t1, t2)
    | _ -> failwith "Tried to perform arithmetic operation with functions!")
  | REL_BOP (relop, e1, e2) -> (
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
    match (t1, t2) with
    | (LVALUE _ | RVALUE _), (LVALUE _ | RVALUE _) ->
      check_rel_bop (relop, t1, t2)
    | _ -> failwith "Tried to perform relational operation with functions!")
  | ARITH_UOP (arithop, e) -> (
    let t = check_expr e in
    match t with
    | LVALUE _ | RVALUE _ -> check_arith_uop (arithop, t)
    | _ -> failwith "Tried to perform unary arithmetic with a function!")
  | REL_UOP (relop, e) -> (
    let t = check_expr e in
    match t with
    | LVALUE _ | RVALUE _ -> check_rel_uop (relop, t)
    | _ -> failwith "Tried to perform unary relation op with a function!")

and check_arith_bop (op, t1, t2) =
  match op with
  | ADD | SUB -> (
    match (t1, t2) with
    | RVALUE (ARR (t, _)), (RVALUE INT | LVALUE INT) -> LVALUE (PTR t)
    | (RVALUE (PTR _) | LVALUE (PTR _)), (RVALUE INT | LVALUE INT) -> t1
    | (RVALUE INT | LVALUE INT), (RVALUE INT | LVALUE INT) -> RVALUE INT
    | _ -> failwith "ADD/SUB operands invalid!")
  | MUL | DIV | MOD | BOR | BAND -> (
    match (t1, t2) with
    | (RVALUE INT | LVALUE INT), (RVALUE INT | LVALUE INT) -> RVALUE INT
    | _ -> failwith "MUL/DIV/MOD/BOR/BAND operands invalid!")

and check_rel_bop (op, t1, t2) =
  match op with
  | GT | LT | GEQ | LEQ | EQ | NEQ -> (
    match (t1, t2) with
    | ( ( RVALUE (ARR (t1, _))
        | RVALUE (PTR t1)
        | LVALUE (ARR (t1, _))
        | LVALUE (PTR t1) ),
        ( RVALUE (ARR (t2, _))
        | RVALUE (PTR t2)
        | LVALUE (ARR (t2, _))
        | LVALUE (PTR t2) ) ) ->
      if t1 = t2 then RVALUE INT
      else failwith "Pointer comparison between different types!"
    | (RVALUE INT | LVALUE INT), (RVALUE INT | LVALUE INT) -> RVALUE INT
    | _ -> failwith "Comparison operands invalid!")
  | LOR | LAND -> (
    match (t1, t2) with
    | (RVALUE INT | LVALUE INT), (RVALUE INT | LVALUE INT) -> RVALUE INT
    | _ -> failwith "Boolean operands invalid!")

and check_arith_uop (op, t) =
  match op with
  | PRE_INCR | PRE_DECR | POST_INCR | POST_DECR -> (
    match t with
    | LVALUE (INT | PTR _ as t) -> RVALUE t
    | _ -> failwith "(Incr/Decr)-ementing a rvalue is not permitted!")
  | BNOT | NEG -> (
    match t with
    | LVALUE INT | RVALUE INT -> RVALUE INT
    | _ -> failwith "Unary arithmetic operand invalid!")

and check_rel_uop (op, t) =
  match op with
  | LNOT -> (
    match t with
    | LVALUE INT | RVALUE INT -> RVALUE INT
    | _ -> failwith "Unary logical operand invalid!")
