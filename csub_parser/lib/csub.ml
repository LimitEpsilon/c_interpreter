type pgm = toplevel list
and toplevel = TOP_DECL of decl | FUN of fun_decl * local
and local = decl list * stmt list
and decl = DECL_VAR of var_decl | DECL_FUN of fun_decl
and struct_tbl = (id, decl list) Hashtbl.t
and var_decl = ty * id
and fun_decl = ty * id * var_decl list
and ty = INT | CHAR | VOID | STRUCT of id | PTR of ty | ARR of ty * int

and stmt =
  | SCOPE of local
  | EXPR of expr
  | RETURN of expr option
  | IF of expr option * stmt * stmt option
  | WHILE of expr option * stmt
  | FOR of expr option * expr option * expr option * stmt
  | BREAK
  | CONTINUE
  | EMPTY (* just a semicolon *)  (** statements change control flow *)

and expr =
  | VAR of id
  | CONST_INT of int
  | CONST_CHAR of char
  | STRING of string
  | ASSIGN of expr * expr
  | REF of expr
  | DEREF of expr * expr (* *x is translated to x[0] *)
  | FIELD of expr * id
  | CALL of expr * expr list
  | ARITH_BOP of bop_arith * expr * expr
  | REL_BOP of bop_rel * expr * expr
  | ARITH_UOP of uop_arith * expr
  | REL_UOP of uop_rel * expr

and bop_arith = ADD | SUB | MUL | DIV | MOD | BOR | BAND (* bitwise *)
and bop_rel = GT | LT | GEQ | LEQ | LOR | LAND | EQ | NEQ
and uop_arith = PRE_INCR | POST_INCR | PRE_DECR | POST_DECR | BNOT | NEG
and uop_rel = LNOT
and id = string

let struct_tbl : struct_tbl = Hashtbl.create 31

let insert_struct id decls =
  if Hashtbl.mem struct_tbl id then
    failwith ("Structure " ^ id ^ " is already defined, cannot redefine.")
  else Hashtbl.add struct_tbl id decls

let rec eval_const_integer = function
  | VAR _ | STRING _ | ASSIGN _ | REF _ | DEREF _ | FIELD _ | CALL _ ->
    failwith "Array index is not a constant integer expression"
  | CONST_INT i -> i
  | CONST_CHAR c -> int_of_char c
  | ARITH_BOP (op, e1, e2) -> (
    let v1 = eval_const_integer e1 in
    let v2 = eval_const_integer e2 in
    match op with
    | ADD -> v1 + v2
    | SUB -> v1 - v2
    | MUL -> v1 * v2
    | DIV -> v1 / v2
    | MOD -> v1 mod v2
    | BOR -> v1 lor v2
    | BAND -> v1 land v2)
  | REL_BOP (op, e1, e2) -> (
    let v1 = eval_const_integer e1 in
    let v2 = eval_const_integer e2 in
    let b =
      match op with
      | GT -> v1 > v2
      | LT -> v1 < v2
      | GEQ -> v1 >= v2
      | LEQ -> v1 <= v2
      | LOR -> v1 != 0 || v2 != 0
      | LAND -> v1 != 0 && v2 != 0
      | EQ -> v1 = v2
      | NEQ -> v1 != v2
    in
    match b with true -> 1 | false -> 0)
  | ARITH_UOP ((PRE_INCR | POST_INCR | PRE_DECR | POST_DECR), _) ->
    failwith "Array index is not a constant integer expression"
  | ARITH_UOP (BNOT, e) -> -eval_const_integer e - 1
  | ARITH_UOP (NEG, e) -> -eval_const_integer e
  | REL_UOP (LNOT, e) -> if eval_const_integer e = 0 then 1 else 0

let rec print_pgm pgm =
  List.iter
    (fun x ->
      print_toplevel x;
      print_endline "")
    pgm

and print_toplevel = function
  | TOP_DECL d ->
    print_string "(";
    print_decl d;
    print_string ")"
  | FUN (d, l) ->
    print_fun_decl d;
    print_endline "";
    print_local l

and print_decl = function DECL_VAR d -> print_var_decl d | _ -> ()

and print_var_decl (t, x) =
  print_string x;
  print_string " is of type ";
  print_ty t

and print_fun_decl = function
  | t, f, l ->
    print_string f;
    print_string " returns type ";
    print_ty t;
    print_string " with arguments:";
    List.iter
      (fun d ->
        print_var_decl d;
        print_endline ",")
      l

and print_ty = function
  | INT -> print_string "int"
  | CHAR -> print_string "char"
  | VOID -> print_string "void"
  | STRUCT id -> print_string ("struct " ^ id)
  | PTR t ->
    print_string "pointer to ";
    print_ty t
  | ARR (t, i) ->
    print_string "array to ";
    print_ty t;
    print_string (" with length " ^ string_of_int i)

and print_local = function
  | d_l, s_l ->
    print_string "Scope with decls : {";
    List.iter
      (fun d ->
        print_decl d;
        print_endline ",")
      d_l;
    print_endline "}";
    print_string "      with stmts : {";
    List.iter
      (fun s ->
        print_stmt s;
        print_endline ",")
      s_l;
    print_endline "}"

and print_stmt = function
  | SCOPE l -> print_local l
  | EXPR e -> print_expr e
  | RETURN None -> print_string "return;"
  | RETURN (Some e) ->
    print_string "return ";
    print_expr e;
    print_string ";"
  | IF (e_o, stmt, else_o) ->
    print_string "(";
    print_string "if ";
    (match e_o with None -> () | Some e -> print_expr e);
    print_string "then ";
    print_stmt stmt;
    (match else_o with
    | None -> ()
    | Some s ->
      print_string "else ";
      print_stmt s);
    print_string ")"
  | WHILE (_, body) ->
    print_string "while with body : {";
    print_stmt body;
    print_endline "}"
  | FOR (_, _, _, body) ->
    print_string "for with body : {";
    print_stmt body;
    print_endline "}"
  | BREAK | CONTINUE | EMPTY -> ()

and print_expr = function
  | VAR x -> print_string x
  | CONST_INT i -> print_int i
  | CONST_CHAR c ->
    print_char '\'';
    print_char c;
    print_char '\''
  | STRING s -> print_string ("\"" ^ s ^ "\"")
  | ASSIGN (e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string ":=";
    print_expr e2;
    print_string ")"
  | REF e ->
    print_string "(&";
    print_expr e;
    print_string ")"
  | DEREF (e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string "[";
    print_expr e2;
    print_string "]";
    print_string ")"
  | FIELD (e, l) ->
    print_string "(";
    print_expr e;
    print_string ".";
    print_string l;
    print_string ")"
  | CALL (e, args) ->
    print_expr e;
    print_string "(";
    List.iter
      (fun e ->
        print_expr e;
        print_string ",")
      args;
    print_string ")"
  | ARITH_BOP (bop, e1, e2) ->
    print_string "(";
    print_expr e1;
    print_arith_bop bop;
    print_expr e2;
    print_string ")"
  | REL_BOP (bop, e1, e2) ->
    print_string "(";
    print_expr e1;
    print_rel_bop bop;
    print_expr e2;
    print_string ")"
  | ARITH_UOP (uop, e) ->
    print_string "(";
    print_arith_uop uop e;
    print_string ")"
  | REL_UOP (uop, e) ->
    print_string "(";
    print_rel_uop uop e;
    print_string ")"

and print_arith_bop = function
  | ADD -> print_string "+"
  | SUB -> print_string "-"
  | MUL -> print_string "*"
  | DIV -> print_string "/"
  | MOD -> print_string "%"
  | BOR -> print_string "|"
  | BAND -> print_string "&"

and print_rel_bop = function
  | GT -> print_string ">"
  | LT -> print_string "<"
  | GEQ -> print_string ">="
  | LEQ -> print_string "<="
  | LOR -> print_string "||"
  | LAND -> print_string "&&"
  | EQ -> print_string "=="
  | NEQ -> print_string "!="

and print_arith_uop uop e =
  match uop with
  | PRE_INCR ->
    print_string "++";
    print_expr e
  | POST_INCR ->
    print_expr e;
    print_string "++"
  | PRE_DECR ->
    print_string "--";
    print_expr e
  | POST_DECR ->
    print_expr e;
    print_string "--"
  | BNOT ->
    print_string "~";
    print_expr e
  | NEG ->
    print_string "-";
    print_expr e

and print_rel_uop uop e =
  match uop with
  | LNOT ->
    print_string "!";
    print_expr e
