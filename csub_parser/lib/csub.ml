type pgm = toplevel list
and toplevel = TOP_DECL of decl | FUN of fun_decl * local
and local = decl list * stmt list
and decl = DECL_VAL of val_decl | DECL_STRUCT of struct_decl
and val_decl = DECL_VAR of var_decl | DECL_FUN of fun_decl
and var_decl = ty * id * int option (* for arrays *)
and fun_decl = ty * id * var_decl list
and struct_decl = id option * val_decl list

and ty =
  | INT
  | CHAR
  | VOID
  | STRUCT_NAMED of id
  | STRUCT_DECLARED of struct_decl
  | PTR of ty  (** statements change control flow *)

and stmt =
  | SCOPE of local
  | EXPR of expr
  | RETURN of expr option
  | IF of expr option * stmt * stmt option
  | WHILE of expr option * stmt
  | FOR of expr option * expr option * expr option * stmt
  | BREAK
  | CONTINUE
  | EMPTY (* just a semicolon *)

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

and bop_arith = PLUS | MINUS | MUL | DIV | MOD | BOR | BAND (* bitwise *)
and bop_rel = GT | LT | GEQ | LEQ | LOR | LAND | EQ | NEQ
and uop_arith = PRE_INCR | POST_INCR | PRE_DECR | POST_DECR | BNOT | NEG
and uop_rel = LNOT
and id = string

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

and print_decl = function
  | DECL_VAL d -> print_val_decl d
  | DECL_STRUCT d -> print_struct_decl d

and print_val_decl = function
  | DECL_VAR d -> print_var_decl d
  | DECL_FUN d -> print_fun_decl d

and print_var_decl = function
  | t, x, None ->
    print_string x;
    print_string " is of type ";
    print_ty t
  | t, x, Some _ ->
    print_string x;
    print_string " is of type ";
    print_ty t;
    print_string " array"

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

and print_struct_decl = function
  | None, l ->
    print_endline "anonymous struct with fields: {";
    List.iter
      (fun d ->
        print_val_decl d;
        print_endline ",")
      l;
    print_endline "}"
  | Some id, l ->
    print_endline ("struct with name " ^ id ^ " with fields: {");
    List.iter
      (fun d ->
        print_val_decl d;
        print_endline ",")
      l;
    print_endline "}"

and print_ty = function
  | INT -> print_string "int"
  | CHAR -> print_string "char"
  | VOID -> print_string "void"
  | STRUCT_NAMED id -> print_string ("struct with name " ^ id)
  | STRUCT_DECLARED d -> print_struct_decl d
  | PTR t ->
    print_string "(";
    print_ty t;
    print_string ")*"

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
  | WHILE (_, _) | FOR (_, _, _, _) | BREAK | CONTINUE | EMPTY -> ()

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
  | PLUS -> print_string "+"
  | MINUS -> print_string "-"
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
