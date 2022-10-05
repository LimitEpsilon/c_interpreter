/*
 * SNU 430.414 Introduction to Compilers
 * Code by Joonhyup Lee, ECE@SNU
 * Parser for csub
 */

%{
open Type_checker

let str_unique_label = ref 0

let new_anonymous_struct () =
  let s = "\'anonymous_" ^ string_of_int !str_unique_label in
  incr str_unique_label;
  s

let transl_assign = function
    | ("=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE (PTR t1), RVALUE (ARR (t2, _))
        | LVALUE t1, (RVALUE t2 | LVALUE t2) ->
           if t1 = t2 then () else failwith "Assignment types don't match!"
        | _ -> failwith "Can't assign to a rvalue!");
       (t1, Csub.ASSIGN (e1, e2))
    | ("+=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE INT, (RVALUE INT | LVALUE INT) -> ()
        | _ -> failwith "Assignment types don't match!");
       (t1, Csub.ASSIGN (e1, ARITH_BOP (ADD, e1, e2)))
    | ("-=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE INT, (RVALUE INT | LVALUE INT) -> ()
        | _ -> failwith "Assignment types don't match!");
       (t1, Csub.ASSIGN (e1, ARITH_BOP (SUB, e1, e2)))
    | ("/=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE INT, (RVALUE INT | LVALUE INT) -> ()
        | _ -> failwith "Assignment types don't match!");
       (t1, Csub.ASSIGN (e1, ARITH_BOP (DIV, e1, e2)))
    | ("*=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE INT, (RVALUE INT | LVALUE INT) -> ()
        | _ -> failwith "Assignment types don't match!");
       (t1, Csub.ASSIGN (e1, ARITH_BOP (MUL, e1, e2)))
    | ("%=", (t1, e1), (t2, e2)) ->
       (match (t1, t2) with
        | LVALUE INT, (RVALUE INT | LVALUE INT) -> ()
        | _ -> failwith "Assignment types don't match!");
       (t1, Csub.ASSIGN (e1, ARITH_BOP (MOD, e1, e2)))
    | (s, _, _) -> failwith (s ^ " is not a valid assignop!")

let transl_rel : string -> Csub.bop_rel = function
    | ">" -> GT
    | "<" -> LT
    | ">=" -> GEQ
    | "<=" -> LEQ
    | s -> failwith (s ^ " is not a valid relop!")

let transl_eq : string -> Csub.bop_rel = function
    | "==" -> EQ
    | "!=" -> NEQ
    | s -> failwith (s ^ " is not a valid equop!")

let rec make_ptr (ty, ptrs) =
  let prevent_premature_struct = function
    | Csub.STRUCT x -> if Hashtbl.mem struct_tbl x then ()
                  else failwith ("Struct " ^ x ^ " used before definition!")
    | _ -> ()
  in
  match ptrs with
    | 0 -> prevent_premature_struct ty; ty
    | n when n > 0 -> make_ptr (Csub.PTR ty, n - 1)
    | _ -> assert false

let handle_var_decl (ty, id_list) =
  let map (id, ptrs, arr_opt) = match arr_opt with
    | None -> (make_ptr (ty, ptrs), id)
    | Some i -> (Csub.ARR (make_ptr (ty, ptrs), i), id)
  in
  List.map map id_list

let check_predicate = function
    | (LVALUE INT | RVALUE INT) -> ()
    | _ -> failwith "Predicate must be int!"
%}
%token EOF SEMICOLON COMMA
%token LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token STRUCT INT CHAR VOID IF ELSE WHILE FOR RETURN BREAK CONTINUE
%token STAR AMP DOT ARROW PLUS MINUS MOD DIV INCR DECR BNOT BOR LNOT LAND LOR
%token <string> ID
%token <int> NUM
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> ASSIGNOP
%token <string> RELOP
%token <string> EQUOP

%start program
%type <Csub.pgm> program

%%

program: top_list EOF { $1 }
    ;
top_list: { push_env (Hashtbl.create 11); [] }
    | top_list top { $1 @ $2 }
    ;
top: ty var_decls SEMICOLON
        { let decls = List.map (fun d -> Csub.DECL_VAR d) (handle_var_decl ($1, $2))
          in
          List.map (fun d -> update_env d; Csub.TOP_DECL d) decls }
    | ty ptrs fun_decl SEMICOLON
        { let t = make_ptr ($1, $2) in
          let decl = (t, fst $3, snd $3) in
          insert_function decl;
          update_env (DECL_FUN decl);
          [Csub.TOP_DECL (DECL_FUN decl)] }
    | ty SEMICOLON { [] }
    | enter_function decl_list stmt_list RBRACE
        { pop_env (); [Csub.FUN ($1, ($2, $3))] }
    ;
enter_function: ty ptrs fun_decl LBRACE {
         let t = make_ptr ($1, $2) in
         let decl = (t, fst $3, snd $3) in
         update_env (DECL_FUN decl);
         insert_function decl;
         current_return_ty := t;
         push_env (Hashtbl.create 5);
         List.iter (fun d -> update_env (DECL_VAR d)) (snd $3);
         decl }
    ;
var_decls: var_decl { [$1] }
    | var_decls COMMA var_decl { $1 @ [$3] }
    ;
var_decl: ptrs ID { ($2, $1, None) }
    | ptrs ID LBRACKET RBRACKET { ($2, $1 + 1, None) }
    | ptrs ID LBRACKET expr RBRACKET { ($2, $1, Some (Csub.eval_const_integer (snd $4))) }
    ;
ptrs: { 0 }
    | ptrs STAR { $1 + 1 }
    ;
fun_decl: ID LPAREN param_list RPAREN { ($1, $3) }
    ;
decl_list: { [] }
    | decl_list decl { $1 @ $2 }
    ;
decl: ty var_decls SEMICOLON
        { let decls = List.map (fun decl -> Csub.DECL_VAR decl) (handle_var_decl ($1, $2))
          in List.iter update_env decls; decls }
    | ty ptrs fun_decl SEMICOLON
        { let t = make_ptr ($1, $2) in
          let decl = (t, fst $3, snd $3) in
          insert_function decl;
          let decls = [Csub.DECL_FUN decl]
          in List.iter update_env decls; decls }
    | ty SEMICOLON { [] }
    ;
param_list: { [] }
    | VOID { [] }
    | param_nonempty_list { $1 }
    ;
param_nonempty_list: param_decl { $1 }
    | param_nonempty_list COMMA param_decl
        { $1 @ $3 }
    ;
param_decl: ty ptrs ID { handle_var_decl ($1, [($3, $2, None)]) }
    | ty ptrs ID LBRACKET RBRACKET { handle_var_decl ($1, [($3, $2 + 1, None)]) }
    | ty ptrs ID LBRACKET expr RBRACKET { handle_var_decl ($1, [($3, $2 + 1, None)]) }
    ;
ty: INT { Csub.INT }
    | CHAR { Csub.CHAR }
    | VOID { Csub.VOID }
    | STRUCT ID { Csub.STRUCT $2 }
    | enter_struct decl_list RBRACE
        { let name = $1 in
          insert_struct name (top_env ());
          pop_env ();
          Csub.STRUCT name }
    ;
enter_struct: STRUCT LBRACE { push_env (Hashtbl.create 5); new_anonymous_struct () }
    | STRUCT ID LBRACE { push_env (Hashtbl.create 5); $2 }
    ;
enter_scope: LBRACE { push_env (Hashtbl.create 5) }
    ;
stmt_list: { [] }
    | stmt_list stmt { $1 @ [$2] }
    ;
stmt: closed_stmt { $1 }
    | open_stmt { $1 }
    ;
closed_stmt: enter_scope decl_list stmt_list RBRACE { pop_env (); Csub.SCOPE ($2, $3) }
    | expr SEMICOLON { Csub.EXPR (snd $1) }
    | RETURN expr_opt SEMICOLON
        { (match fst $2 with
            | (LVALUE t | RVALUE t) ->
               if t = !current_return_ty
               then ()
               else (Csub.print_ty t; failwith "Return type mismatch!")
            | _ -> failwith "Return type mismatch!");
          Csub.RETURN (snd $2) }
    | WHILE LPAREN expr_opt RPAREN closed_stmt
        { check_predicate (fst $3);
          Csub.WHILE (snd $3, $5) }
    | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN closed_stmt
        { check_predicate (fst $5);
          Csub.FOR (snd $3, snd $5, snd $7, $9) }
    | BREAK SEMICOLON { Csub.BREAK }
    | CONTINUE SEMICOLON { Csub.CONTINUE }
    | SEMICOLON { Csub.EMPTY }
    | IF LPAREN expr_opt RPAREN closed_stmt ELSE closed_stmt
        { check_predicate (fst $3);
          Csub.IF (snd $3, $5, Some $7) }
    ;
open_stmt: IF LPAREN expr_opt RPAREN stmt
        { check_predicate (fst $3);
          Csub.IF (snd $3, $5, None) }
    | IF LPAREN expr_opt RPAREN closed_stmt ELSE open_stmt
        { check_predicate (fst $3);
          Csub.IF (snd $3, $5, Some $7) }
    | WHILE LPAREN expr_opt RPAREN open_stmt
        { check_predicate (fst $3);
          Csub.WHILE (snd $3, $5) }
    | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN open_stmt
        { check_predicate (fst $5);
          Csub.FOR (snd $3, snd $5, snd $7, $9) }
    ;
expr_opt: { (RVALUE INT, None) }
    | expr { let e = $1 in (fst e, Some (snd e)) }
    ;
expr: or_expr ASSIGNOP expr
        { transl_assign ($2, (check_expr $1, $1), $3) }
    | or_expr { let e = $1 in (check_expr e, e) }
    ;
or_expr: or_expr LOR and_expr { Csub.REL_BOP (LOR, $1, $3) }
    | and_expr { $1 }
    ;
and_expr: and_expr LAND bor_expr { Csub.REL_BOP (LAND, $1, $3) }
    | bor_expr { $1 }
    ;
bor_expr: bor_expr BOR band_expr { Csub.ARITH_BOP (BOR, $1, $3) }
    | band_expr { $1 }
    ;
band_expr: band_expr AMP equ_expr { Csub.ARITH_BOP (BAND, $1, $3) }
    | equ_expr { $1 }
    ;
equ_expr: equ_expr EQUOP rel_expr { Csub.REL_BOP (transl_eq $2, $1, $3) }
    | rel_expr { $1 }
    ;
rel_expr: rel_expr RELOP add_expr { Csub.REL_BOP (transl_rel $2, $1, $3) }
    | add_expr { $1 }
    ;
add_expr: add_expr PLUS mul_expr { Csub.ARITH_BOP (ADD, $1, $3) }
    | add_expr MINUS mul_expr { Csub.ARITH_BOP (SUB, $1, $3) }
    | mul_expr { $1 }
    ;
mul_expr: mul_expr STAR unary { Csub.ARITH_BOP (MUL, $1, $3) }
    | mul_expr DIV unary { Csub.ARITH_BOP (DIV, $1, $3) }
    | mul_expr MOD unary { Csub.ARITH_BOP (MOD, $1, $3) }
    | unary { $1 }
    ;
unary: MINUS atom { Csub.ARITH_UOP (NEG, $2) }
    | LNOT atom { Csub.REL_UOP (LNOT, $2) }
    | BNOT atom { Csub.ARITH_UOP (BNOT, $2) }
    | INCR atom { Csub.ARITH_UOP (PRE_INCR, $2) }
    | DECR atom { Csub.ARITH_UOP (PRE_DECR, $2) }
    | AMP atom { Csub.REF $2 }
    | STAR atom { Csub.DEREF ($2, CONST_INT 0) }
    | atom { $1 }
    ;
atom: LPAREN expr RPAREN { snd $2 }
    | NUM { Csub.CONST_INT $1 }
    | CHAR_LIT { Csub.CONST_CHAR $1 }
    | ID { Csub.VAR $1 }
    | STRING_LIT { Csub.STRING $1 }
    | atom INCR { Csub.ARITH_UOP (POST_INCR, $1) }
    | atom DECR { Csub.ARITH_UOP (POST_DECR, $1) }
    | atom LBRACKET expr RBRACKET { Csub.DEREF ($1, snd $3) }
    | atom DOT ID { Csub.FIELD ($1, $3) }
    | atom ARROW ID { Csub.FIELD (DEREF ($1, CONST_INT 0), $3) }
    | atom LPAREN args RPAREN { Csub.CALL ($1, $3) }
    ;
args: { [] }
    | nonempty_args { $1 }
    ;
nonempty_args: expr { [snd $1] }
    | nonempty_args COMMA expr { $1 @ [snd $3] }
    ;
%%
