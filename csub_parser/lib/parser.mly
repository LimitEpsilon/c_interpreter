/*
 * SNU 430.414 Introduction to Compilers
 * Code by Joonhyup Lee, ECE@SNU
 * Parser for csub
 */

%{
let str_unique_label = ref 0

let new_anonymous_struct () =
  let s = "\'anonymous_" ^ string_of_int !str_unique_label in
  incr str_unique_label;
  s

let transl_assign : string * Csub.expr * Csub.expr -> Csub.expr = function
    | ("=", e1, e2) -> ASSIGN (e1, e2)
    | ("+=", e1, e2) -> ASSIGN (e1, ARITH_BOP (ADD, e1, e2))
    | ("-=", e1, e2) -> ASSIGN (e1, ARITH_BOP (SUB, e1, e2))
    | ("/=", e1, e2) -> ASSIGN (e1, ARITH_BOP (DIV, e1, e2))
    | ("*=", e1, e2) -> ASSIGN (e1, ARITH_BOP (MUL, e1, e2))
    | ("%=", e1, e2) -> ASSIGN (e1, ARITH_BOP (MOD, e1, e2))
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

let rec make_ptr (ty, ptrs) = match ptrs with
    | 0 -> ty
    | n when n > 0 -> make_ptr (Csub.PTR ty, n - 1)
    | _ -> assert false

let handle_var_decl (ty, id_list) =
  let map (id, ptrs, arr_opt) = match arr_opt with
    | None -> (make_ptr (ty, ptrs), id)
    | Some i -> (Csub.ARR (make_ptr (ty, ptrs), i), id)
  in
  List.map map id_list
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

%right NOELSE
%left ELSE
%right ASSIGNOP
%left LOR
%left LAND
%left BOR
%left BAND
%left EQUOP
%left RELOP
%left PLUS SUB
%left MUL DIV MOD
%right BNOT LNOT INCR DECR MINUS STAR AMP
%left DOT ARROW LBRACKET LPAREN

%start program
%type <Csub.pgm> program

%%

program: top_list EOF { $1 }
    ;
top_list: { [] }
    | top_list top { $1 @ $2 }
    ;
top: ty var_decls SEMICOLON
        { List.map (fun decl -> Csub.TOP_DECL (DECL_VAR decl)) (handle_var_decl ($1, $2)) }
    | ty ptrs fun_decl SEMICOLON
        { let t = make_ptr ($1, $2) in [Csub.TOP_DECL (DECL_FUN (t, fst $3, snd $3))] }
    | ty SEMICOLON { [] }
    | ty ptrs fun_decl local
        { let t = make_ptr ($1, $2) in [Csub.FUN ((t, fst $3, snd $3), $4)] }
    ;
var_decls: var_decl { [$1] }
    | var_decls COMMA var_decl { $1 @ [$3] }
    ;
var_decl: ptrs ID { ($2, $1, None) }
    | ptrs ID LBRACKET RBRACKET { ($2, $1 + 1, None) }
    | ptrs ID LBRACKET expr RBRACKET { ($2, $1, Some (Csub.eval_const_integer $4)) }
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
        { List.map (fun decl -> Csub.DECL_VAR decl) (handle_var_decl ($1, $2)) }
    | ty ptrs fun_decl SEMICOLON
        { let t = make_ptr ($1, $2) in [Csub.DECL_FUN (t, fst $3, snd $3)] }
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
    | STRUCT LBRACE decl_list RBRACE
        { let name = new_anonymous_struct () in
          Csub.insert_struct name $3; Csub.STRUCT name }
    | STRUCT ID LBRACE decl_list RBRACE
        { Csub.insert_struct $2 $4; Csub.STRUCT $2 }
    ;
local: LBRACE decl_list stmt_list RBRACE { ($2, $3) }
    ;
stmt_list: { [] }
    | stmt_list stmt { $1 @ [$2] }
    ;
stmt: local { Csub.SCOPE $1 }
    | expr SEMICOLON { Csub.EXPR $1 }
    | RETURN expr_opt SEMICOLON { Csub.RETURN $2 }
    | IF LPAREN expr_opt RPAREN stmt else_opt { Csub.IF ($3, $5, $6) }
    | WHILE LPAREN expr_opt RPAREN stmt { Csub.WHILE ($3, $5) }
    | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN stmt { Csub.FOR ($3, $5, $7, $9) }
    | BREAK SEMICOLON { Csub.BREAK }
    | CONTINUE SEMICOLON { Csub.CONTINUE }
    | SEMICOLON { Csub.EMPTY }
    ;
else_opt: %prec NOELSE { None }
    | ELSE stmt { Some $2 }
    ;
expr_opt: { None }
    | expr { Some $1 }
    ;
expr: LPAREN expr RPAREN { $2 }
    | ID { Csub.VAR $1 }
    | NUM { Csub.CONST_INT $1 }
    | CHAR_LIT { Csub.CONST_CHAR $1 }
    | STRING_LIT { Csub.STRING $1 }
    | expr ASSIGNOP expr { transl_assign ($2, $1, $3) }
    | AMP expr { Csub.REF $2 }
    | STAR expr { Csub.DEREF ($2, CONST_INT 0) }
    | expr LBRACKET expr RBRACKET { Csub.DEREF ($1, $3) }
    | expr DOT ID { Csub.FIELD ($1, $3) }
    | expr ARROW ID { Csub.FIELD (Csub.DEREF ($1, CONST_INT 0), $3) }
    | expr LPAREN args RPAREN { Csub.CALL ($1, $3) }
    | expr PLUS expr { Csub.ARITH_BOP (ADD, $1, $3) }
    | expr DIV expr { Csub.ARITH_BOP (DIV, $1, $3) }
    | expr MOD expr { Csub.ARITH_BOP (MOD, $1, $3) }
    | expr BOR expr { Csub.ARITH_BOP (BOR, $1, $3) }
    | expr AMP expr %prec BAND { Csub.ARITH_BOP (BAND, $1, $3) }
    | expr STAR expr %prec MUL { Csub.ARITH_BOP (MUL, $1, $3) }
    | expr MINUS expr %prec SUB { Csub.ARITH_BOP (SUB, $1, $3) }
    | expr RELOP expr { Csub.REL_BOP (transl_rel $2, $1, $3) }
    | expr EQUOP expr { Csub.REL_BOP (transl_eq $2, $1, $3) }
    | expr LOR expr { Csub.REL_BOP (LOR, $1, $3) }
    | expr LAND expr { Csub.REL_BOP (LAND, $1, $3) }
    | INCR expr { Csub.ARITH_UOP (PRE_INCR, $2) }
    | expr INCR { Csub.ARITH_UOP (POST_INCR, $1) }
    | DECR expr { Csub.ARITH_UOP (PRE_DECR, $2) }
    | expr DECR { Csub.ARITH_UOP (POST_DECR, $1) }
    | BNOT expr { Csub.ARITH_UOP (BNOT, $2) }
    | MINUS expr { Csub.ARITH_UOP (NEG, $2) }
    | LNOT expr { Csub.REL_UOP (LNOT, $2) }
    ;
args: { [] }
    | nonempty_args { $1 }
    ;
nonempty_args: expr { [$1] }
    | nonempty_args COMMA expr { $1 @ [$3] }
	;
%%
