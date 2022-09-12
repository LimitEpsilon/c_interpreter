(*
   SNU 430.414 Introduction to Compilers
   Code by Joonhyup Lee, ECE@SNU
   Csub lexer
*)

{
 open Parser
 exception Eof
 exception LexicalError
 let verbose_id s = (*print_string ("id: " ^ s ^ "\n");*) s
 let verbose s =  ((*print_string s; print_newline();*) s)
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 31
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("struct", STRUCT);
                    ("return", RETURN);
                    ("break", BREAK);
                    ("continue", CONTINUE);
				    ("int", INT);
                    ("char", CHAR);
                    ("if", IF);
                    ("else",ELSE);
                    ("while", WHILE);
                    ("for", FOR);
                    ("void" , VOID);
                  ]
let handle_string s =
    let s' = ref "" in
    let i = ref 1 in
    let l = (String.length s) - 1 in
    while !i < l do
        (match s.[!i] with
            | '\\' -> incr i; (match s.[!i] with
              | 'n' -> s' := !s' ^ "\n"
              | 't' -> s' := !s' ^ "\t"
              | '\\' -> s' := !s' ^ "\\"
              | '\"' -> s' := !s' ^ "\""
              | c -> failwith ("invalid escape character \\" ^ (String.make 1 c)))
            | c -> s' := !s' ^ (String.make 1 c)
        ); incr i
    done;
    !s'
}

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let escaped = '\\' ['r' 't' 'n' '\"' '\\']
let string_literal = '\"' ( escaped | [^'\\' '\n'] )* '\"'
let char_literal = '\'' ( escaped | [^'\\' '\n'] ) '\''
let number = ['0'-'9']+
let assignop = "=" | "+=" | "-=" | "/=" | "*=" | "%="
let relop = ">" | "<" | ">=" | "<="
let equop = "==" | "!="

rule start =
 parse blank { start lexbuf }
     | "/*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | number { NUM (int_of_string (verbose (Lexing.lexeme lexbuf))) }
     | id { let id = Lexing.lexeme lexbuf in
       try Hashtbl.find keyword_tbl (verbose_id id) with _ -> ID id }
     | string_literal { let s = Lexing.lexeme lexbuf in
       STRING_LIT (verbose (handle_string s)) }
     | char_literal { let s = Lexing.lexeme lexbuf in
       CHAR_LIT (String.get (verbose (handle_string s)) 0) }
     | assignop { let s = Lexing.lexeme lexbuf in ASSIGNOP (verbose s) }
     | relop { let s = Lexing.lexeme lexbuf in RELOP (verbose s) }
     | equop { let s = Lexing.lexeme lexbuf in EQUOP (verbose s) }
     | "+" {PLUS}
     | "-" {MINUS}
     | "*" {STAR}
     | "/" {DIV}
     | "&" {AMP}
     | "++" {INCR}
     | "--" {DECR}
     | "~" {BNOT}
     | "|" {BOR}
     | "!" {LNOT}
     | "&&" {LAND}
     | "||" {LOR}
     | "]" {RBRACKET}
     | "[" {LBRACKET}
     | ";" {SEMICOLON}
	 | "," {COMMA}
	 | "." {DOT}
     | "->" {ARROW}
     | "(" {LPAREN}
     | ")" {RPAREN}
	 | "{" {LBRACE}
	 | "}" {RBRACE}
     | eof {EOF}
     | _ {raise LexicalError}

and comment = parse
     "/*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*/" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
