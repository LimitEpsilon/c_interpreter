open Csub_parser

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.start lexbuf
  with Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let main () =
  let print_code = ref false in
  let src = ref "" in
  let spec =
    [("-pp", Arg.Set print_code, "Pretty printing of the input program")]
  in
  let usage = "Usage : run <options> <file> \nOptions : " in
  let _ =
    Arg.parse spec
      (fun x ->
        if Sys.file_exists x then src := x
        else raise (Arg.Bad (x ^ ": File unfound")))
      usage
  in

  if !src = "" then Arg.usage spec usage
  else
    let file_channel = open_in !src in
    let lexbuf = Lexing.from_channel file_channel in
    let () = if !print_code then Csub.debug := true in
    try Type_checker.check_pgm (parse_with_error lexbuf) |> ignore
    with Lexer.LexicalError -> print_endline (!src ^ ": Lexical error")

let _ = main ()
