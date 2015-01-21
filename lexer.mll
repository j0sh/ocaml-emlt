{
  open Lexing
  open Parser

}

rule tokens = parse
  | "<%" { print_endline "open"; OPEN }
  | "<%=" { print_endline "open_p"; OPEN_P }
  | "<%yield " { print_endline "yield"; OPEN_Y }
  | "%>" { print_endline "close"; CLOSE }
  | eof { print_endline "eof"; EOF }
  | _ { CH }
