{
  open Lexing
  open Parser

}

rule tokens = parse
| "<%" { OPEN (string (Buffer.create 200 ) lexbuf) }
| "<%=" { OPEN_P (string (Buffer.create 200) lexbuf) }
| "<%yield " { OPEN_Y (string (Buffer.create 200) lexbuf) }
| eof { EOF }
| _ as c { CH c }
and string buf = parse
| "%>" { Buffer.contents buf }
| _ as c { Buffer.add_char buf c; string buf lexbuf }
