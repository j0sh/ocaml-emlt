{
  open Lexing
  open Parser

}

rule tokens = parse
| "<%" { OPEN (string (Buffer.create 200 ) lexbuf) }
| "<%=" { OPEN_P (string (Buffer.create 100) lexbuf) }
| "<%yield " { OPEN_Y (string (Buffer.create 200) lexbuf) }
| "<%for " { OPEN_F (string (Buffer.create 50) lexbuf) }
| "<% end %>"'\n'? { CLOSE_F }
| eof { EOF }
| _ as c { CH c }
and string buf = parse
| "%>"'\n'? { Buffer.contents buf }
| _ as c { Buffer.add_char buf c; string buf lexbuf }
