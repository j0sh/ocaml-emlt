(*
  <%= ... %>    execute ocaml value, print result (expects string)
  <% ... %>     execute ocaml
  <%yield <name> %>  expects a thing of <name> and will incorporate it
          (maybe a generalized version of: <%= apply(<name>, ())  %>
*)

open Parser
open Types

let () =
  let lex = Lexing.from_channel stdin in
  let rec p = function
    | Open :: t -> print_endline "main/open"; p t
    | Close :: t -> print_endline "main/close"; p t
    | Open_p :: t -> print_endline "main/open/print"; p t
    | Open_y :: t -> print_endline "main/open/yield"; p t
    (*| TEXT s :: t -> print_endline s; p t*)
    | Eof :: t | t -> () in
  p (prog Lexer.tokens lex)
