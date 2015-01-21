(*
  <%= ... %>    execute ocaml value, print result (expects string)
  <% ... %>     execute ocaml
  <%yield <name> %>  expects a thing of <name> and will incorporate it
          (maybe a generalized version of: <%= apply(<name>, ())  %>
*)

open Parser
open Types
open Printf

let () =
  let lex = Lexing.from_channel stdin in
  let rec p = function
    | Open s :: t -> printf "main/open: %s\n" s; p t
    | Open_p s :: t -> printf "main/open/print: %s\n"s ; p t
    | Open_y s :: t -> printf "main/open/yield %s\n" s; p t
    | String s :: t -> print_endline s; p t
    | Ch c :: t -> printf "%c" c; p t
    | Eof :: t | t -> () in
  p (prog Lexer.tokens lex)
