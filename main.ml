(*
  <%= ... %>    execute ocaml value, print result (expects string)
  <% ... %>     execute ocaml
  <%yield <name> %>  expects a thing of <name> and will incorporate it
          (maybe a generalized version of: <%= apply(<name>, ())  %>
*)

open Parser
open Types
open Printf

let collapse toks =
  (* takes [Ch a; Ch b; Ch c;] and converts into [String abc] *)
  let f (buf, acc) = function
    | Ch a -> Buffer.add_char buf a; (buf, acc)
    | b -> begin if Buffer.length buf > 0 then
              ((Buffer.create 200), b::(String (Buffer.contents buf))::acc)
            else (buf, b::acc)
      end in
  let (buf, acc) = List.fold_left f ((Buffer.create 200), []) toks in
  String (Buffer.contents buf) :: acc |> List.rev

let prologue () =
  print_endline "let print ?(f = fun s -> print_string s; flush stdout) param ="

let epilogue () = print_endline "()"

let print_printer () = print_endline "let () = print ()"

let () =
  prologue ();
  let lex = Lexing.from_channel stdin in
  let rec p = function
    | Open s :: t -> print_endline s; p t
    | Open_p s :: t -> printf "let () = f (%s) in \n"s ; p t
    | Open_y s :: t -> printf "main/open/yield %s\n" s; p t
    | String s :: t -> printf "let () = f \"%s\" in \n" s; p t
    | Ch c :: t -> (); p t
    | Eof :: t | t -> () in
  prog Lexer.tokens lex |> collapse |> p;
  epilogue ();
  print_printer ()
