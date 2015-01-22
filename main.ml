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

let yields toks =
  let f acc = function Open_y h -> h::acc | _ -> acc in
  List.fold_left f [] toks |> List.rev

let prologue (y : string list) : unit =
  let f = sprintf "?(%s = fun () -> ())" in
  let z = List.map f y |> String.concat " " in
  print_endline ("let print ?(f = fun s -> print_string s; flush stdout) " ^
  z ^ " param =")

let epilogue () = print_endline "()"

let print_printer () = print_endline "let () = print ()"

let () =
  let lex = Lexing.from_channel stdin in
  let rec p = function
    | Open s :: t -> print_endline s; p t
    | Open_p s :: t -> printf "let () = f (%s) in \n"s ; p t
    | Open_y s :: t -> printf "let () = %s () in \n" s; p t
    | String s :: t -> printf "let () = f \"%s\" in \n" s; p t
    | Fn (f, s) :: t -> printf "let %s () = f \"%s\" in\n" f s; p t
    | Ch c :: t -> (); p t
    | Eof :: t | t -> () in
  let toks = prog Lexer.tokens lex |> collapse in
  prologue (yields toks);
  p toks;
  epilogue ();
  print_printer ()
