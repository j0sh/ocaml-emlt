(*
  <%= ... %>    execute ocaml value, print result (expects string)
  <% ... %>     execute ocaml
  <%yield <name> %>  expects a thing of <name> and will incorporate it
          (maybe a generalized version of: <%= apply(<name>, ())  %>
*)

open Parser
open Types
open Printf

let print = ref false
let exec = ref false

let opts = [
  "-p",
  Arg.Set print,
  "Call the printer within the generated code.";
  "-x",
  Arg.Set exec,
  "Execute the generated code and print results to stdout."
]

let usage =
  let s = sprintf "Usage: %s -[px] ... " (Sys.argv.(0)) in
  s

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

let fors toks =
  let f acc = function Fn (h, _) -> (String.trim h) :: acc | _ -> acc in
  List.fold_left f [] toks |> List.rev

let prologue toks emit =
  let f = sprintf "?(%s = fun () -> ())" in
  let z = List.map f (yields toks) |> String.concat " " in
  let fors = fors toks in
  let fl = List.length fors > 0 in
  let fs = if fl then "?layout " else "" in
  let z = sprintf "let print ?(f = fun s -> print_string s; flush stdout) %s %sparam =\n"
      z fs in
  emit z

let epilogue toks emit =
  let fors = fors toks in
  let fl = List.length fors > 0 in
  let fm = List.map (fun s -> sprintf "?%s:(Some %s)" s s) fors in
  let fm = String.concat " " fm in
  let z = if fl then "match layout with Some (layout, p) -> layout ?f:(Some f) "
      ^ fm ^ " p | None -> ()\n" else "()\n" in
  emit z

let print_printer emit = emit  "let () = print ()\n"

let execute buf =
  let (fname, oc) = Filename.open_temp_file "emlt" "eml" in
  Buffer.contents buf |> output_string oc;
  close_out oc;
  let ob = Buffer.create 1000 in
  let ic = Unix.open_process_in (sprintf "ocaml %s" fname) in
  begin try while true do
    Buffer.add_char ob (input_char ic)
  done with End_of_file ->
    let finish () = Sys.remove fname in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> finish ()
    | Unix.WEXITED n -> finish (); exit n
    | _ -> finish (); exit 2
  end;
  Buffer.output_buffer stdout ob

let () =
  Arg.parse opts (fun x -> ()) usage;
  let lex = Lexing.from_channel stdin in
  let buf = Buffer.create (if !exec then 2000 else 0) in
  let emit = if !exec then Buffer.add_string buf
    else print_string in
  let rec p = function
    | Open s :: t -> emit (sprintf "%s\n" s); p t
    | Open_p s :: t -> emit (sprintf "let () = f (%s) in\n" s) ; p t
    | Open_y s :: t -> emit (sprintf "let () = %s () in\n" s); p t
    | String s :: t -> emit (sprintf "let () = f \"%s\" in\n" s); p t
    | Fn (f, s) :: t -> emit (sprintf "let %s () = f \"%s\" in\n" f s); p t
    | Ch c :: t -> (); p t
    | Eof :: t | t -> () in
  let toks = prog Lexer.tokens lex |> collapse in
  prologue toks emit;
  p toks;
  epilogue toks emit;
  if !print || !exec then print_printer emit;
  if !exec then execute buf;
