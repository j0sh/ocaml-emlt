%{
  open Types

  let chars2str chars =
    let buf = Buffer.create 200 in
    let add = Buffer.add_char buf in
    List.iter (fun c -> if c = '"' then add '\\'; add c;) chars;
    Buffer.contents buf
%}

%token EOF CLOSE_F
%token <char> CH
%token <string> OPEN OPEN_P OPEN_Y OPEN_F

%start <Types.prim list> prog

%%

prog: tok* EOF { $1 }

opener: OPEN { Open $1 } | OPEN_P { Open_p $1 } | OPEN_Y { Open_y $1 }

tok:
| CH { Ch $1 }
| opener { $1 }
| OPEN_F CH* CLOSE_F { Fn ($1, (chars2str $2)) }
