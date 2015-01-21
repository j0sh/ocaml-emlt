%{
  open Types
%}

%token OPEN OPEN_P OPEN_Y CLOSE EOF CH
%token <string> TEXT

%start <Types.prim list> prog

%%

prog: tok* EOF { $1 }

opener: OPEN { Open } | OPEN_P { Open_p } | OPEN_Y { Open_y }

tok:
| CH { Eof }
| opener CH* CLOSE tok { print_endline "PARSER statement"; $1 }
