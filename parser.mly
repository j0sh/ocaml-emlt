%{
  open Types
%}

%token EOF
%token <char> CH
%token <string> OPEN OPEN_P OPEN_Y

%start <Types.prim list> prog

%%

prog: tok* EOF { $1 }

opener: OPEN { Open $1 } | OPEN_P { Open_p $1 } | OPEN_Y { Open_y $1 }

tok:
| CH { Ch $1 }
| opener tok { $1 }
