%{
  open Types

%}

%token EOF CLOSE_F
%token <char> CH
%token <string> OPEN OPEN_P OPEN_Y OPEN_F

%start <Types.prim list> prog

%%

prog: tok* EOF { $1 }

opener: OPEN { Open $1 } | OPEN_P { Open_p $1 } | OPEN_Y { Open_y $1 }

fn: CH { Ch $1 } | opener { $1 }

tok:
| CH { Ch $1 }
| opener { $1 }
| OPEN_F fn* CLOSE_F { Fn ($1, $2) }
