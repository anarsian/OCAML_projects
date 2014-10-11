%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token COLONCOLON
%token SEMI
%token LBRAC
%token RBRAC
%token LPAREN
%token RPAREN
%token TRUE 
%token FALSE
%token ARROW
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE
%token NE
%token AND
%token OR
%token <string> Id

%nonassoc LET FUN IF
%left OR
%left AND
%left EQ NE LT LE
%left PLUS MINUS
%left MUL DIV
%left APP




%start exp
%type <Nano.expr> exp

%%

exp: 
	LET Id EQ exp IN exp		{ Let($2,$4,$6)}
  |	LET REC Id EQ exp IN exp	{ Letrec($3,$5,$7)}
  |	FUN Id ARROW exp		{ Fun($2, $4) }
  |     IF exp THEN exp ELSE exp	{ If($2,$4, $6)}
    |     e2				{ $1 }

e2:
	e2 OR e3 			{ Bin($1, Or, $3) }
  |	e3				{ $1 }
e3:
	e3 AND e4			{ Bin($1, And, $3) }
  |	e4				{ $1 }
e4:
	e4 EQ e41			{ Bin($1, Eq, $3) }
  |	e4 NE e41			{ Bin($1, Ne, $3) }
  |	e4 LT e41			{ Bin($1, Lt, $3) }
  |	e4 LE e41			{ Bin($1, Le, $3) }
  |     e41				{ $1 }
e41:
  	e5 SEMI e41			{ Bin($1,Cons, $3) }
  |	e5 COLONCOLON e41		{ Bin($1,Cons, $3) }
  |	LBRAC e41			{ $2 }
  |	e41 RBRAC			{ Bin($1,Cons,NilExpr)}
  |	e5				{ $1 }
e5:
	e5 PLUS e6			{ Bin($1, Plus, $3)}
  |	e5 MINUS e6			{ Bin($1, Minus, $3)}
  |	e6				{ $1 }
e6:
	e6 MUL e7			{ Bin($1, Mul, $3)}
  |	e6 DIV e7			{ Bin($1, Div, $3)}
  |	e7				{ $1 }
e7:
	e7 e8				{ App($1,$2) }
  |	e8				{ $1 }
	
e8:

  	Num                        { Const $1 }
  |	Id			   { Var $1  }
  |	TRUE			   { True }
  |	FALSE			   { False}
  |	LPAREN exp RPAREN	   	{ $2 }
  |	LBRAC RBRAC		   { NilExpr }





	
