{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
   eof         { EOF }
  | [' ' '\t' '\n' '\r']	{ token lexbuf }
  | "true"			{ TRUE }
  | "false" 			{ FALSE }
 (******************************************)
  | "let"			{ LET }
  | "rec"			{ REC }
  | "="				{ EQ }
  | "in"			{ IN }
  | "fun"			{ FUN }
  | "->"			{ ARROW}
  | "if"			{ IF}
  | "then"			{ THEN }
  | "else"			{ ELSE }
  | "+"				{ PLUS }
  | "-"				{ MINUS }
  | "*"				{ MUL  }
  | "/"				{ DIV }
  | "<"				{ LT }
  | "<="			{ LE }
  | "!="			{ NE }
  | "&&"			{ AND }
  | "||"			{ OR }
  | "("				{ LPAREN }
  | ")"				{ RPAREN }
  | "["				{ LBRAC }
  | "]"				{ RBRAC }
  | ";"				{ SEMI }
  | "::"			{ COLONCOLON }
  | ['0'-'9']+ as k		{Num(int_of_string k)}
  | ['a'-'z' 'A'-'Z'] ['0'-'9' 'a'-'z' 'A'-'Z']* as l		{Id(l)} 
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
