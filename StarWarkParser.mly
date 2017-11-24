%{

  open StarWark

%}

%token <int>    NUM
%token <string> LBL

%token DEREF
%token LNOT
%token NOT
%token LAND
%token LOR
%token LXOR
%token LSL
%token LSR
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token CONJ
%token DISJ
%token HUH
%token COLON

%token DATA
%token LARR
%token IF
%token GOTO
%token SPAWN

%token LPAREN
%token RPAREN
%token NL
%token EOF

%start source
%type <StarWark.source> source

%right HUH COLON
%left  DISJ
%left  CONJ
%left  LOR
%left  LXOR
%left  LAND
%left  EQ NEQ
%left  LT LE GT GE
%left  LSL LSR
%left  ADD SUB
%left  MUL DIV MOD
%right DEREF LNOT NOT unary_arith

%%

source:
  | blanks lines blanks EOF
      { List.rev $2 }

blanks:
  | { () }
  | NL blanks
      { () }

lines:
  | line
      { $1 :: [] }
  | lines NL line
      { $3 :: $1 }

line:
  | COLON cell
      { ("", $2) }
  | LBL COLON cell
      { ($1, $3) }

cell:
  | DATA NUM          { Data $2          }
  | expr LARR expr    { Asgn ($1, $3)    }
  | GOTO expr         { Cjmp (Int 1, $2) }
  | IF expr GOTO expr { Cjmp ($2, $4)    }
  | SPAWN expr        { Spwn $2          }

expr :
  | NUM { Int $1 }
  | LBL { Lbl $1 }

  | SUB expr
    %prec unary_arith
      { unop Neg $2 }
  | ADD expr
    %prec unary_arith
      { unop Pos $2 }

  | DEREF expr { unop Deref $2 }
  | LNOT  expr { unop Lnot  $2 }
  | NOT   expr { unop Not   $2 }

  | expr LAND expr { binop Land $1 $3 }
  | expr LOR  expr { binop Lor  $1 $3 }
  | expr LXOR expr { binop Lxor $1 $3 }
  | expr LSL  expr { binop Lsl  $1 $3 }
  | expr LSR  expr { binop Lsr  $1 $3 }
  | expr ADD  expr { binop Add  $1 $3 }
  | expr SUB  expr { binop Sub  $1 $3 }
  | expr MUL  expr { binop Mul  $1 $3 }
  | expr DIV  expr { binop Div  $1 $3 }
  | expr MOD  expr { binop Mod  $1 $3 }
  | expr EQ   expr { binop Eq   $1 $3 }
  | expr NEQ  expr { binop Neq  $1 $3 }
  | expr LT   expr { binop Lt   $1 $3 }
  | expr LE   expr { binop Le   $1 $3 }
  | expr GT   expr { binop Gt   $1 $3 }
  | expr GE   expr { binop Ge   $1 $3 }
  | expr CONJ expr { binop Conj $1 $3 }
  | expr DISJ expr { binop Disj $1 $3 }

  | expr HUH expr COLON expr
      { Cond ($1, $3, $5) }

  | LPAREN expr RPAREN { $2 }

%%

