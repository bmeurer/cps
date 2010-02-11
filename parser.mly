/* parser.mly - ocamlyacc parser for Anf */

%token <int> INT
%token <bool> BOOL
%token <Anf.variable> ID
%token PLUS MINUS TIMES
%token LSS GRT LEQ GEQ EQ
%token LPAREN RPAREN
%token LET REC IN AND
%token IF THEN ELSE
%token FUN TO COMMA

%start expr
%type <Anf.variable list> idlist
%type <Anf.operator> op
%type <Anf.constant> const
%type <Anf.primitive> prim
%type <Anf.primitive list> primlist
%type <Anf.abstraction> abstr
%type <(Anf.variable * Anf.abstraction) list> reclist
%type <Anf.expression> expr

%%

idlist:
  ID { [$1] }
| ID COMMA idlist { $1 :: $3 }
;

op:
  PLUS { Anf.Op_add }
| MINUS { Anf.Op_sub }
| TIMES { Anf.Op_mul }
| LSS { Anf.Op_lss }
| GRT { Anf.Op_grt }
| LEQ { Anf.Op_leq }
| GEQ { Anf.Op_geq }
| EQ { Anf.Op_eq }
;

const:
  INT { Anf.Const_int $1 }
| BOOL { Anf.Const_bool $1 }
;

prim:
  const { Anf.Prim_const $1 }
| ID { Anf.Prim_var $1 }
;

primlist:
  prim { [$1] }
| prim COMMA primlist { $1 :: $3 }
;

abstr:
  FUN LPAREN idlist RPAREN TO expr { ($3, $6) }
;

reclist:
  ID EQ abstr { [$1, $3] }
| ID EQ abstr AND reclist { ($1, $3) :: $5 }
;

expr:
  prim { Anf.Exp_prim $1 }
| prim op prim { Anf.Exp_op ($1, $2, $3) }
| abstr { Anf.Exp_abstr $1 }
| prim LPAREN primlist RPAREN { Anf.Exp_app ($1, $3) }
| IF prim THEN expr ELSE expr { Anf.Exp_cond ($2, $4, $6) }
| LET ID EQ expr IN expr { Anf.Exp_let ($2, $4, $6) }
| LET REC reclist IN expr { Anf.Exp_letrec ($3, $5) }
;
