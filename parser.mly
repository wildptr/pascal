%{
open Ast
%}

%token EOF
%token LParen
%token RParen
%token Star
%token Plus
%token Comma
%token Minus
%token Dot
%token Colon
%token Semi
%token Lt
%token Eq
%token Gt
%token <string> Ident
%token <int> Int
%token ColonEq
%token LtEq
%token NotEq
%token GtEq
%token AND
%token ARRAY
%token ASSERT
%token BEGIN
%token CASE
%token CONST
%token DIV
%token DO
%token DOWNTO
%token ELSE
%token END
%token FALSE
%token FOR
%token FUNCTION
%token IF
%token IN
%token INVARIANT
%token MOD
%token NIL
%token NOT
%token OF
%token OR
%token PACKED
%token PROCEDURE
%token PROGRAM
%token RECORD
%token REPEAT
%token SET
%token SHL
%token SHR
%token THEN
%token TO
%token TRUE
%token TYPE
%token UNTIL
%token VAR
%token WHILE
%token XOR

%type <Ast.ast_program> program
%type <Ast.ast_proc> proc_def

%start program

%%

program:
  PROGRAM name=Ident Semi block=block Dot EOF
  {{ name; block }}

block:
  vars=loption(var_part) procs=list(proc_def) body=comp_stmt
  {{ vars; procs; body }}

var_part:
  VAR list(var_decl) {$2}

var_decl:
  ident_list Colon typ Semi {$1,$3}

ident_list:
  separated_nonempty_list(Comma, Ident) {$1}

typ:
  Ident {$1}

stmt:
  | lvalue ColonEq expr { A_AssignStmt ($1, $3) }
  | comp_stmt { A_CompStmt $1 }
  | ASSERT expr { A_AssertStmt $2 }
  | IF expr THEN stmt { A_IfStmt ($2, $4, A_CompStmt []) }
  | IF expr THEN stmt ELSE stmt { A_IfStmt ($2, $4, $6) }
  | REPEAT option(invariant) stmt_list UNTIL expr { A_RepeatStmt ($2, $3, $5) }
  | Ident { A_CallStmt ($1, []) }
  | Ident LParen expr_list RParen { A_CallStmt ($1, $3) }

invariant: INVARIANT expr {$2}

stmt_list: separated_list(Semi, stmt) {$1}

comp_stmt:
  BEGIN stmt_list END {$2}

factor:
  | Int { A_IntExpr $1 }
  | TRUE { A_BoolExpr true }
  | FALSE { A_BoolExpr false }
  | Ident { A_IdentExpr $1 }
  | LParen expr RParen {$2}

term_op:
  | Star        { Canon.Mul }

add_expr_op:
  | Plus        { Canon.Add }
  | Minus       { Canon.Sub }

rel_op:
  | Eq          { Canon.Eq }
  | NotEq       { Canon.NotEq }
  | Lt          { Canon.Lt }
  | GtEq        { Canon.GtEq }
  | Gt          { Canon.Gt }
  | LtEq        { Canon.LtEq }

term:
  | factor {$1}
  | term term_op factor { A_BinaryExpr ($2, $1, $3) }

add_expr:
  | term {$1}
  | add_expr add_expr_op term { A_BinaryExpr ($2, $1, $3) }

rel_expr:
  | add_expr {$1}
  | rel_expr rel_op add_expr { A_BinaryExpr ($2, $1, $3) }

expr: rel_expr {$1}

proc_def:
  PROCEDURE name=Ident params=loption(delimited(LParen, separated_list(Comma, param_decl), RParen)) Semi block=block Semi
  {{ name; params; block }}

param_decl:
  boption(VAR) ident_list Colon Ident { $1, $2, $4 }

lvalue:
  Ident { A_IdentExpr $1 }

expr_list: separated_list(Comma, expr) {$1}

(* vim: set indentexpr= : *)
