%define parse.error verbose

%{
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
#include "variable.h"
extern FILE *yyin;
// To suppress warnings.
void yyerror(const char *s);
int yylex(void);
%}

/* Note: You will not have to fix the code above */

%union {
    int n;
    char *s;
    struct AST *a;
    struct VarNode *v;
}

/* Declare tokens and types */
%token <n> NUM
%token <s> ID
%token PLUS MINU MULT DIVI ASIG LPAR RPAR COMA SEMI

%type <a> E T F
%type <v> Vars

%start Prog

%%

Prog:
    E           { printf("%d\n", eval_ast(NULL, $1));     }
  | Vars SEMI E { printf("%d\n", eval_ast($1, $3));       }
;

Vars:
    ID ASIG NUM           { $$ = add_var($1, $3, NULL);   }
  | ID ASIG NUM COMA Vars { $$ = add_var($1, $3, $5);     }
;

E:  E PLUS T  { $$ = make_binop_ast(Add, $1, $3);         }
  | E MINU T  { $$ = make_binop_ast(Sub, $1, $3);         }
  | T         { $$ = $1;                                  }
;

T:  T MULT F  { $$ = make_binop_ast(Mul, $1, $3);         }  
  | T DIVI F  { $$ = make_binop_ast(Div ,$1 ,$3);         }
  | F         { $$ = $1;                                  }
;

F:  NUM            { $$ = make_num_ast($1);               }
  | ID             { $$ = make_id_ast($1);                }
  | LPAR E RPAR    { $$ = $2;                             }
  | MINU F         { $$ = make_neg_ast($2);               }
;

%%

/* Note: DO NOT TOUCH THE CODE BELOW */

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input file>\n", argv[0]);
        exit(1);
    }

    if (NULL == (yyin = fopen(argv[1], "r"))) {
        printf("Failed to open %s\n", argv[1]);
        exit(1);
    }

    yyparse();
}

void yyerror(const char *s) {
    fprintf(stderr, "error: %s\n", s);
}
