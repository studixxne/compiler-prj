%option noyywrap

%{
#include "prog.tab.h"
%}

dig [0-9]
let [A-Za-z]

%%

"+"                         { return PLUS; }
"-"                         { return MINU; }
"*"                         { return MULT; }
"/"                         { return DIVI; }
"="                         { return ASIG; }
"("                         { return LPAR; }
")"                         { return RPAR; }
","                         { return COMA; }
";"                         { return SEMI; }
{dig}+                      { yylval.n = atoi(yytext); return NUM; }
({let}|_)({let}|{dig}|_)*   { yylval.s = strdup(yytext); return ID; }
[ \t\n]+                    { /* Skip whitespaces. */ }
.                           { /* Wildcard '.' that matches anything. */
                              printf("Invalid token: %s\n", yytext);
                              exit(1);
                            }

%%
