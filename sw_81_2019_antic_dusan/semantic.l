%option noyywrap yylineno

%{
   #include <string.h>
   #include "semantic.tab.h"
   #include "defs.h"
%}

%%

[ \t\n]+             { /* skip */ }

"int"                { yylval.i = INT;  return _TYPE; }
"unsigned"           { yylval.i = UINT; return _TYPE; }
"if"                 { return _IF; }
"else"               { return _ELSE; }
"return"             { return _RETURN; }
"for"                { return _FOR; }/**/
"while"              { return _WHILE; }	/**/
"branch"             { return _BRANCH; }/**/
"one"                { return _ONE; }/**/
"two"                { return _TWO; }/**/
"three"              { return _THREE; }/**/
"other"              { return _OTHER; }/**/
"end_branch"         { return _ENDBRANCH; }/**/
"void"               { yylval.i = VOID; return _TYPE; }/**/
"select"             { return _SELECT; }	/**/
"from"               { return _FROM; }	/**/
"where"              { return _WHERE; }	/**/

"("                  { return _LPAREN; }
")"                  { return _RPAREN; }
"{"                  { return _LBRACKET; }
"}"                  { return _RBRACKET; }
";"                  { return _SEMICOLON; }
":"                  { return _COLON; }/**/
"="                  { return _ASSIGN; }
","                  { return _COMMA; } /**/

"+"                  { yylval.i = ADD; return _AROP; }
"++"                 { return _AROP_AROP; }/**/
"-"                  { yylval.i = SUB; return _AROP; }

"<"                  { yylval.i = LT; return _RELOP; }
">"                  { yylval.i = GT; return _RELOP; }
"<="                 { yylval.i = LE; return _RELOP; }
">="                 { yylval.i = GE; return _RELOP; }
"=="                 { yylval.i = EQ; return _RELOP; }
"!="                 { yylval.i = NE; return _RELOP; }

"//".* 
"/*"([^*]|"\n"|(\*+([^*/]|"\n")))*"*"+"/"	{}

[a-zA-Z][a-zA-Z0-9]* { yylval.s = strdup(yytext); 
                       return _ID; }
[+-]?[0-9]{1,10}     { yylval.s = strdup(yytext); 
                       return _INT_NUMBER;}
[0-9]{1,10}[uU]      { yylval.s = strdup(yytext); 
                       yylval.s[yyleng-1] = 0;
                       return _UINT_NUMBER;}

.                    { printf("line %d: LEXICAL ERROR on char %c\n", yylineno, *yytext);}
%%
