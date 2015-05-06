%{
#include <stdio.h>
#include "parser.tab.h"

void yyerror(char *);
char message[100];
%}

%option noyywrap
%option yylineno

%x COMMENT

WHITESPACE [ \t\n]
DIGIT [0-9]
ID [a-zA-Z_][a-zA-Z_0-9]*

A [aA]
B [bB]
C [cC]
D [dD]
E [eE]
F [fF]
G [gG]
H [hH]
I [iI]
J [jJ]
K [kK]
L [lL]
M [mM]
N [nN]
O [oO]
P [pP]
Q [qQ]
R [rR]
S [sS]
T [tT]
U [uU]
V [vV]
W [wW]
X [xX]
Y [yY]
Z [zZ]

%%

"{"                  BEGIN(COMMENT);
<COMMENT>[^}\n]+     ;
<COMMENT>\n          ;
<COMMENT><<EOF>>     sprintf(message, "Lexical Error (Line: %d) EOF in comment", yylineno); yyerror(message);
<COMMENT>"}"         BEGIN(INITIAL);

    /* Keywords */
{P}{R}{O}{G}{R}{A}{M}			return PROGRAM;
{V}{A}{R}				return VAR;
{I}{N}{T}{E}{G}{E}{R}			return INTEGER;
{A}{R}{R}{A}{Y}				return ARRAY;
{O}{F}					return OF;
{R}{E}{A}{L}				return REAL;
{B}{O}{O}{L}{E}{A}{N}                   return BOOLEAN;
{B}{E}{G}{I}{N}				return _BEGIN;
{W}{H}{I}{L}{E}				return WHILE;
{D}{O}					return DO;
{I}{F}					return IF;
{T}{H}{E}{N}				return THEN;
{E}{L}{S}{E}				return ELSE;
{E}{N}{D}				return END;
{F}{O}{R}				return FOR;
{T}{O}					return TO;
{D}{O}{W}{N}{T}{O}                      return DOWNTO;
{R}{E}{A}{D}                            return READ;
{W}{R}{I}{T}{E}                         return WRITE;
{D}{I}{V}                               return DIV;
{M}{O}{D}                               return MOD;
{A}{N}{D}                               return AND;
{O}{R}                                  return OR;
{T}{R}{U}{E}                            return TRUE;
{F}{A}{L}{S}{E}                         return FALSE;
{N}{O}{T}                               return NOT;
					       
   /* Special symbols */		       
";"					return SEMICOLON;
","					return COMMA;
":="					return ASSIGNMENT;
":"					return COLON;
"["					return LEFT_SQUARE_BRACKET;
"]"					return RIGHT_SQUARE_BRACKET;
".."					return DOT_DOT;
"."					return DOT;
"("					return LEFT_BRACKET;
")"					return RIGHT_BRACKET;
"*"					return STAR;
"/"					return SLASH;
"+"					return PLUS;
"-"					return MINUS;
"<>"					return UNEQUAL;
"<"					return LESS_THAN;
">"					return GREATER_THAN;
">="|"=>"				return GREATER_EQUAL_THAN;
"<="|"=<"				return LESS_EQUAL_THAN;
"="					return EQUAL;
					       
\"(\\.|[^\\"])*\"|\'(\\.|[^\\'])*\'     return STRING;
					       
    /* Numbers */			       
{DIGIT}+                                return NUMBER;
					       
    /* Identifiers */			       
{ID}                                    return ID;

    /* Whitespace */
{WHITESPACE}+                           ;

    /* Lexical errors */
.                                       sprintf(message, "Lexical Error (Line: %d) Illegal input", yylineno); yyerror(message);

%%
