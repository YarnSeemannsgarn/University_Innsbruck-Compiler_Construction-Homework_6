%{ 
#include <stdio.h>

typedef enum { PROGRAM, ASSIGN, IF, WHILE, STATEMENT, CONST, VAR, TYPE, EXPR, INT_CONST, REAL_CONST, BOOL_CONST, STRING_CONST, IDENTIFIER, OP } node_type;
typedef enum { PLUS, MINUS, MUL, DIV, MOD, LT, LE, GT, GE, EQ, NE, AND, OR } operator;

typedef struct _node {
    node_type type;
    union {
	int iValue; /* integer, true, false, compOp, addOp, mulOp */
	float fValue; /* number */
	char *identifier; /* identifier */
	/* list of BNF right-hand side symbols of nonterminal type */
	struct _node *body;
    };
    struct _node *next; /* decl-list, stmt-list */
} node;

// Prototypes
node *new_node(node_type type);
void yyerror(char *);
%}

// Tokens from homework 4
%token		/* Keywords */ T_PROGRAM T_VAR T_INTEGER T_ARRAY T_OF T_REAL T_BOOLEAN T_BEGIN T_WHILE T_DO T_IF T_THEN T_ELSE T_END T_FOR T_TO T_DOWNTO T_READ T_WRITE T_DIV T_MOD T_AND T_OR T_TRUE T_FALSE T_NOT 
                /* Special symbols */ T_SEMICOLON T_COMMA T_ASSIGNMENT T_COLON T_LEFT_SQUARE_BRACKET T_RIGHT_SQUARE_BRACKET T_DOT_DOT T_DOT T_LEFT_BRACKET T_RIGHT_BRACKET T_STAR T_SLASH T_PLUS T_MINUS T_UNEQUAL T_LESS_THAN T_GREATER_THAN T_GREATER_EQUAL_THAN T_LESS_EQUAL_THAN T_EQUAL T_STRING 
                /* Numbers */ T_NUMBER 
                /* Identifiers */ T_ID
			
%start	        program

// see http://www.gnu.org/software/bison/manual/html_node/Shift_002fReduce.html
// see http://www.gnu.org/software/bison/manual/html_node/Non-Operators.html#Non-Operators
%right T_THEN T_ELSE

%union {
    struct _node *n;
}

%type <n> program varDec compStmt /* TODO: Add more */

%%

// Grammar from homework 4
program                 : T_PROGRAM T_ID T_SEMICOLON varDec compStmt T_DOT { $$ = new_node(PROGRAM); 
                                                                             $$->body = $4;
                                                                             $4->body = $5; }
                        ;

varDec 		        : T_VAR varDecList 
		        | /* ε */
                        ;

varDecList              : varDecList identListType T_SEMICOLON 
                        | identListType T_SEMICOLON
                        ;

identListType           : identList T_COLON type
                        ;
 
identList               : identList T_COMMA T_ID 
                        | T_ID
                        ;
		 
type                    : simpleType
                        | T_ARRAY T_LEFT_SQUARE_BRACKET T_NUMBER T_DOT_DOT T_NUMBER T_RIGHT_SQUARE_BRACKET T_OF simpleType
                        ;
 
simpleType	        : T_INTEGER
                        | T_REAL
                        | T_BOOLEAN
                        ;
 
compStmt	        : T_BEGIN stmtList T_END
                        ;

stmtList                : stmtList T_SEMICOLON statement 
                        | statement
                        ;
 
statement	        : assignStmt
		        | compStmt
		        | ifStmt
		        | whileStmt
		        | forStmt
		        | T_READ T_LEFT_BRACKET exprList T_RIGHT_BRACKET
		        | T_WRITE T_LEFT_BRACKET exprList T_RIGHT_BRACKET
                        ;

assignStmt              : T_ID T_ASSIGNMENT expr 
                        | T_ID T_LEFT_SQUARE_BRACKET expr T_RIGHT_SQUARE_BRACKET T_ASSIGNMENT expr
                        ;
 
ifStmt		        : T_IF expr T_THEN statement
                        | T_IF expr T_THEN statement T_ELSE statement	
                        ;
 
whileStmt	        : T_WHILE expr T_DO statement
                        ;
 
forStmt		        : T_FOR T_ID T_ASSIGNMENT expr toPart expr T_DO statement
                        ;
 
toPart		        : T_TO
		        | T_DOWNTO
                        ;
 
expr                    : simpleExpr relOp simpleExpr
                        | simpleExpr
                        ;

exprList                : exprList T_COMMA expr 
                        | expr
                        ;


simpleExpr              : simpleExpr addOp term 
                        | term
                        ;

term                    : term mulOp factor 
                        | factor
                        ;

factor		        : T_NUMBER
		        | T_FALSE
		        | T_TRUE
		        | T_ID
                        | T_ID T_LEFT_SQUARE_BRACKET expr T_RIGHT_SQUARE_BRACKET	
		        | T_NOT factor
		        | T_MINUS factor
		        | T_LEFT_BRACKET expr T_RIGHT_BRACKET
		        | T_STRING
                        ;
 		        
relOp		        : T_LESS_THAN
		        | T_LESS_EQUAL_THAN
		        | T_GREATER_THAN
		        | T_GREATER_EQUAL_THAN
		        | T_EQUAL
		        | T_UNEQUAL
                        ;
 		        
addOp		        : T_PLUS
		        | T_MINUS
		        | T_OR
                        ;
 		        
mulOp		        : T_STAR
		        | T_SLASH
		        | T_DIV
		        | T_MOD
		        | T_AND
                        ;

%%

node *new_node(node_type type){
    node *new_node = (node *)malloc(sizeof(node));
    new_node->type = type;

    new_node->next = NULL;

    return new_node;
}

void printTree(node *tree)
{

}

void yyerror(char *s) {
    extern int yylineno;
    extern char *yytext;
    fprintf(stderr, "%s (line %d): \"%s\"\n", s, yylineno, yytext);
}

int main() {
    int status = yyparse();
    if (!status) {
        printf("parsing successful\n");
    }
    return status;
}
