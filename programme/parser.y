%{ 
#include <stdio.h>

typedef enum { NT_PROGRAM, NT_ASSIGN, NT_IF, NT_WHILE, NT_STATEMENT, NT_CONST, NT_VAR, NT_TYPE, NT_EXPR, NT_INT_CONST, NT_REAL_CONST, NT_BOOL_CONST, NT_STRING_CONST, NT_IDENTIFIER, NT_OP } node_type;
typedef enum { OP_PLUS, OP_MINUS, OP_MUL, OP_DIV, OP_MOD, OP_LT, OP_LE, OP_GT, OP_GE, OP_EQ, OP_NE, OP_AND, OP_OR } operator;

// Node values as union
typedef union { 
    int iValue; /* integer, true, false, compOp, addOp, mulOp */
    float fValue;
    /* number */
    char *identifier;
    /* identifier */
    /* list of BNF right-hand side symbols of nonterminal type */
    struct _node *body;
} node_value;

// Node
typedef struct _node {
    node_type type;
    node_value val;
    struct _node *next ;
    /* decl-list, stmt-list */
} node;

// Union Helper
typedef enum { int_val_type, float_val_type, char_val_type, node_val_type } node_value_type;
typedef struct union_node_describer_s {
    node_value_type val_type;
    node_value val;
} union_node_describer;

// Prototypes
node *mknode(node_type type, union_node_describer node_describer, node *next);
void yyerror(char *);
%}

// Tokens from homework 4
%token		/* Keywords */ PROGRAM VAR INTEGER ARRAY OF REAL BOOLEAN _BEGIN /* BEGIN is already a builtin keyword */ WHILE DO IF THEN ELSE END FOR TO DOWNTO READ WRITE DIV MOD AND OR TRUE FALSE NOT 
                /* Special symbols */ SEMICOLON COMMA ASSIGNMENT COLON LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET DOT_DOT DOT LEFT_BRACKET RIGHT_BRACKET STAR SLASH PLUS MINUS UNEQUAL LESS_THAN GREATER_THAN GREATER_EQUAL_THAN LESS_EQUAL_THAN EQUAL STRING 
                /* Numbers */ NUMBER 
                /* Identifiers */ ID
			
%start	        start

// see http://www.gnu.org/software/bison/manual/html_node/Shift_002fReduce.html
// see http://www.gnu.org/software/bison/manual/html_node/Non-Operators.html#Non-Operators
%right THEN ELSE

%%

// Grammar from homework 4
start                   : PROGRAM ID SEMICOLON varDec compStmt DOT
                        ;

varDec 		        : VAR varDecList 
		        | /* ε */
                        ;

varDecList              : varDecList identListType SEMICOLON 
                        | identListType SEMICOLON
                        ;

identListType           : identList COLON type
                        ;
 
identList               : identList COMMA ID 
                        | ID
                        ;
		 
type                    : simpleType
                        | ARRAY LEFT_SQUARE_BRACKET NUMBER DOT_DOT NUMBER RIGHT_SQUARE_BRACKET OF simpleType
                        ;
 
simpleType	        : INTEGER
                        | REAL
                        | BOOLEAN
                        ;
 
compStmt	        : _BEGIN stmtList END
                        ;

stmtList                : stmtList SEMICOLON statement 
                        | statement
                        ;
 
statement	        : assignStmt
		        | compStmt
		        | ifStmt
		        | whileStmt
		        | forStmt
		        | READ LEFT_BRACKET exprList RIGHT_BRACKET
		        | WRITE LEFT_BRACKET exprList RIGHT_BRACKET
                        ;

assignStmt              : ID ASSIGNMENT expr 
                        | ID LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET ASSIGNMENT expr
                        ;
 
ifStmt		        : IF expr THEN statement
                        | IF expr THEN statement ELSE statement	
                        ;
 
whileStmt	        : WHILE expr DO statement
                        ;
 
forStmt		        : FOR ID ASSIGNMENT expr toPart expr DO statement
                        ;
 
toPart		        : TO
		        | DOWNTO
                        ;
 
expr                    : simpleExpr relOp simpleExpr
                        | simpleExpr
                        ;

exprList                : exprList COMMA expr 
                        | expr
                        ;


simpleExpr              : simpleExpr addOp term 
                        | term
                        ;

term                    : term mulOp factor 
                        | factor
                        ;

factor		        : NUMBER {union_node_describer node_describer; node_describer.val_type = int_val_type; node_describer.val.iValue = (int)yylval; $$ = mknode(NT_INT_CONST, node_describer, NULL); }
		        | FALSE
		        | TRUE
		        | ID
                        | ID LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET	
		        | NOT factor
		        | MINUS factor
		        | LEFT_BRACKET expr RIGHT_BRACKET
		        | STRING
                        ;
 		        
relOp		        : LESS_THAN
		        | LESS_EQUAL_THAN
		        | GREATER_THAN
		        | GREATER_EQUAL_THAN
		        | EQUAL
		        | UNEQUAL
                        ;
 		        
addOp		        : PLUS
		        | MINUS
		        | OR
                        ;
 		        
mulOp		        : STAR
		        | SLASH
		        | DIV
		        | MOD
		        | AND
                        ;

%%

node *mknode(node_type type, union_node_describer node_describer, node *next){
    node *newnode = (node *)malloc(sizeof(node));

    newnode->type = type;
    
    node_value n_val;
    switch(node_describer.val_type){
    case int_val_type: n_val.iValue = node_describer.val.iValue; break;
    case float_val_type: n_val.fValue = node_describer.val.fValue; break;
    case char_val_type: n_val.identifier = node_describer.val.identifier; break;
    case node_val_type: n_val.body = node_describer.val.body; break;
    }

    newnode->val = n_val;
    newnode->next = next;
    
    return(newnode);
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
