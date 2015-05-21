LEX = flex
YACC = bison
CC = gcc

.PHONY: all clean

parser: parser.o node.o lexer.o

node.o: node.c parser.y

lexer.o: lexer.l parser.y

clean:
	rm -f parser.tab.* lex.yy.c parser *.o
