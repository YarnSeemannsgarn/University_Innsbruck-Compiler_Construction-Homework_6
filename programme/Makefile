LEX = flex
YACC = bison
CC = gcc

all: parser

parser: lex.yy.c parser.tab.c
	$(CC) -o parser parser.tab.c lex.yy.c

lex.yy.c: scanner.l parser.tab.c
	$(LEX) scanner.l

parser.tab.c: parser.y
	$(YACC) -d parser.y

clean:
	rm -f parser.tab.* lex.yy.c parser
