YACC=bison
YFLAGS = -d

all: parser

parser: node.o parser.o scanner.o

node.o: node.c parser.y

scanner.o: scanner.l parser.y

parser.o: parser.tab.c 
	$(CC) -c -o parser.o parser.tab.c

parser.tab.c: parser.y
	$(YACC) $(YFLAGS) parser.y


clean:
	$(RM) parser *.o *.tab.*
