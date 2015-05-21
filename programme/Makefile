YFLAGS = -d

.PHONY: all clean

parser: node.o parser.o scanner.o

node.o: node.c parser.y

scanner.o: scanner.l parser.y

clean:
	$(RM) parser *.o y.tab.h
