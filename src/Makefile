java: java.tab.c lex.yy.c
	g++ java.tab.c lex.yy.c -o java_symbol_table 

java.tab.c: java.y
	bison -d -t java.y 

lex.yy.c:java.l
	flex java.l

clean:
	rm java_symbol_table  java.tab.* lex.yy.c 