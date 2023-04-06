parser: lex.yy.c java.tab.c
	g++ lex.yy.c java.tab.c -o java_scanner

lex.yy.c: java.l
	flex java.l

java.tab.c: java.y
	bison -t -d java.y

clean:
	rm java_scanner java.tab.* lex.yy.c *.dot *.pdf ../tests/test_*/*.csv *.csv *.txt
