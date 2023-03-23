%option noyywrap nodefault
%option yylineno
%{ 
    #include "java.tab.h"
    #include <stdio.h>
    #include <fstream> 
    int yylex();
    void yyerror (const char *s);
    struct nodeptr{

    };
%}
ALPHA   [a-zA-Z]
DIGIT   [0-9]
ID  {ALPHA}+[a-zA-Z|0-9]*
INTEGER "0"|[0-9]{DIGIT}*
DECIMAL   "0"|{DIGIT}*"."{DIGIT}+
FLOATING [-+]?{DIGIT}*\.{DIGIT}+([eE][-+]?{DIGIT}+)?
BOOL "true"|"false"
NULL null
WS [ \t\n]+
STRING \"(\\.|[^"\\])*\"
Comment "//".*

%%
{WS}
"/*"    {
            int c;

            while((c = yyinput()) != 0)
                {
                if(c == '\n')
                {

                }

                else if(c == '*')
                    {
                    if((c = yyinput()) == '/')
                        break;
                    else
                        unput(c);
                    }
                }
            }
{Comment}
abstract {printf("abstract\n");return ABSTRACT;}
continue {printf("continue\n");return CONTINUE;}
for {printf("for\n");return FOR;}
new {printf("new\n");return NEW;}
switch {printf("switch\n");return SWITCH;}
assert {printf("assert\n");return ASSERT;}
default {printf("default\n");return DEFAULT;}
if {printf("if\n");return IF;}
package {printf("package\n");return PACKAGE;}
synchronized {printf("synchronized\n");return SYNCHRONIZED;}
boolean {printf("boolean\n");return BOOLEAN;}
do {printf("do\n");return DO;}
goto {printf("goto\n");return GOTO;}
private {printf("private\n");return PRIVATE;}
this {printf("this\n");return THIS;}
break {printf("break\n");return BREAK;}
double {printf("double\n");return DOUBLE;}
implements {printf("implements\n");return IMPLEMENTS;}
protected {printf("protected\n");return PROTECTED;}
throw {printf("throw\n");return THROW;}
byte {printf("byte\n");return BYTE;} 
else {printf("else\n");return ELSE;}
import {printf("import\n");return IMPORT;}
public {printf("public\n");return PUBLIC;}
throws {printf("throws\n");return THROWS;}
case {printf("case\n");return CASE;}
enum {printf("enum\n");return ENUM;}
instanceof {printf("instanceof\n");return INSTANCEOF;}
return {printf("return\n");return RETURN;}
transient {printf("transient\n");return TRANSIENT;}
catch {printf("catch\n");return CATCH;}
extends {printf("extends\n");return EXTENDS;}
int {printf("int\n");return INT;}
short {printf("short\n");return SHORT;}
try {printf("try\n");return TRY;}
char {printf("char\n");return CHAR;}
final {printf("final\n");return FINAL;}
interface {printf("interface\n");return INTERFACE;}
static {printf("static\n");return STATIC;}
void {printf("void\n");return VOID;}
class {printf("class\n");return CLASS;}
finally {printf("finally\n");return FINALLY;}
long {printf("long\n");return LONG;}
strictfp {printf("strictfp\n");return STRICTFP;}
volatile {printf("volatile\n");return VOLATILE;}
const {printf("const\n");return CONST;}
float {printf("float\n");return FLOAT;}
native {printf("native\n");return NATIVE;}
super {printf("super\n");return SUPER;}
while {printf("while\n");return WHILE;}
_ {printf("UNDERSCORE\n");return UNDERSCORE;}
exports {printf("exports\n");return EXPORTS;}
opens {printf("opens\n");return OPENS;}
requires {printf("requires\n");return REQUIRES;}
uses {printf("uses\n");USES;}
module {printf("module\n");return MODULE;}
permits {printf("permits\n");return PERMITS;}
sealed {printf("sealed\n");return SEALED;}
var {printf("var\n");return VAR;}
non-sealed {printf("non-sealed\n ");return NON_SEALED;}
provides {printf("provides\n");return PROVIDES;}
to {printf("to\n");return TO;}
with {printf("with\n");return WITH;}
open {printf("open\n");return OPEN;} 
record {printf("record\n");return RECORD;}
transitive {printf("transitive\n");return TRANSITIVE;}
yield {printf("yield\n");return YIELD;}
{INTEGER}|{DECIMAL}|{FLOATING}|{BOOL}|{NULL}|{STRING} {printf("%s",yytext);return LITERAL;}

"(" {printf("LB\n");return LB;}
")" {printf("RB\n");return RB;}
"{" {printf("Curly_L\n");return Curly_L;}
"}" {printf("Curly_R\n");return Curly_R;}
"[" {printf("LB\n");return SQ_L;}
"]" {printf("SQ_L\n");return SQ_R;}
";" {printf("SM_COLON\n");return SM_COLON;}
"," {printf("COMMA\n");return COMMA;}
"." {printf("DOT\n");return DOT;}
"..." {printf("TRIPLE_DOT\n");return TRIPLE_DOT;}
"@" {printf("AT\n");return AT;}
"::" {printf("DOUBLE_COLON\n");return DOUBLE_COLON;}

"=" {printf("EQ\n");return EQ;}
">" {printf("GT\n");return GT;}
"<"  {printf("LT\n");return LT;}
"!" {printf("NOT\n");return NOT;}
"~" {printf("Tilde\n");return Tilde;}
"?" {printf("QM\n");return QM;}
":" {printf("COLON\n");return COLON;}
"->" {printf("ARROW\n");return ARROW;}
"==" {printf("DOUBLE_EQ\n");return DOUBLE_EQ;}
">=" {printf("GEQ\n");return GEQ;}
"<=" {printf("LEQ\n");return LEQ;}
"!=" {printf("NOTEQ\n");return NOTEQ;}
"&&" {printf("AND\n");return AND;}
"||" {printf("OR\n");return OR;}
"++" {printf("INC\n");return INC;}
"--" {printf("DECINC\n");return DECINC;}
"+" {printf("PLUS\n");return PLUS;}
"-" {printf("MINUS\n");return MINUS;}
"*" {printf("MULTI\n");return MULTI;}
"/" {printf("DIV\n");return DIV;}
"&" {printf("BITAND\n");return BITAND;}
" \|" {printf("BITOR\n");return BITOR;}
"^" {printf("XOR\n");return XOR;}
"%" {printf("MOD\n");return MOD;}
"<<" {printf("SHIFT_L\n");return SHIFT_L;}
">>"  {printf("SHIFT_R\n");return SHIFT_R;}
">>>" {printf("TRIPLE_SHIFT\n");return TRIPLE_SHIFT;}
"+=" {printf("PLUS_EQ\n");return PLUS_EQ;}
"-=" {printf("SHIFT_L\n");return MINUS_EQ;}
"*=" {printf("MINUS_EQ\n");return MULTI_EQ;}
"/=" {printf("DIV_EQ\n");return DIV_EQ;}
"&=" {printf("AND_EQ\n");return AND_EQ;}
"|=" {printf("OR_EQ\n");return OR_EQ;}
"^=" {printf("XOR_EQ\n");return XOR_EQ;}
"%=" {printf("MOD_EQ\n");return MOD_EQ;}
"<<=" {printf("SHIFT_L_EQ\n");return SHIFT_L_EQ;}
">>=" {printf("SHIFT_R_EQ\n");return SHIFT_R_EQ;}
">>>="  {printf("TRIPLE_SHIFT_EQ\n");return TRIPLE_SHIFT_EQ;}

{ID} {printf("%s\n",yytext);return IDENTIFIER;}

. {yyerror("ERROR : %d\n");}
%%