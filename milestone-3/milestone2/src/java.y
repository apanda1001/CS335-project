%expect 12
%{
#include <iostream>
	#include <fstream>
    #include <string>
    #include <vector>
    #include <queue>
	#include <stack>
	#include <sys/stat.h>
	#include <sys/types.h>
	#include <cstring>
	using namespace std;
	extern int yylex();
	extern char* yytext;
	extern int yylineno;
	extern FILE* yyin;
	fstream fout, lexout;
    struct nodeptr{
        string Lexeme;
        string Token;
        string nonTerm;
		string Type;
		int intval;
		short srtval;
		char byteval;
		char chval;
		bool boolval;
		long longval;
		float fltval;
		double dblval;
		string strval;
        int n;
		string code;
        vector<nodeptr*> children;
    };
	struct entry{
		string token;
		string lexeme;
		string type;
		int lineno;
		int size;
		int is_method;
		vector<string>param_types;
		int is_mat;
		vector<int>matdims;
	};
	struct stable{
		string Name;
		vector<entry>table;
		vector<stable*>children;
		stable* parent;
	}GlobalSTable;
	struct Three_AC{
		string OPERATOR;
		string operand1;
		string operand2;
		string result;
		bool isConditional;
		bool isLabelled;
		string label;
	};
	stable* curtable = &GlobalSTable;
    nodeptr* root;
    string lex;
    int num = 1;
	string lookup(stable* , string);
	string Qlookup(nodeptr*, nodeptr*);
	void variable_check(nodeptr*);
	void tabname(stable*,string);
	void yyerror(const char*);
	void yyinfo(char*);
    nodeptr* NODE(string,string);
    nodeptr* NON_TERMINAL(string);
	void pushentry(nodeptr*,nodeptr*,int);
	void pushparams(nodeptr*,nodeptr*,int);
	stable* symtablegen(nodeptr*);
	void typecheck(nodeptr*&, nodeptr*&, string);
	void arrdims(nodeptr*&, nodeptr*&);
	int typeupdt(string&, string&);
	vector<Three_AC> Quadraple;
	string NewTemp();
	string WhileNewLabel();
	string ForNewLabel();
	string NewLabel();
	int i=0;
	int j=0;
	int w=0;
	int f=0;
	fstream Three_ac_code;
%}

%union{
	struct nodeptr* node;
}
%token <node> CONST;
%token <node> GOTO;
%token <node> UNDERSCORE;
%token <node> COMMENT;
%token <node> IDENTIFIER;
%token <node> STRLIT;
%token <node> BOOLLIT;
%token <node> NUMLIT;
%token <node> CHARLIT;
%token <node> NULLLIT;
%token <node> BOOLEAN;
%token <node> INT;
%token <node> BYTE;
%token <node> SHORT;
%token <node> LONG;
%token <node> CHAR;
%token <node> FLOAT;
%token <node> DOUBLE;
%token <node> SQ_L;
%token <node> SQ_R;
%token <node> DOT;
%token <node> EXTENDS;
%token <node> BITAND;
%token <node> LT;
%token <node> GT;
%token <node> COMMA;
%token <node> QM;
%token <node> SUPER;
%token <node> SM_COLON;
%token <node> PACKAGE;
%token <node> IMPORT;
%token <node> MULTI;
%token <node> STATIC;
%token <node> OPEN;
%token <node> MODULE;
%token <node> Curly_L;
%token <node> Curly_R;
%token <node> REQUIRES;
%token <node> SUSPEND;
%token <node> EXPORTS;
%token <node> TO;
%token <node> OPENS;
%token <node> USES;
%token <node> PROVIDES;
%token <node> WITH;
%token <node> TRANSITIVE;
%token <node> CLASS;
%token <node> PUBLIC;
%token <node> PROTECTED;
%token <node> PRIVATE;
%token <node> ABSTRACT;
%token <node> FINAL;
%token <node> SEALED;
%token <node> NON_SEALED;
%token <node> STRICTFP;
%token <node> IMPLEMENTS;
%token <node> PERMITS;
%token <node> TRANSIENT;
%token <node> VOLATILE;
%token <node> EQ;
%token <node> SYNCHRONIZED;
%token <node> NATIVE;
%token <node> VOID;
%token <node> LB;
%token <node> RB;
%token <node> TRIPLE_DOT;
%token <node> THIS;
%token <node> THROWS;
%token <node> ENUM;
%token <node> RECORD;
%token <node> INTERFACE;
%token <node> DEFAULT;
%token <node> AT;
%token <node> VAR;
%token <node> COLON;
%token <node> IF;
%token <node> ELSE;
%token <node> ASSERT;
%token <node> SWITCH;
%token <node> ARROW;
%token <node> CASE;
%token <node> WHILE;
%token <node> DO;
%token <node> FOR;
%token <node> BREAK;
%token <node> YIELD;
%token <node> CONTINUE;
%token <node> RETURN;
%token <node> THROW;
%token <node> TRY;
%token <node> CATCH;
%token <node> BITOR;
%token <node> FINALLY;
%token <node> NEW;
%token <node> DIAMOND;
%token <node> DOUBLE_COLON;
%token <node> MULTI_EQ;
%token <node> DIV_EQ;
%token <node> MOD_EQ;
%token <node> PLUS_EQ;
%token <node> MINUS_EQ;
%token <node> SHIFT_L_EQ;
%token <node> SHIFT_R_EQ;
%token <node> TRIPLE_SHIFT_EQ;
%token <node> AND_EQ;
%token <node> XOR_EQ;
%token <node> OR_EQ;
%token <node> OR;
%token <node> AND;
%token <node> XOR;
%token <node> DOUBLE_EQ;
%token <node>  NOTEQ;
%token <node> LEQ;
%token <node> GEQ;
%token <node> INSTANCEOF;
%token <node> SHIFT_L;
%token <node> SHIFT_R;
%token <node> TRIPLE_SHIFT;
%token <node> PLUS;
%token <node> MINUS;
%token <node> DIV;
%token <node> MOD;
%token <node> INC;
%token <node> DECINC;
%token <node> Tilde;
%token <node> NOT;
%right IF ELSE;
%start CompilationUnit;

%type <node> Literal
%type <node> Class 
%type <node> For
%type <node> Identifier
%type <node> Type 
%type <node> PrimitiveType 
%type <node> NumericType 
%type <node> IntegralType
%type <node> FloatingPointType 
%type <node> ReferenceType 
%type <node> ClassOrInterfaceType
%type <node> ClassType 
%type <node> InterfaceType 
%type <node> ArrayType
%type <node> Name
%type <node> SimpleName
%type <node> QualifiedName
%type <node> CompilationUnit
%type <node> ImportDeclarations
%type <node> TypeDeclarations
%type <node> PackageDeclaration
%type <node> ImportDeclaration
%type <node> SingleTypeImportDeclaration 
%type <node> TypeImportOnDemandDeclaration
%type <node> TypeDeclaration 
%type <node> Modifiers
%type <node> Modifier
%type <node> ClassDeclaration
%type <node> Super
%type <node> Interfaces
%type <node> InterfaceTypeList
%type <node> ClassBody
%type <node> ClassBodyDeclarations
%type <node> ClassBodyDeclaration
%type <node> ClassMemberDeclaration
%type <node> FieldDeclaration VariableDeclarators
%type <node> VariableDeclarator
%type <node> VariableDeclaratorId
%type <node> VariableInitializer
%type <node> MethodDeclaration
%type <node> MethodHeader
%type <node> MethodDeclarator
%type <node> FormalParameterList
%type <node> FormalParameter
%type <node> Throws
%type <node> ClassTypeList
%type <node> MethodBody
%type <node> StaticInitializer
%type <node> ConstructorDeclaration
%type <node> ConstructorDeclarator
%type <node> ConstructorBody
%type <node> ExplicitConstructorInvocation
%type <node> InterfaceDeclaration
%type <node> ExtendsInterfaces
%type <node> InterfaceBody
%type <node> InterfaceMemberDeclarations
%type <node> InterfaceMemberDeclaration
%type <node> ConstantDeclaration
%type <node> AbstractMethodDeclaration
%type <node> ArrayInitializer
%type <node> VariableInitializers 
%type <node> Block
%type <node> BlockStatements
%type <node> BlockStatement
%type <node> LocalVariableDeclarationStatement
%type <node> LocalVariableDeclaration
%type <node> Statement
%type <node> StatementNoShortIf
%type <node> StatementWithoutTrailingSubstatement
%type <node> EmptyStatement
%type <node> LabeledStatement
%type <node> LabeledStatementNoShortIf
%type <node> ExpressionStatement
%type <node> StatementExpression
%type <node> IfThenStatement
%type <node> IfThenElseStatement
%type <node> IfThenElseStatementNoShortIf
%type <node> WhileStatement
%type <node> WhileStatementNoShortIf
%type <node> DoStatement
%type <node> ForStatement
%type <node> ForStatementNoShortIf
%type <node> ForInit
%type <node> ForUpdate
%type <node> StatementExpressionList
%type <node> BreakStatement
%type <node> ContinueStatement
%type <node> ReturnStatement
%type <node> ThrowStatement
%type <node> SynchronizedStatement
%type <node> TryStatement
%type <node> Catches
%type <node> CatchClause
%type <node> Finally
%type <node> Primary
%type <node> PrimaryNoNewArray
%type <node> ClassInstanceCreationExpression
%type <node> ArgumentList
%type <node> ArrayCreationExpression
%type <node> Dims
/* %type <node> DimExprs
%type <node> DimExprsOpt
%type <node> DimExpr */
%type <node> FieldAccess
%type <node> MethodInvocation
%type <node> ArrayAccess
%type <node> PostFixExpression
%type <node> PostIncrementExpression
%type <node> PostDecrementExpression
%type <node> UnaryExpression
%type <node> PreIncrementExpression
%type <node> PreDecrementExpression
%type <node> UnaryExpressionNotPlusMinus
%type <node> CastExpression
%type <node> MultiplicativeExpression
%type <node> AdditiveExpression
%type <node> ShiftExpression
%type <node> RelationalExpression
%type <node> EqualityExpression
%type <node> AndExpression
%type <node> ExclusiveOrExpression
%type <node> InclusiveOrExpression
%type <node> ConditionalAndExpression
%type <node> ConditionalOrExpression
%type <node> ConditionalExpression
%type <node> AssignmentExpression
%type <node> Assignment
%type <node> LeftHandSide
%type <node> AssignmentOperator
%type <node> Expression
%type <node> PackageDeclarationOpt
%type <node> ImportDeclarationsOpt
%type <node> TypeDeclarationsOpt
%type <node> ModifiersOpt
%type <node> SuperOpt
%type <node> InterfacesOpt
%type <node> ClassBodyDeclarationsOpt
%type <node> ThrowsOpt
%type <node> FormalParameterListOpt
%type <node> IDENTOpt
%type <node> CatchesOpt
%type <node> ExplicitConstructorInvocationOpt
%type <node> BlockStatementsOpt
%type <node> ArgumentListOpt
%type <node> DimsOpt
%type <node> ExtendsInterfacesOpt
%type <node> InterfaceMemberDeclarationsOpt
%type <node> VariableInitializersOpt 
%type <node> COMMAOpt 
%type <node> ForInitOpt 
%type <node> ExpressionOpt
%type <node> ForUpdateOpt
%type <node> MethodInit
%type <node> If
%type <node> Else
%type <node> While
%type <node> Do
%type <node> ConstructorDeclaratorInit
%%
Literal: STRLIT        {   $$ = NON_TERMINAL("Literal"); lex = yytext;  $$->children.push_back(NODE("LITERAL",lex)); $$->Type = "String"; $$->strval = lex;}
	| NULLLIT			{   $$ = NON_TERMINAL("Literal"); lex = yytext;  $$->children.push_back(NODE("LITERAL",lex)); $$->Type = "NULL";}
	| NUMLIT			{   $$ = NON_TERMINAL("Literal"); lex = yytext;  $$->children.push_back(NODE("LITERAL",lex)); $$->Type = "byte";}
	| CHARLIT			{   $$ = NON_TERMINAL("Literal"); lex = yytext;  $$->children.push_back(NODE("LITERAL",lex)); $$->Type = "char";}
	|BOOLLIT			{   $$ = NON_TERMINAL("Literal"); lex = yytext;  $$->children.push_back(NODE("LITERAL",lex)); $$->Type = "boolean";}
		;
For : FOR				{ 	$$ = NON_TERMINAL("For");		$$->children.push_back(NODE("Keyword","for"));	stable *temp = new stable;	tabname(temp,"For");	curtable->children.push_back(temp);	temp->parent = curtable; 	 curtable = temp;	}
	;
Class: CLASS			{	$$ = NON_TERMINAL("Class");	$$->children.push_back(NODE("Keyword","class"));	stable *temp = new stable;	tabname(temp,"Class");	curtable->children.push_back(temp);	temp->parent = curtable; 	 curtable = temp;	}
	;
If: IF					{	$$ = NON_TERMINAL("If");	$$->children.push_back(NODE("Keyword","if"));	stable *temp = new stable;	tabname(temp,"If");	curtable->children.push_back(temp);	temp->parent = curtable;	 curtable = temp;	}
	;
Else: ELSE				{	$$ = NON_TERMINAL("Else");	$$->children.push_back(NODE("Keyword","else"));	if(curtable->parent) {curtable = curtable->parent; } 	stable *temp = new stable;	tabname(temp,"Else");	curtable->children.push_back(temp);	temp->parent = curtable; 	 curtable = temp;	}
	;
Identifier: IDENTIFIER  {   $$ = NON_TERMINAL("Identifier");  lex=yytext; $1 = NODE("Identifier",lex);   $$->children.push_back($1);		$1->Type = lookup(curtable,$1->Lexeme);	$$->Type = $1->Type;	cout << lex << "\n";}
    ;
Type: PrimitiveType     {   $$ = NON_TERMINAL("Type");  if($1)  $$->children.push_back($1); 	$$->Type = $1->Type;}
		| ReferenceType {   $$ = NON_TERMINAL("Type");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		; 
PrimitiveType: NumericType  {   $$ = NON_TERMINAL("PrimitiveType"); if($1)   $$->children.push_back($1);	$$->Type = $1->Type;}
		| BOOLEAN           {   $$ = NON_TERMINAL("PrimitiveType"); lex=yytext;   $1 = NODE("Keyword",lex);  $$->children.push_back($1);	$$->Type = "boolean";}
		; 
NumericType: IntegralType       {   $$ = NON_TERMINAL("NumericType");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| FloatingPointType     {   $$ = NON_TERMINAL("NumericType");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
IntegralType:BYTE       {   $$ = NON_TERMINAL("IntegralType");    lex=yytext;     $1 = NODE("Keyword",lex);    $$->children.push_back($1);	$$->Type = "byte";}
		| SHORT         {   $$ = NON_TERMINAL("IntegralType");    lex=yytext;     $1 = NODE("Keyword",lex);   $$->children.push_back($1);	$$->Type = "short";}
		| INT           {   $$ = NON_TERMINAL("IntegralType");    lex=yytext;     $1 = NODE("Keyword",lex);     $$->children.push_back($1);	$$->Type = "int";}
		| LONG          {   $$ = NON_TERMINAL("IntegralType");    lex=yytext;     $1 = NODE("Keyword",lex);    $$->children.push_back($1);	$$->Type = "long";}
		| CHAR          {   $$ = NON_TERMINAL("IntegralType");    lex=yytext;     $1 = NODE("Keyword",lex);    $$->children.push_back($1);	$$->Type = "char";}
		;
FloatingPointType: FLOAT    {   $$ = NON_TERMINAL("FloatingPointType");   lex=yytext; $1 = NODE("Keyword",lex);    $$->children.push_back($1);	$$->Type = "float";}
		| DOUBLE        {   $$ = NON_TERMINAL("FloatingPointType");   lex=yytext; $1 = NODE("Keyword",lex);    $$->children.push_back($1);	$$->Type = "double";}	
		;

ReferenceType:	  ClassOrInterfaceType  {   $$ = NON_TERMINAL("ReferenceType");   if($1) $$->children.push_back($1);	$$->Type = $1->Type;}
		| ArrayType                     {   $$ = NON_TERMINAL("ReferenceType");   if($1) $$->children.push_back($1);	$$->Type = $1->Type;}
		;

ClassOrInterfaceType: Name              {   $$ = NON_TERMINAL("ClassOrInterfaceType");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;

ClassType:	  ClassOrInterfaceType      {   $$ = NON_TERMINAL("ClassType");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;

InterfaceType:	  ClassOrInterfaceType  {   $$ = NON_TERMINAL("InterfaceType");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;

ArrayType:	  PrimitiveType SQ_L SQ_R  {   $$ = NON_TERMINAL("ArrayType");   $2 = NODE("Separator","[");    $3 = NODE("Separator","]");  if($1)  $$->children.push_back($1); $$->children.push_back($2); $$->children.push_back($3);	$$->Type = $1->Type;}
		| Name SQ_L SQ_R               {   $$ = NON_TERMINAL("ArrayType");   $2 = NODE("Separator","[");    $3 = NODE("Separator","]");   if($1) $$->children.push_back($1); $$->children.push_back($2); $$->children.push_back($3);	$$->Type = $1->Type;}
		| ArrayType SQ_L SQ_R          {   $$ = NON_TERMINAL("ArrayType");   $2 = NODE("Separator","[");    $3 = NODE("Separator","]");  if($1)  $$->children.push_back($1); $$->children.push_back($2); $$->children.push_back($3);	$$->Type = $1->Type;}
		;

Name:		  SimpleName        {   $$ = NON_TERMINAL("Name");  if($1)  $$->children.push_back($1);		$$->Type = $1->Type;}
		| QualifiedName         {   $$ = NON_TERMINAL("Name");  if($1)  $$->children.push_back($1);		$$->Type = $1->Type;}
		;

SimpleName:	  Identifier        {   $$ = NON_TERMINAL("SimpleName"); if($1)  $$->children.push_back($1);		$$->Type = $1->Type;}  
		;

QualifiedName:  THIS DOT Identifier	{   $$ = NON_TERMINAL("Name");     $$->children.push_back(NODE("Keyword","this"));    $2 = NODE("Separator",".");     $$->children.push_back($2); if($3) $$->children.push_back($3);		$$->Type = $3->Type;}
	| Name DOT Identifier   {   $$ = NON_TERMINAL("QualifiedName");  if($1)  $$->children.push_back($1);    $2 = NODE("Separator",".");     $$->children.push_back($2); if($3) $$->children.push_back($3);	$3->Type = Qlookup($1,$3);	$$->Type = $3->Type;}
		;

CompilationUnit:  PackageDeclarationOpt ImportDeclarationsOpt TypeDeclarationsOpt   {   $$ = NON_TERMINAL("CompilationUnit"); root = $$;  if($1)  $$->children.push_back($1); if($2) $$->children.push_back($2); if($3) $$->children.push_back($3);}
		;

PackageDeclarationOpt: PackageDeclaration   {   $$ = NON_TERMINAL("PackageDeclarationOpt");  if($1)  $$->children.push_back($1);}
    |                                       {   $$ = NULL;}
    ;

ImportDeclarationsOpt: ImportDeclarations   {   $$ = NON_TERMINAL("ImportDeclarationsOpt");   if($1) $$->children.push_back($1);}
    |                                       {   $$ = NULL;}
    ;

TypeDeclarationsOpt: TypeDeclarations       {   $$ = NON_TERMINAL("TypeDeclarationsOpt");  if($1)  $$->children.push_back($1);}
    |                                       {   $$ = NULL;}
    ;

ImportDeclarations: ImportDeclaration       {   $$ = NON_TERMINAL("ImportDeclarations"); if($1)   $$->children.push_back($1);}
		| ImportDeclarations ImportDeclaration  {   $$ = NON_TERMINAL("ImportDeclarations");  if($1)  $$->children.push_back($1); if($2)  $$->children.push_back($2);}
		;

TypeDeclarations: TypeDeclaration           {   $$ = NON_TERMINAL("TypeDeclarations");  if($1)  $$->children.push_back($1);}
		| TypeDeclarations TypeDeclaration  {   $$ = NON_TERMINAL("TypeDeclarations");  if($1)  $$->children.push_back($1);  if($2)   $$->children.push_back($2);}
		;

PackageDeclaration: PACKAGE Name SM_COLON     {   $$ = NON_TERMINAL("PackageDeclarations");    $$->children.push_back(NODE("Keyword","package")); if($2) $$->children.push_back($2); $$->children.push_back(NODE("Separator",";"));}
		;

ImportDeclaration: SingleTypeImportDeclaration  {   $$ = NON_TERMINAL("ImportDeclaration");  if($1)  $$->children.push_back($1);}
		| TypeImportOnDemandDeclaration         {   $$ = NON_TERMINAL("ImportDeclarations");  if($1)  $$->children.push_back($1);}
		;

SingleTypeImportDeclaration: IMPORT Name SM_COLON {   $$ = NON_TERMINAL("SingleTypeImportDeclaration");    $$->children.push_back(NODE("Keyword","import"));  if($2) $$->children.push_back($2); $$->children.push_back(NODE("Separator",";"));}
		;

TypeImportOnDemandDeclaration: IMPORT Name DOT MULTI SM_COLON   {   $$ = NON_TERMINAL("TypeImportOnDemandDeclaration");    $$->children.push_back(NODE("Keyword","import")); if($2)$$->children.push_back($2); $$->children.push_back(NODE("Separator",".")); $$->children.push_back(NODE("Operator","*")); $$->children.push_back(NODE("Separator",";"));}
		;

TypeDeclaration:  ClassDeclaration      {   $$ = NON_TERMINAL("TypeDeclaration");  if($1)  $$->children.push_back($1);}
		| InterfaceDeclaration          {   $$ = NON_TERMINAL("TypeDeclaration");   if($1) $$->children.push_back($1);}
		;

Modifiers: Modifier                 {   $$ = NON_TERMINAL("Modifiers");   if($1) $$->children.push_back($1);}
		| Modifiers Modifier        {   $$ = NON_TERMINAL("Modifiers");   if($1) $$->children.push_back($1); if($2) $$->children.push_back($2);}
		;

Modifier:	  PUBLIC        {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","public")); cout << "public\n";}
		| PROTECTED     {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","protected"));}
		| PRIVATE       {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","private"));	}
		| STATIC        {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","static"));}
		| ABSTRACT  {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","abstract"));}
		| FINAL {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","final"));		}
		| NATIVE    {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","native"));		}
		| SYNCHRONIZED  {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","synchronized"));}
		| TRANSIENT {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","transient"));		}
		| VOLATILE  {   $$ = NON_TERMINAL("Modifier");    $$->children.push_back(NODE("Keyword","volatile"));	}
		;

ClassDeclaration: ModifiersOpt Class Identifier SuperOpt InterfacesOpt ClassBody {   $$ = NON_TERMINAL("ClassDeclaration");    if($1)$$->children.push_back($1); if($2)	$$->children.push_back($2); if($3) $$->children.push_back($3);    if($4) $$->children.push_back($4);if($5) $$->children.push_back($5); if($6) $$->children.push_back($6); tabname(curtable, "Class_" + $3->children[0]->Lexeme);	if(curtable->parent) {curtable = curtable->parent; }   pushentry(NULL,$3,0);}
	;
ModifiersOpt: Modifiers {   $$ = NON_TERMINAL("ModifiersOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
SuperOpt:	  Super     {   $$ = NON_TERMINAL("SuperOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
InterfacesOpt:	  Interfaces {   $$ = NON_TERMINAL("InterfaceOpt");  if($1)  $$->children.push_back($1);}
    |   {	$$ = NULL; }
    ;
Super:		  EXTENDS ClassType {   $$ = NON_TERMINAL("Super");  $$->children.push_back(NODE("Keyword","extends")); if($2)  $$->children.push_back($2);}
	;
Interfaces:	  IMPLEMENTS InterfaceTypeList  {   $$ = NON_TERMINAL("Super");  $$->children.push_back(NODE("Keyword","implements")); if($2)  $$->children.push_back($2);}
		;
InterfaceTypeList: InterfaceType    {   $$ = NON_TERMINAL("InterfaceTypeList");  if($1)  $$->children.push_back($1);}
		| InterfaceTypeList COMMA InterfaceType {   $$ = NON_TERMINAL("InterfaceTypeList");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Separator",",")); if($3) $$->children.push_back($3);}
		;

ClassBody:	  Curly_L ClassBodyDeclarationsOpt Curly_R   {   $$ = NON_TERMINAL("ClassBody"); $$->children.push_back(NODE("Separator","{"));  if($2)  $$->children.push_back($2); $$->children.push_back(NODE("Separator","}"));}
		;

ClassBodyDeclarationsOpt: ClassBodyDeclarations {   $$ = NON_TERMINAL("ClassBodyDeclarationsOpt");  if($1)  $$->children.push_back($1);	}
    |   {	$$ = NULL; }
    ;

ClassBodyDeclarations: ClassBodyDeclaration     {   $$ = NON_TERMINAL("ClassBodyDeclarations");  if($1)  $$->children.push_back($1);}
		| ClassBodyDeclarations ClassBodyDeclaration    {   $$ = NON_TERMINAL("ClassBodyDeclarations");  if($1)  $$->children.push_back($1); if($2) $$->children.push_back($2);}
		;

ClassBodyDeclaration: ClassMemberDeclaration    {   $$ = NON_TERMINAL("ClassBodyDeclaration");  if($1)  $$->children.push_back($1);}
		| StaticInitializer     {   $$ = NON_TERMINAL("ClassBodyDeclaration");  if($1)  $$->children.push_back($1);}
		| ConstructorDeclaration        {   $$ = NON_TERMINAL("ClassBodyDeclaration");  if($1)  $$->children.push_back($1);}
		;

ClassMemberDeclaration: FieldDeclaration        {   $$ = NON_TERMINAL("ClassMemberDeclaration");  if($1)  $$->children.push_back($1);}
		| MethodDeclaration                     {   $$ = NON_TERMINAL("ClassMemberDeclaration");  if($1)  $$->children.push_back($1);}
		;

FieldDeclaration: ModifiersOpt Type VariableDeclarators SM_COLON  {   $$ = NON_TERMINAL("FieldDeclaration");  if($1)  $$->children.push_back($1);   if($2) $$->children.push_back($2);    if($3) $$->children.push_back($3);    $$->children.push_back(NODE("Separator",";")); pushentry($2,$3,-1);}
		;

VariableDeclarators: VariableDeclarator         {   $$ = NON_TERMINAL("VariableDeclarators");  if($1)  $$->children.push_back($1);}
		| VariableDeclarators COMMA VariableDeclarator  {$$ = NON_TERMINAL("VariableDeclarators");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Separator",",")); if($3) $$->children.push_back($3); }
		;

VariableDeclarator: VariableDeclaratorId        {   $$ = NON_TERMINAL("VariableDeclarator");  if($1)  $$->children.push_back($1);}
		| VariableDeclaratorId EQ VariableInitializer   {   $$ = NON_TERMINAL("VariableDeclarator");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Operator","="));  if($3) $$->children.push_back($3);}
		;

VariableDeclaratorId: Identifier    {   $$ = NON_TERMINAL("VariableDeclaratorId");  if($1)	$$->children.push_back($1);}
		| VariableDeclaratorId SQ_L SQ_R   {   $$ = NON_TERMINAL("VariableDeclaratorId");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator","["));  $$->children.push_back(NODE("Separator","]"));}
		;

VariableInitializer: Expression     {   $$ = NON_TERMINAL("VariableInitializer");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| ArrayInitializer          {   $$ = NON_TERMINAL("VariableInitializer");  if($1)  $$->children.push_back($1);}
		;

MethodDeclaration: MethodHeader MethodBody  {   $$ = NON_TERMINAL("MethodDeclaration");  if($1)  $$->children.push_back($1);      if($2) $$->children.push_back($2);	if(curtable->parent) {curtable = curtable->parent; } }
		;

MethodHeader: ModifiersOpt Type MethodDeclarator ThrowsOpt  {   $$ = NON_TERMINAL("MethodHeader");  if($1)  $$->children.push_back($1);   if($2) $$->children.push_back($2);    if($3) $$->children.push_back($3);    if($4) $$->children.push_back($4); pushparams($2,$3,0); }
		|  ModifiersOpt VOID MethodDeclarator ThrowsOpt     {   $$ = NON_TERMINAL("MethodHeader");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Keyword","void")); if($3) $$->children.push_back($3);    if($4) $$->children.push_back($4); pushparams(NULL,$3,0);}
		;
ThrowsOpt:	  Throws    {   $$ = NON_TERMINAL("ThrowsOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
FormalParameterListOpt: FormalParameterList     {   $$ = NON_TERMINAL("FormalParameterListOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; } 
    ;
MethodInit: Identifier LB	{ $$ = NON_TERMINAL("MethodInit");	if($1)	$$->children.push_back($1);	$$->children.push_back(NODE("Separator","("));	stable *temp = new stable; tabname(temp,"Method");		curtable->children.push_back(temp);	temp->parent = curtable;  curtable = temp; 	}
	;
MethodDeclarator: MethodInit FormalParameterListOpt RB    {   $$ = NON_TERMINAL("MethodDeclarator");    if($1)	$$->children.push_back($1);      if($2)  $$->children.push_back($2);    $$->children.push_back(NODE("Separator",")")); }
		| MethodDeclarator SQ_L SQ_R   {   $$ = NON_TERMINAL("MethodDeclarator");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator","["));  $$->children.push_back(NODE("Separator","]"));}
		;

FormalParameterList: FormalParameter        {   $$ = NON_TERMINAL("FormalParameterList");  if($1)  $$->children.push_back($1);}
		| FormalParameterList COMMA FormalParameter {   $$ = NON_TERMINAL("FormalParameterList");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Separator",",")); if($3) $$->children.push_back($3);}
		;

FormalParameter: ModifiersOpt Type VariableDeclaratorId {   $$ = NON_TERMINAL("FormalParameter");  if($1)  $$->children.push_back($1);    if($2) $$->children.push_back($2);    if($3) $$->children.push_back($3);	pushentry($2,$3,-1);}
		;


Throws: THROWS ClassTypeList    {   $$ = NON_TERMINAL("Throws");    $$->children.push_back(NODE("Keyword","throws"));  if($2)  $$->children.push_back($2);}
		;


ClassTypeList: ClassType        {   $$ = NON_TERMINAL("ClassTypeList");  if($1)  $$->children.push_back($1);}
		| ClassTypeList COMMA ClassType {   $$ = NON_TERMINAL("ClassTypeList");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Separator",",")); if($3) $$->children.push_back($3);}
		;

MethodBody: Block       {   $$ = NON_TERMINAL("MethodBody");  if($1)  $$->children.push_back($1);}
		;

StaticInitializer: STATIC Block     {   $$ = NON_TERMINAL("StaticInitializer");   $$->children.push_back(NODE("Keyword","static"));  if($2)  $$->children.push_back($2);}
		;

ConstructorDeclaration: ModifiersOpt ConstructorDeclarator ThrowsOpt ConstructorBody    {   $$ = NON_TERMINAL("ConstructorDeclaration");  if($1)  $$->children.push_back($1);     if($2) $$->children.push_back($2);  if($3) $$->children.push_back($3);    if($4) $$->children.push_back($4); pushparams(NULL,$2,1);	if(curtable->parent) {curtable = curtable->parent; } }
		;
ConstructorDeclaratorInit: SimpleName LB		{ $$ = NON_TERMINAL("ConstructorDeclaratorInit");	if($1)	$$->children.push_back($1);  $$->children.push_back(NODE("Separator","(")); stable *temp = new stable; tabname(temp,"Constructor");		curtable->children.push_back(temp);	temp->parent = curtable;  curtable = temp; 	}
	;
ConstructorDeclarator: ConstructorDeclaratorInit FormalParameterListOpt RB   {   $$ = NON_TERMINAL("ConstructorDeclarator");  if($1)  $$->children.push_back($1);   if($2) $$->children.push_back($2);  $$->children.push_back(NODE("Separator",")"));}
		;

ExplicitConstructorInvocationOpt: ExplicitConstructorInvocation {   $$ = NON_TERMINAL("ExplicitConstructorInvocationOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
BlockStatementsOpt: BlockStatements {   $$ = NON_TERMINAL("BlockStatementsOpt");  if($1)  $$->children.push_back($1);	}
    |       {	$$ = NULL; }
    ;
ArgumentListOpt:  ArgumentList {   $$ = NON_TERMINAL("ArgumentListOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
ConstructorBody: Curly_L ExplicitConstructorInvocationOpt BlockStatementsOpt Curly_R {   $$ = NON_TERMINAL("ConstructorBody"); $$->children.push_back(NODE("Separator","{"));  if($2) $$->children.push_back($2);    if($3) $$->children.push_back($3);   $$->children.push_back(NODE("Separator","}"));	}
		;

ExplicitConstructorInvocation: THIS LB ArgumentListOpt RB SM_COLON     {   $$ = NON_TERMINAL("ExplicitConstructorInvocation");   $$->children.push_back(NODE("Keyword","this"));    $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); $$->children.push_back(NODE("Separator",";"));}
		| SUPER LB ArgumentListOpt RB SM_COLON  {   $$ = NON_TERMINAL("ExplicitConstructorInvocation");   $$->children.push_back(NODE("Keyword","super"));    $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); $$->children.push_back(NODE("Separator",";"));}
		;

ExtendsInterfacesOpt: ExtendsInterfaces {   $$ = NON_TERMINAL("ExtendsInterfacesOpt");  if($1)  $$->children.push_back($1);}
    |   {	$$ = NULL; }
    ;
InterfaceDeclaration: ModifiersOpt INTERFACE Identifier ExtendsInterfacesOpt InterfaceBody    {   $$ = NON_TERMINAL("InterfaceDeclaration");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Keyword","interface"));  if($3) $$->children.push_back($3);    if($4) $$->children.push_back($4);    if($5) $$->children.push_back($5);}
		;

ExtendsInterfaces: EXTENDS InterfaceType    {   $$ = NON_TERMINAL("ExtendsInterfaces"); $$->children.push_back(NODE("Keyword","extends")); if($2)  $$->children.push_back($2);}
		| ExtendsInterfaces COMMA InterfaceType {   $$ = NON_TERMINAL("ExtendsInterfaces");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Separator",","));  if($3) $$->children.push_back($3);}
		;

InterfaceMemberDeclarationsOpt: InterfaceMemberDeclarations     {   $$ = NON_TERMINAL("InterfaceMemberDeclarationsOpt");  if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
InterfaceBody: Curly_L InterfaceMemberDeclarationsOpt Curly_R    {   $$ = NON_TERMINAL("InterfaceBody");   $$->children.push_back(NODE("Separator","{"));  if($2)  $$->children.push_back($2);    $$->children.push_back(NODE("Separator","}"));	}
		;

InterfaceMemberDeclarations: InterfaceMemberDeclaration     {   $$ = NON_TERMINAL("InterfaceMemberDeclarations");  if($1)  $$->children.push_back($1);}
		| InterfaceMemberDeclarations InterfaceMemberDeclaration    {   $$ = NON_TERMINAL("InterfaceMemberDeclarations");  if($1)  $$->children.push_back($1);    if($2) $$->children.push_back($2);}
		;

InterfaceMemberDeclaration: ConstantDeclaration {   $$ = NON_TERMINAL("InterfaceMemberDeclaration");  if($1)  $$->children.push_back($1);}
		| AbstractMethodDeclaration {   $$ = NON_TERMINAL("InterfaceMemberDeclaration");  if($1)  $$->children.push_back($1);}
		;

ConstantDeclaration: FieldDeclaration   {   $$ = NON_TERMINAL("ConstantDeclaration");  if($1)  $$->children.push_back($1);}
		;

AbstractMethodDeclaration: MethodHeader SM_COLON  {   $$ = NON_TERMINAL("AbstractMethodDeclaration");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Separator",";"));}
		;

VariableInitializersOpt: VariableInitializers   {   $$ = NON_TERMINAL("VariableInitializersOpt");  if($1)  $$->children.push_back($1);}
    |           {	$$ = NULL; }
    ;
COMMAOpt:	COMMA   {   $$ = NON_TERMINAL("COMMAOpt");  $$->children.push_back(NODE("Separator",","));}
    |           {	$$ = NULL; }
    ;
ArrayInitializer: Curly_L VariableInitializersOpt COMMAOpt Curly_R   {   $$ = NON_TERMINAL("ArrayInitializer"); $$->children.push_back(NODE("Separator","{")); if($2)  $$->children.push_back($2); if($3) $$->children.push_back($3); $$->children.push_back(NODE("Separator","}"));}
		;

VariableInitializers: VariableInitializer   {   $$ = NON_TERMINAL("VariableInitializers");  if($1)  $$->children.push_back($1);}
		| VariableInitializers COMMA VariableInitializer    {   $$ = NON_TERMINAL("VariableInitializers");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator",","));  if($3) $$->children.push_back($3);}
		;

Block:		  Curly_L BlockStatementsOpt Curly_R     {   $$ = NON_TERMINAL("Block");$$->children.push_back(NODE("Separator","{"));    if($2)  $$->children.push_back($2);   $$->children.push_back(NODE("Separator","}"));		}
		;

BlockStatements:  BlockStatement    {   $$ = NON_TERMINAL("BlockStatements");  if($1)  $$->children.push_back($1);}
		| BlockStatements BlockStatement    {   $$ = NON_TERMINAL("BlockStatements");  if($1)  $$->children.push_back($1);   if($2) $$->children.push_back($2);}
		;

BlockStatement:   LocalVariableDeclarationStatement {   $$ = NON_TERMINAL("BlockStatement");  if($1)  $$->children.push_back($1);}
		| Statement {   $$ = NON_TERMINAL("BlockStatement");  if($1)  $$->children.push_back($1);}
		;

LocalVariableDeclarationStatement: ModifiersOpt LocalVariableDeclaration SM_COLON     {   $$ = NON_TERMINAL("LocalVariableDeclarationStatement");  if($1)  $$->children.push_back($1);      if($2) $$->children.push_back($2);      $$->children.push_back(NODE("Separator",";"));}
		;

LocalVariableDeclaration: Type VariableDeclarators  {   $$ = NON_TERMINAL("LocalVariableDeclaration");  if($1)  $$->children.push_back($1);   if($2) $$->children.push_back($2); pushentry($1,$2,-1);}
		;

Statement:	  StatementWithoutTrailingSubstatement  {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1);}
		| LabeledStatement  {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1); }
		| IfThenStatement   {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1);}
		| IfThenElseStatement   {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1);}
		| WhileStatement    {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1);}
		| ForStatement  {   $$ = NON_TERMINAL("Statement");  if($1)  $$->children.push_back($1);}
		;

StatementNoShortIf: StatementWithoutTrailingSubstatement    {   $$ = NON_TERMINAL("StatementNoShortIf");  if($1)  $$->children.push_back($1);}
		| LabeledStatementNoShortIf {   $$ = NON_TERMINAL("StatementNoShortIf");  if($1)  $$->children.push_back($1);}
		| IfThenElseStatementNoShortIf  {   $$ = NON_TERMINAL("StatementNoShortIf");  if($1)  $$->children.push_back($1);}
		| WhileStatementNoShortIf   {   $$ = NON_TERMINAL("StatementNoShortIf");  if($1)  $$->children.push_back($1);}
		| ForStatementNoShortIf     {   $$ = NON_TERMINAL("StatementNoShortIf");  if($1)  $$->children.push_back($1);}
		;

StatementWithoutTrailingSubstatement: Block     {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| EmptyStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| ExpressionStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| DoStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| BreakStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| ContinueStatement {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| ReturnStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| SynchronizedStatement {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| ThrowStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		| TryStatement  {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  if($1)  $$->children.push_back($1);}
		;

EmptyStatement:	  SM_COLON        {   $$ = NON_TERMINAL("EmptyStatement");  $$->children.push_back(NODE("Separator",";"));}
		;

LabeledStatement: Identifier COLON Statement    {   $$ = NON_TERMINAL("LabeledStatement");    if($1)	$$->children.push_back($1); Three_ac_code << $1->children[0]->Lexeme << ": \n";    $$->children.push_back(NODE("Separator",":"));  if($3)  $$->children.push_back($3);}
		;

LabeledStatementNoShortIf: Identifier COLON StatementNoShortIf      {   $$ = NON_TERMINAL("LabeledStatementNoShortIf");    if($1)	$$->children.push_back($1);  Three_ac_code << $1->children[0]->Lexeme << ": \n";   $$->children.push_back(NODE("Separator",":"));  if($3)  $$->children.push_back($3);}
		;

ExpressionStatement: StatementExpression SM_COLON     {   $$ = NON_TERMINAL("ExpressionStatement");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Separator",";"));		variable_check($1);	$$->Type = $1->Type;}
		;

StatementExpression: Assignment     {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PreIncrementExpression    {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PreDecrementExpression    {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PostIncrementExpression   {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PostDecrementExpression   {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| MethodInvocation          {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| ClassInstanceCreationExpression   {   $$ = NON_TERMINAL("StatementExpression");  if($1)  $$->children.push_back($1);}
		;

IfThenStatement:  If LB Expression RB Statement  {   $$ = NON_TERMINAL("IfThenStatement"); $$->children.push_back(NODE("Keyword","if"));  $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); if($5) $$->children.push_back($5);		if(curtable->parent) {curtable = curtable->parent; } }
		;

IfThenElseStatement:  If LB Expression RB StatementNoShortIf Else Statement  {   $$ = NON_TERMINAL("IfThenStatement"); $$->children.push_back(NODE("Keyword","if"));  $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Keyword","else"));    if($7) $$->children.push_back($7);		if(curtable->parent) {curtable = curtable->parent; } }
		;

IfThenElseStatementNoShortIf:  If LB Expression RB StatementNoShortIf Else StatementNoShortIf    {   $$ = NON_TERMINAL("IfThenStatement"); $$->children.push_back(NODE("Keyword","if"));  $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Keyword","else"));    if($7) $$->children.push_back($7);	if(curtable->parent) {curtable = curtable->parent; } }
		;
While: WHILE	{	$$ = NON_TERMINAL("While");	$$->children.push_back(NODE("Keyword","while")); 	stable *temp = new stable;	tabname(temp,"While");	curtable->children.push_back(temp);	temp->parent = curtable; 	 curtable = temp;	}; 
WhileStatement:	  While LB Expression RB Statement   { Three_ac_code<<WhileNewLabel()<<": \n";   $$ = NON_TERMINAL("WhileStatement");  $$->children.push_back(NODE("Keyword","while"));   $$->children.push_back(NODE("Separator","("));  if($3){  $$->children.push_back($3);}    $$->children.push_back(NODE("Separator",")")); 
if($5) 
  $$->children.push_back($5);	
  if(curtable->parent) {curtable = curtable->parent; } }
		;

WhileStatementNoShortIf:  While LB Expression RB StatementNoShortIf  {   $$ = NON_TERMINAL("WhileStatement");  $$->children.push_back(NODE("Keyword","while"));   $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",")")); if($5) $$->children.push_back($5);	if(curtable->parent) {curtable = curtable->parent; } }
		;
Do: DO		{ $$ = NON_TERMINAL("Do"); $$->children.push_back(NODE("Keyword","do"));	stable *temp = new stable;	tabname(temp,"Do_While");	curtable->children.push_back(temp);	temp->parent = curtable;	 curtable = temp;	}
	;
DoStatement:	  Do Statement WHILE LB Expression RB SM_COLON     {   $$ = NON_TERMINAL("DoStatement"); if($1)	$$->children.push_back($1); if($2)  $$->children.push_back($2);    $$->children.push_back(NODE("Keyword","while"));   $$->children.push_back(NODE("Separator","("));  if($5) $$->children.push_back($5);   $$->children.push_back(NODE("Separator",")"));    $$->children.push_back(NODE("Separator",";"));		if(curtable->parent) {curtable = curtable->parent; } }
		;
ForInitOpt: ForInit     {   $$ = NON_TERMINAL("ForInitOpt");  if($1)  $$->children.push_back($1);}
    |                   {	$$ = NULL; }
    ;
ExpressionOpt: Expression   {   $$ = NON_TERMINAL("ExpressionOpt");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
    |                       {	$$ = NULL; }
    ;
ForUpdateOpt: ForUpdate     {   $$ = NON_TERMINAL("ForUpdateOpt");  if($1)  $$->children.push_back($1);}
    |                       {	$$ = NULL; }
    ;
ForStatement:	  For LB ForInitOpt SM_COLON ExpressionOpt SM_COLON ForUpdateOpt RB Statement    {   $$ = NON_TERMINAL("ForStatement"); Three_ac_code<<ForNewLabel()<< ": \n"; if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",";"));  if($5) $$->children.push_back($5);   $$->children.push_back(NODE("Separator",";"));  if($7) $$->children.push_back($7);   $$->children.push_back(NODE("Separator",")")); if($9) $$->children.push_back($9);	if(curtable->parent) {curtable = curtable->parent; } }
	;
ForStatementNoShortIf:	  For LB ForInitOpt SM_COLON ExpressionOpt SM_COLON ForUpdateOpt RB StatementNoShortIf       {   $$ = NON_TERMINAL("ForStatement"); if($1)    $$->children.push_back($1); $$->children.push_back(NODE("Separator","("));  if($3)  $$->children.push_back($3);    $$->children.push_back(NODE("Separator",";"));  if($5) $$->children.push_back($5);   $$->children.push_back(NODE("Separator",";"));  if($7) $$->children.push_back($7);   $$->children.push_back(NODE("Separator",")")); if($9) $$->children.push_back($9);	if(curtable->parent) {curtable = curtable->parent; } }
	;
ForInit: StatementExpressionList    {   $$ = NON_TERMINAL("ForInit");  if($1)  $$->children.push_back($1);}
		| LocalVariableDeclaration  {   $$ = NON_TERMINAL("ForInit");  if($1)  $$->children.push_back($1);}
		;
ForUpdate:	  StatementExpressionList   {   $$ = NON_TERMINAL("ForUpdate");  if($1)  $$->children.push_back($1);}
		;
StatementExpressionList: StatementExpression    {   $$ = NON_TERMINAL("StatementExpressionList");  if($1)  $$->children.push_back($1);	variable_check($$);	$$->Type = $1->Type;}
		| StatementExpressionList COMMA StatementExpression {   $$ = NON_TERMINAL("StatementExpressionList");  if($1)  $$->children.push_back($1);        $$->children.push_back(NODE("Separator",","));     if($3) $$->children.push_back($3);	variable_check($$);	$$->Type = $3->Type;	typecheck($1,$3,",");}
		;
IDENTOpt: Identifier    {   $$ = NON_TERMINAL("IDENTOpt");    if($1)	$$->children.push_back($1);		$$->Type = $1->Type;}
    |       {	$$ = NULL; }
    ;
BreakStatement:	  BREAK IDENTOpt SM_COLON {   $$ = NON_TERMINAL("BreakStatement");  $$->children.push_back(NODE("Keyword","break"));  if($2)  $$->children.push_back($2);  $$->children.push_back(NODE("Separator",";"));}
		;
ContinueStatement: CONTINUE IDENTOpt SM_COLON     {   $$ = NON_TERMINAL("ContinueStatement");   $$->children.push_back(NODE("Keyword","continue"));  if($2)  $$->children.push_back($2);  $$->children.push_back(NODE("Separator",";"));}
		;
ReturnStatement:  RETURN ExpressionOpt SM_COLON   {   $$ = NON_TERMINAL("ReturnStatement");   $$->children.push_back(NODE("Keyword","return"));  if($2)  $$->children.push_back($2);  $$->children.push_back(NODE("Separator",";"));}
    | SUSPEND ExpressionOpt SM_COLON      {   $$ = NON_TERMINAL("ReturnStatement");   $$->children.push_back(NODE("Keyword","suspend"));  if($2)  $$->children.push_back($2);  $$->children.push_back(NODE("Separator",";"));}
	;
ThrowStatement:  THROW Expression SM_COLON     {   $$ = NON_TERMINAL("ThrowStatement");   $$->children.push_back(NODE("Keyword","throw"));  if($2)  $$->children.push_back($2);  $$->children.push_back(NODE("Separator",";"));}
		;
SynchronizedStatement:  SYNCHRONIZED LB Expression RB Block  {   $$ = NON_TERMINAL("SynchronizedStatement");   $$->children.push_back(NODE("Keyword","synchronized"));    $$->children.push_back(NODE("Separator","(")); if($3)  $$->children.push_back($3);     $$->children.push_back(NODE("Separator",")"));     if($5) $$->children.push_back($5);}
		;
CatchesOpt: Catches     {   $$ = NON_TERMINAL("CatchesOpt");  if($1)  $$->children.push_back($1);}
    |                   {	$$ = NULL; }
    ;
TryStatement:	  TRY Block Catches     {   $$ = NON_TERMINAL("TryStatement");    $$->children.push_back(NODE("Keyword","try"));  if($2)  $$->children.push_back($2);    if($3) $$->children.push_back($3);}
		| TRY Block CatchesOpt Finally  {   $$ = NON_TERMINAL("TryStatement");    $$->children.push_back(NODE("Keyword","try"));  if($2)  $$->children.push_back($2);    if($3) $$->children.push_back($3);  if($4) $$->children.push_back($4);}
		;
Catches:	  CatchClause   {   $$ = NON_TERMINAL("Catches");  if($1)  $$->children.push_back($1);}
		| Catches CatchClause   {   $$ = NON_TERMINAL("Catches");  if($1)  $$->children.push_back($1);    if($2) $$->children.push_back($2);}
		;
CatchClause:	  CATCH LB FormalParameter RB Block      {   $$ = NON_TERMINAL("CatchClause");     $$->children.push_back(NODE("Keyword","catch"));   $$->children.push_back(NODE("Separator","(")); if($3)  $$->children.push_back($3); $$->children.push_back(NODE("Separator",")")); if($5) $$->children.push_back($5);}
	;
Finally:	  FINALLY Block     {   $$ = NON_TERMINAL("Finally"); $$->children.push_back(NODE("Keyword","finally"));  if($2)  $$->children.push_back($2);}
	;
Primary:	  PrimaryNoNewArray     {   $$ = NON_TERMINAL("Primary");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| ArrayCreationExpression   {   $$ = NON_TERMINAL("Primary");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
PrimaryNoNewArray: Literal          {   $$ = NON_TERMINAL("PrimaryNoNewArray");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
        | THIS                      {   $$ = NON_TERMINAL("PrimaryNoNewArray");  $$->children.push_back(NODE("Keyword","THIS"));}
		| LB Expression RB	{ $$ = NON_TERMINAL("PrimaryNoNewArray");	$$->children.push_back(NODE("Separator","(")); if($2)	$$->children.push_back($2);	$$->children.push_back(NODE("Separator",")"));	$$->Type = $2->Type;}
		| ClassInstanceCreationExpression   {   $$ = NON_TERMINAL("PrimaryNoNewArray");  if($1)  $$->children.push_back($1);}
		| FieldAccess               {   $$ = NON_TERMINAL("PrimaryNoNewArray");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| MethodInvocation          {   $$ = NON_TERMINAL("PrimaryNoNewArray");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| ArrayAccess               {   $$ = NON_TERMINAL("PrimaryNoNewArray");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
ClassInstanceCreationExpression: NEW ClassType LB ArgumentListOpt RB     {   $$ = NON_TERMINAL("ClassInstanceCreationExpression"); $$->children.push_back(NODE("Keyword","new"));  if($2)  $$->children.push_back($2);    $$->children.push_back(NODE("Separator","("));    if($4)  $$->children.push_back($4);  $$->children.push_back(NODE("Separator",")"));}
		;
ArgumentList:	  Expression        {   $$ = NON_TERMINAL("ArgumentList");  if($1)  $$->children.push_back($1);}
		| ArgumentList COMMA Expression     {   $$ = NON_TERMINAL("ArgumentList");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator",","));     if($3) $$->children.push_back($3);}
		;
DimsOpt: Dims       {   $$ = NON_TERMINAL("DimsOpt");  if($1)  $$->children.push_back($1);}
    |           {	$$ = NULL; }
    ;
ArrayCreationExpression: ArrayCreationExpression ArrayInitializer      {   $$ = NON_TERMINAL("ArrayCreationExpression");  if($1)  $$->children.push_back($1);        if($2) $$->children.push_back($2);		$$->Type = $1->Type;}
		| NEW PrimitiveType DimsOpt      {   $$ = NON_TERMINAL("ArrayCreationExpression"); $$->children.push_back(NODE("Keyword","new"));  if($2)  $$->children.push_back($2);    if($3) $$->children.push_back($3); 	$$->Type = $2->Type; arrdims($$,$3);}

Dims: SQ_L ExpressionOpt SQ_R        {   $$ = NON_TERMINAL("Dims");    $$->children.push_back(NODE("Separator","[")); if($2)	$$->children.push_back($2); $$->children.push_back(NODE("Separator","]"));  }
		| Dims SQ_L ExpressionOpt SQ_R        {   $$ = NON_TERMINAL("Dims"); if($1)	$$->children.push_back($1);   $$->children.push_back(NODE("Separator","[")); if($3)	$$->children.push_back($3); $$->children.push_back(NODE("Separator","]"));  }
		;
FieldAccess:	  Primary DOT Identifier    {   $$ = NON_TERMINAL("FieldAccess");  if($1)  $$->children.push_back($1);        $$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);		$$->Type = $3->Type;}
		| SUPER DOT Identifier              {   $$ = NON_TERMINAL("FieldAccess"); $$->children.push_back(NODE("Keyword","super"));   $$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);		$$->Type = $3->Type;}
		;
MethodInvocation: Name LB ArgumentListOpt RB     {   $$ = NON_TERMINAL("MethodInvocation");  if($1)  $$->children.push_back($1);       $$->children.push_back(NODE("Separator","(")); if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator",")"));	$$->Type = $1->Type;}
		| Primary DOT Identifier LB ArgumentListOpt RB   {   $$ = NON_TERMINAL("MethodInvocation");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);    $$->children.push_back(NODE("Separator","(")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Separator",")"));}
		| SUPER DOT Identifier LB ArgumentListOpt RB     {   $$ = NON_TERMINAL("MethodInvocation");   $$->children.push_back(NODE("Keyword","super"));$$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);    $$->children.push_back(NODE("Separator","(")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Separator",")"));}
		| Name Curly_L ArgumentListOpt Curly_R   {   $$ = NON_TERMINAL("MethodInvocation");  if($1)  $$->children.push_back($1);       $$->children.push_back(NODE("Separator","{")); if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator","}"));}
		| Primary DOT Identifier Curly_L ArgumentListOpt Curly_R {   $$ = NON_TERMINAL("MethodInvocation");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);    $$->children.push_back(NODE("Separator","{")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Separator","}"));}
		| SUPER DOT Identifier Curly_L ArgumentListOpt Curly_R        {   $$ = NON_TERMINAL("MethodInvocation");   $$->children.push_back(NODE("Keyword","super"));$$->children.push_back(NODE("Separator",".")); if($3)	$$->children.push_back($3);    $$->children.push_back(NODE("Separator","{")); if($5) $$->children.push_back($5);  $$->children.push_back(NODE("Separator","}"));}
		;
ArrayAccess:	  Name SQ_L Expression SQ_R        {   $$ = NON_TERMINAL("ArrayAccess");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Separator","["));  if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator","]"));	$$->Type = $1->Type.substr(0,($1->Type.size())-2);string t = "int"; typeupdt($3->Type,t);	if($3->Type!="int")	cerr << "Line: " << yylineno<< ": ERROR: Array Index Must Be of Type 'int'\n";}
		| PrimaryNoNewArray SQ_L Expression SQ_R   {   $$ = NON_TERMINAL("ArrayAccess");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Separator","["));  if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator","]"));	$$->Type = $1->Type.substr(0,($1->Type.size())-2); string t = "int"; typeupdt($3->Type,t);	if($3->Type!="int")	cerr << "Line: " << yylineno<< ": ERROR: Array Index Must Be of Type 'int'\n";}
		;
PostFixExpression: Primary              {   $$ = NON_TERMINAL("PostFixExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| Name                          {   $$ = NON_TERMINAL("PostFixExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PostIncrementExpression       {   $$ = NON_TERMINAL("PostFixExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PostDecrementExpression       {   $$ = NON_TERMINAL("PostFixExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
PostIncrementExpression: PostFixExpression INC  {   $$ = NON_TERMINAL("PostIncrementExpression");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Operator","++"));		$$->Type = $1->Type;}
		;
PostDecrementExpression: PostFixExpression DECINC  {   $$ = NON_TERMINAL("PostDecrementExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","--"));		$$->Type = $1->Type;}
		;
UnaryExpression:  PreIncrementExpression        {   $$ = NON_TERMINAL("UnaryExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PreDecrementExpression                {   $$ = NON_TERMINAL("UnaryExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| PLUS UnaryExpression                   {   $$ = NON_TERMINAL("UnaryExpression"); $$->children.push_back(NODE("Operator","+"));  if($2)  $$->children.push_back($2);		$$->Type = $2->Type;}
		| MINUS UnaryExpression                   {   $$ = NON_TERMINAL("UnaryExpression");     $$->children.push_back(NODE("Operator","-")); if($2)  $$->children.push_back($2);		$$->Type = $2->Type;}
		| UnaryExpressionNotPlusMinus           {   $$ = NON_TERMINAL("UnaryExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
PreIncrementExpression: INC UnaryExpression     {   $$ = NON_TERMINAL("PreIncrementExpression");  $$->children.push_back(NODE("Operator","++"));  if($2)  $$->children.push_back($2);	$$->Type = $2->Type;}
		;
PreDecrementExpression: DECINC UnaryExpression     {   $$ = NON_TERMINAL("PreDecrementExpression");  $$->children.push_back(NODE("Operator","--"));  if($2)  $$->children.push_back($2);	$$->Type = $2->Type;}
		;
UnaryExpressionNotPlusMinus: PostFixExpression  {   $$ = NON_TERMINAL("UnaryExpressionNotPlusMinus");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| Tilde UnaryExpression                 {   $$ = NON_TERMINAL("UnaryExpressionNotPlusMinus"); $$->children.push_back(NODE("Operator","~"));  if($2)  $$->children.push_back($1);	$$->Type = $2->Type;}
		| NOT UnaryExpression                   {   $$ = NON_TERMINAL("UnaryExpressionNotPlusMinus"); $$->children.push_back(NODE("Operator","!"));  if($2)  $$->children.push_back($1);	$$->Type = $2->Type;}
		| CastExpression                        {   $$ = NON_TERMINAL("UnaryExpressionNotPlusMinus");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		;
CastExpression:   LB PrimitiveType DimsOpt RB UnaryExpression {   $$ = NON_TERMINAL("CastExpression"); $$->children.push_back(NODE("Separator","("));  if($2)  $$->children.push_back($2);    if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator",")"));  if($5) $$->children.push_back($5);	}
		| LB Expression RB UnaryExpressionNotPlusMinus        {   $$ = NON_TERMINAL("CastExpression"); $$->children.push_back(NODE("Separator","("));  if($2)  $$->children.push_back($2);    $$->children.push_back(NODE("Separator",")"));    if($4) $$->children.push_back($4);	$$->Type = $2->Type;}
		| LB Name Dims RB UnaryExpressionNotPlusMinus         {   $$ = NON_TERMINAL("CastExpression"); $$->children.push_back(NODE("Separator","("));  if($2)  $$->children.push_back($2);    if($3) $$->children.push_back($3);  $$->children.push_back(NODE("Separator",")"));  if($5) $$->children.push_back($5);		$$->Type = $2->Type;}
		;
MultiplicativeExpression: UnaryExpression                   {   $$ = NON_TERMINAL("MultiplicativeExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| MultiplicativeExpression MULTI UnaryExpression      {   $$ = NON_TERMINAL("MultiplicativeExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","*"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"*");}
		| MultiplicativeExpression DIV UnaryExpression      {   $$ = NON_TERMINAL("MultiplicativeExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","/"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"/");}
		| MultiplicativeExpression MOD UnaryExpression      {   $$ = NON_TERMINAL("MultiplicativeExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","%"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"%");}
		;
AdditiveExpression: MultiplicativeExpression                {   $$ = NON_TERMINAL("AdditiveExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| AdditiveExpression PLUS MultiplicativeExpression   {   $$ = NON_TERMINAL("AdditiveExpression");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Operator","+"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"+");}
		| AdditiveExpression MINUS MultiplicativeExpression   {   $$ = NON_TERMINAL("AdditiveExpression");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Operator","-"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"-");}
		;
ShiftExpression:  AdditiveExpression                    {   $$ = NON_TERMINAL("ShiftExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| ShiftExpression SHIFT_L AdditiveExpression     {   $$ = NON_TERMINAL("ShiftExpression");  if($1)  $$->children.push_back($1);$$->children.push_back(NODE("Operator","<<"));   if($3) $$->children.push_back($3);	$$->Type = $3->Type;	typecheck($1,$3,"<<");}
		| ShiftExpression SHIFT_R AdditiveExpression     {   $$ = NON_TERMINAL("ShiftExpression");  if($1)  $$->children.push_back($1);$$->children.push_back(NODE("Operator",">>"));   if($3) $$->children.push_back($3);	$$->Type = $3->Type;	typecheck($1,$3,">>");}
		| ShiftExpression TRIPLE_SHIFT AdditiveExpression    {   $$ = NON_TERMINAL("ShiftExpression");  if($1)  $$->children.push_back($1);$$->children.push_back(NODE("Operator",">>>"));   if($3) $$->children.push_back($3);	$$->Type = $3->Type;	typecheck($1,$3,">>>");}
		;
RelationalExpression: ShiftExpression                       {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| RelationalExpression LT ShiftExpression           {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","<"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"<");}
		| RelationalExpression GT ShiftExpression           {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator",">"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,">");}
		| RelationalExpression LEQ ShiftExpression         {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","<="));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"<=");}
		| RelationalExpression GEQ ShiftExpression         {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator",">="));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,">=");}
		| RelationalExpression INSTANCEOF ReferenceType     {   $$ = NON_TERMINAL("RelationalExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Keyword","instanceof"));   if($3) $$->children.push_back($3);}
		;
EqualityExpression: RelationalExpression                {   $$ = NON_TERMINAL("EqualityExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| EqualityExpression DOUBLE_EQ RelationalExpression    {   $$ = NON_TERMINAL("EqualityExpression");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Operator","=="));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"==");
		}
		| EqualityExpression  NOTEQ RelationalExpression   {   $$ = NON_TERMINAL("EqualityExpression");  if($1)  $$->children.push_back($1); $$->children.push_back(NODE("Operator","!="));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"!=");}
		;
AndExpression: EqualityExpression                       {   $$ = NON_TERMINAL("AndExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| AndExpression BITAND EqualityExpression    {   $$ = NON_TERMINAL("AndExpression");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Operator","&"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"&");
		Three_ac_code<<$$->code<<"&"<<$3->code<<"\n";
		}
		;
ExclusiveOrExpression: AndExpression                    {   $$ = NON_TERMINAL("ExclusiveOrExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| ExclusiveOrExpression XOR AndExpression       {   $$ = NON_TERMINAL("ExclusiveOrExpression");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Operator","^"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"^");
		Three_ac_code<<$$->code<<"^"<<$3->code<<"\n";
		}
		;
InclusiveOrExpression: ExclusiveOrExpression                    {   $$ = NON_TERMINAL("InclusiveOrExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| InclusiveOrExpression BITOR ExclusiveOrExpression    {   $$ = NON_TERMINAL("InclusiveOrExpression");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Operator","|"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"|");
	    Three_ac_code<<$$->code<<"|"<<$3->code<<"\n";
		}
		;
ConditionalAndExpression: InclusiveOrExpression                 {   $$ = NON_TERMINAL("ConditionalAndExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;}
		| ConditionalAndExpression AND InclusiveOrExpression    {   $$ = NON_TERMINAL("ConditionalAndExpression");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Operator","&&"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"&&");
		Three_ac_code<<$$->code<<"||"<<$3->code<<"\n";
		}
		;
ConditionalOrExpression: ConditionalAndExpression               {   $$ = NON_TERMINAL("ConditionalOrExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;}
		| ConditionalOrExpression OR ConditionalAndExpression   {   $$ = NON_TERMINAL("ConditionalOrExpression");  if($1)  $$->children.push_back($1);    $$->children.push_back(NODE("Operator","||"));   if($3) $$->children.push_back($3);	$$->Type = $1->Type;	typecheck($1,$3,"||");
		Three_ac_code<<$$->code<<"||"<<$3->code<<"\n";
		}
		;
ConditionalExpression: ConditionalOrExpression                  {   $$ = NON_TERMINAL("ConditionalExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;
}
		| ConditionalOrExpression QM Expression COLON ConditionalExpression {   $$ = NON_TERMINAL("ConditionalExpression");  if($1)  $$->children.push_back($1);  $$->children.push_back(NODE("Operator","?"));   if($3) $$->children.push_back($3);    $$->children.push_back(NODE("Operator",":"));   if($5) $$->children.push_back($5);	typecheck($3,$5,":");	$$->Type = $3->Type;
		string label1,label2;
		label1=NewLabel();
        Three_ac_code<<"if "<<$1->code<<"==true"<<" goto "<<label1<<"\n";
		label2=NewLabel();
		Three_ac_code<<$5->code<<"\n";
	    Three_ac_code<<"goto "<<label2<<"\n";
		Three_ac_code<<label1<<" : "<<$3->code<<"\n";
		Three_ac_code<<label2<<" : "<<"\n";
		}
		;
AssignmentExpression: ConditionalExpression             {   $$ = NON_TERMINAL("AssignmentExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=$1->code;
}
		| Assignment                                    {   $$ = NON_TERMINAL("AssignmentExpression");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
		$$->code=$1->code;
}
		;
Assignment:	  LeftHandSide AssignmentOperator AssignmentExpression  {   $$ = NON_TERMINAL("Assignment");  if($1)  $$->children.push_back($1); if($2)  $$->children.push_back($2); if($3)  $$->children.push_back($3);	$$->Type = $1->Type; typecheck($1,$3,"=");
$$->code=NewTemp();
$3->code=NewTemp();
Three_ac_code<<$1->code<<$2->children[0]->Lexeme<<$3->code<<"\n";
}
		;
LeftHandSide:	  Name      {   $$ = NON_TERMINAL("LeftHandSide");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
$$->code=NewTemp();
}
		| FieldAccess       {   $$ = NON_TERMINAL("LeftHandSide");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
		$$->code=$1->code;
		}
		| ArrayAccess       {   $$ = NON_TERMINAL("LeftHandSide");  if($1)  $$->children.push_back($1);	$$->Type = $1->Type;
		$$->code=$1->code;}
		| ClassOrInterfaceType Name	{   $$ = NON_TERMINAL("LeftHandSide");  if($1)  $$->children.push_back($1);	if($2)  $$->children.push_back($2);	pushentry($1,$2,-1);	$$->Type = $2->Type;
		$$->code=$1->code;}
		;
AssignmentOperator: EQ  {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","="));}
	| MULTI_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","*="));}
	| DIV_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","/="));}
	| MOD_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","%="));}
	| PLUS_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","+="));}
	| MINUS_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","-="));}
	| SHIFT_L_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","<<="));}
	| SHIFT_R_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator",">>="));}
	| TRIPLE_SHIFT_EQ          {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator",">>>="));}
	| AND_EQ          {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","&="));}
	| XOR_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","^="));}
	| OR_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  $$->children.push_back(NODE("Operator","|="));}
	;
Expression:	  AssignmentExpression      {   $$ = NON_TERMINAL("Expression");  if($1)  $$->children.push_back($1);  variable_check($$);	$$->Type = $1->Type;
$$->code=$1->code;}
		;
%%
int main(int argc, char* argv[])
{
	yyin = fopen(argv[1],"r");
	struct stat sb;
	lexout.open("Lexemes.txt",ios::out);
	fout.open("output.dot",ios::out);
	Three_ac_code.open("three_Ac.txt",ios::out);
	fout << "digraph{\n";
	tabname(&GlobalSTable,"Global");
	GlobalSTable.parent = NULL;
	entry e = {"Identifier","println","int",1,0,1,{"String"},0,{}};
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("int");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("byte");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("short");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("boolean");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("long");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("float");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("double");
	GlobalSTable.table.push_back(e);
	e.param_types.pop_back();
	e.param_types.push_back("char");
	GlobalSTable.table.push_back(e);
	yyparse();
	fclose(yyin);
    queue<nodeptr*>q;
    q.push(root);
    nodeptr*t;
    while(!q.empty())
    {
        t = q.front();
        for(auto i:t->children)
        {
            fout << "node"<<t->n<<" -> "<<"node"<<i->n<<"\n";
            q.push(i);
        }
        if(t->nonTerm!="")
        {
            fout << "node" << t->n << " [ label=\"" << t->nonTerm << "\" ]\n";
        }
        else
        {
            if(t->Lexeme[0]=='\"')
            {
                t->Lexeme = "\\" + t->Lexeme;
                t->Lexeme.pop_back();
                t->Lexeme += "\\\"";
            }
            fout << "node" << t->n << " [ label=\"" << t->Lexeme << "(" << t->Token << ")" << "\" ]\n";
        }
        q.pop();
    }
	fout << "\n}";
	fout.close();
	queue<stable*>qt;
	qt.push(&GlobalSTable);
	stable*p;
	int tabno = 1;
	string fname(argv[1]);
	fname = fname.substr(0,fname.length()-5);
	fname+="/";
	if(stat(fname.c_str(),&sb))
		mkdir(fname.c_str(),0777);
	else{
		string temp = "rm "+fname+"*.csv";
		system(temp.c_str());
	}
	while(!qt.empty())
	{
		p = qt.front();
		for(auto i:p->children)
		{
			qt.push(i);
		}
		
		fout.open(fname+to_string(tabno)+"_"+p->Name+".csv",ios::out);
		fout << "Token,Lexeme,Type,Declared in Line,Is Method, Is Matrix\n";
		if(p->table.size())
		{
			for(auto i:p->table)
			{
				fout << i.token << "," << i.lexeme << "," << i.type << "," << i.lineno << "," ;
				if(i.is_method)
					fout << "YES,";
				else
					fout << "NO,";
				if(i.is_mat)
					fout << "YES,";
				else
					fout << "NO,";
				for(auto j:i.param_types)
				{
					fout << j << ",";
				}
				for(auto j:i.matdims)
				{
					fout << j << ",";
				}
				fout << "\n";
			}
		}
		fout << "\n\n";
		for(auto i:p->children)
		{
			fout << i->Name << ",";
		}
		tabno++;
		fout.close();
		qt.pop();
	}
	return 0;
}

void yyerror (const char *s) {
   fprintf (stderr, "%s in line %d in %s!\n", s, yylineno, yytext);
 }

void findchild(nodeptr* n, vector<string>&names, int &dims)
{
	for(auto i:n->children)
	{
		if(i->nonTerm!="" && i->nonTerm!="VariableInitializer")
			findchild(i,names,dims);
		else
		{
			if(i->Lexeme=="[")
				dims++;
			if(i->Token=="Identifier"){
				names.push_back(i->Lexeme);
			}
		}
	}
}
void pushentry(nodeptr* n1, nodeptr* n2, int code)
{
	entry e;
	int dims=0;
	e.is_mat = 0;
	e.is_method = 0;
	vector<string>names;
	if(n1!=NULL)
	{
		while(n1->nonTerm!="")
		{
			n1 = n1->children[0];
		}
		e.type = n1->Lexeme;
	}
	else
	{
		switch(code)
		{
			case 0: e.type = "User-Defined Class";
					break;
			case 1: e.type = "Method";
					e.is_method = 1;
					break;
		}
	}
	findchild(n2,names,dims);
	if(dims>0)
	{
		e.is_mat = 1;
		e.matdims = vector<int>(dims,0);
	}
	for(auto name:names)
	{
		e.token = "Identifier";
		e.lexeme = name;
		e.lineno = yylineno;
		if(curtable)
			curtable->table.push_back(e);
	}
}


nodeptr* NON_TERMINAL(string s)
{
    nodeptr* t = new nodeptr;
    t->nonTerm = s;
    t->n = num;
    num++;
    return t;
}

nodeptr* NODE(string tok, string l)
{
    nodeptr* t = new nodeptr;
    t->nonTerm = "";
    t->Token = tok;
    t->Lexeme = l;
    t->n = num;
    num++;
    return t;
}

void tabname(stable* tab, string name)
{
	tab->Name = name;
}

void help_param(nodeptr* n, vector<string>& l)
{
	if(n->children.size()==1)
	{
		nodeptr* t = n->children[0];
		for(auto i:t->children)
		{
			if(i->nonTerm=="Type")
			{
				t = i;
				break;
			}
		}
		int dim = 0;
		while(t->nonTerm!="")
		{
			if(t->nonTerm=="ArrayType")		dim++;
			t = t->children[0];
		}
		string val = t->Lexeme;
		while(dim>0)
		{
			val += "[]";
			dim--;
		}
		l.push_back(val);
		return;
	}
	help_param(n->children[0],l);
	if(n->children.size()==3)
		help_param(n->children[2],l);
}

void pushparams(nodeptr* n1, nodeptr* n2, int cns)
{
	entry e;
	e.is_mat = 0;
	e.is_method = 0;
	vector<string>params;
	if(n1!=NULL)
	{
		while(n1->nonTerm!="")
		{
			n1 = n1->children[0];
		}
		e.type = n1->Lexeme;
	}
	else
	{
		if(cns)
			e.type = "";
		else
			e.type = "void";

	}
	if(cns)
		e.lexeme = n2->children[0]->children[0]->children[0]->children[0]->Lexeme;
	else
		e.lexeme = n2->children[0]->children[0]->children[0]->Lexeme;
	if(n2->children[1]!=NULL)
	{
		help_param(n2->children[1]->children[0],e.param_types);
	}
	e.lineno = yylineno;
	e.is_method = 1;
	e.token = "Identifier";
	curtable->parent->table.push_back(e);
}

string lookup(stable* t, string id)
{
	if(t==NULL)
		return "";
	for(auto i:t->table)
	{
		if(i.lexeme==id)
		{
			string ans = i.type;
			for(auto j:i.matdims)
				ans += "[]";
			return ans;
		}
	}
	return lookup(t->parent,id);
}

void variable_check(nodeptr* n)
{
	string found = "";
	for(auto i:n->children)
	{
		if(i->Token=="Identifier")
		{
			found = lookup(curtable,i->Lexeme);
			if(found == "")
				cerr << "Line " << yylineno<< ": ERROR: Undeclared Variable '" << i->Lexeme << "'\n";
		}
		else if(i->nonTerm=="QualifiedName")
		{
				string ob = i->children[0]->children[0]->children[0]->children[0]->Lexeme;
				string id = i->children[2]->children[0]->Lexeme;
				string cls = lookup(curtable,ob);
				stable* t=NULL;
				for(auto j:GlobalSTable.children)
				{
					if(j->Name == "Class_"+cls)
					{
						t = j;
						break;
					}
				}
				if(t==NULL)
				{
					cerr << "Line " << yylineno<< ": ERROR: Undeclared Variable '" << id << "'\n";
				}
				else
				{
					string f = lookup(t,id);
					if(f == "")
						cerr << "Line " << yylineno<< ": ERROR: Undeclared Variable '" << id << "'\n";
					
				}
		}
		else
			variable_check(i);
	}
	 
}

string Qlookup(nodeptr* n1, nodeptr* n2)
{
	string ob,id,f;
	if(n1!=NULL)
	{
		while(n1->nonTerm!="")
		{
			n1 = n1->children[0];
		}
		ob = n1->Lexeme;
	}
	if(n2!=NULL)
	{
		while(n2->nonTerm!="")
		{
			n2 = n2->children[0];
		}
		id = n2->Lexeme;
	}
	string cls = lookup(curtable,ob);
	stable* t=NULL;
	for(auto j:GlobalSTable.children)
	{
		if(j->Name == "Class_"+cls)
		{
			t = j;
			break;
		}
	}
	if(t==NULL)
	{
		cerr << "Line " << yylineno<< ": ERROR: " << cls << "class has no attribute named '" << id << "'\n";
	}
	else
	{
		f = lookup(t,id);
		if(f == "")
				cerr << "Line " << yylineno<< ": ERROR: " << cls << "class has no attribute named '" << id << "'\n";
	}
	return f;
}
int typeupdt(string& s, string& t)
{
	int p = -1, q = -1, a = 0;
	if(s == "byte") p = 0;
	else if(s == "short") p = 1;
	else if(s == "int") p = 2;
	else if(s == "long") p = 3;
	else if(s == "float") p = 4;
	else if(s == "double") p = 5;
	else if(s == "String") p = 6;
	
	if(t == "byte") q = 0;
	else if(t == "short") q = 1;
	else if(t == "int") q = 2;
	else if(t == "long") q = 3;
	else if(t == "float") q = 4;
	else if(t == "double") q = 5;
	else if(t == "String") q = 6;
	
	if(p==-1 || q==-1)
	{
		return a;
	}
	if(q<p)
	{
		q = p;
		switch(q)
		{
			case 0: t = "byte";
			break;
			case 1: t = "short";
			break;
			case 2: t = "int";
			break;
			case 3: t = "long";
			break;
			case 4: t = "float";
			break;
			case 5: t = "double";
			break;
		}
		a = 2;
	}
	else
	{
		p = q;
		switch(q)
		{
			case 0: s = "byte";
			break;
			case 1: s = "short";
			break;
			case 2: s = "int";
			break;
			case 3: s = "long";
			break;
			case 4: s = "float";
			break;
			case 5: s = "double";
			break;
		}
		a = 1;
	}
	return a;
}
void typecheck(nodeptr*& p, nodeptr*& q, string op)
{
	if(p->Type!=q->Type)
	{
		string prmt1 = p->Type, prmt2=q->Type;
		int t1=0,t2=0,c;
		while(prmt1[prmt1.size()-1]==']')
		{
			prmt1 = prmt1.substr(0,prmt1.size()-2);
			t1++;
		}
		while(prmt2[prmt2.size()-1]==']')
		{
			prmt2 = prmt2.substr(0,prmt2.size()-2);
			t2++;
		}
		
		
		c = typeupdt(prmt1,prmt2);
		for(int i=0;i<t1;i++)
		{
			prmt1+="[]";
			prmt2+="[]";
		}
		p->Type = prmt1;
		q->Type = prmt2;
		if(c==0)
			cerr << "Line " << yylineno<< ": ERROR: " << "Type Mismatch between operands of " << op << "LHS: " << p->Type << ", RHS: " << q->Type << "\n" ;
	}
}
void helpdims(nodeptr*p, int &d)
{
	for(auto i:p->children)
	{
		if(i->nonTerm=="Dims")
		{
			d++;
			helpdims(i,d);
		}
	}
}
void arrdims(nodeptr* &p, nodeptr* &q)
{
	int d = 0;
	if(q!=NULL)
		helpdims(q,d);
	for(int i=0;i<d;i++)
	{
		p->Type += "[]";
	}
}
string NewTemp(){
   return "temp_" + to_string(++i);
}
string NewLabel(){
	return "label_" + to_string(++j);
}
string WhileNewLabel(){
	return "while_" + to_string(++w);
}
string ForNewLabel(){
	return "for_" + to_string(++f);
}