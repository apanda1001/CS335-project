%expect 7
%{
#include <iostream>
	#include <fstream>
    #include <string>
    #include <vector>
    #include <queue>
	#include <stack>
	#include <string>
	#include <typeinfo>
	#include "./classes/localTable.h" // the local table that contains other classes and their  headers
	#include "./classes/globalTable.h"
	using namespace std;
	extern int yylex();
	extern char* yytext;
	extern int yylineno;
	extern FILE* yyin;
	fstream fout;
	fstream fout1;
    nodeptr* root;
    int num = 1;
	int scope = 1; // fields: scope 0. method vars: scope 1. inside statements: scope++;
	nodeptr* NON_TERMINAL(string s)
	{
    nodeptr* t = new nodeptr;
    t->name = s;
    t->n = num;
    num++;
    return t;
	}
	nodeptr* NODE(string tok, string l)
	{
    nodeptr* t = new nodeptr;
    t->name = "";
    t->Token = tok;
    t->Lexeme = l;
    t->n = num;
	t->scope = scope;
    num++;
    return t;
	}
    string lex;
	void yyerror(const char*);
	void yyinfo(char*);
    nodeptr* NODE(string,string);
    nodeptr* NON_TERMINAL(string);
	// I am declaring some vectors that will store some useful nodepointers;
	vector<nodeptr*> classes;
	vector< vector<nodeptr*> > fields;
	vector< vector<nodeptr*> > classMembers;
	vector< vector<nodeptr*> > methods;



	string findType ( nodeptr* type ) {
		nodeptr* n = ( nodeptr* ) malloc( sizeof( nodeptr ) );
		n = type;
		if ( n->children[ 0 ]->name == "PrimitiveType" ) {
			n = n->children[ 0 ];
			if ( n->children[ 0 ]->name == "NumericType" ) {
				n = n->children[ 0 ]->children[ 0 ]->children[ 0 ];
				return n->Lexeme;
			} else {
				n = n->children[ 0 ];
				return n->Lexeme; // It's a BOOL
			}
		} else {
			n = n->children[ 0 ]->children[ 0 ];
			if ( n->name == "ArrayType" ) {
				n = n->children[ 0 ];
				if ( n->name == "PrimitiveType" ) {
					n = n->children[ 0 ];
					if ( n->name == "NumericType" ) {
						n = n->children[ 0 ]->children[ 0 ];
						return n->Lexeme;
					} else {
						n = n->children[ 0 ];
						return n->Lexeme; // It's a BOOL
					}
				} else return "MISC";
			}
		}
	}
	ID makeIdentifier( nodeptr* n ) {
		string argType, argID;
		nodeptr* aTemp = ( nodeptr* ) malloc( sizeof( nodeptr ) );
		aTemp = n;
		if ( aTemp->children.size() == 2 ) {
			nodeptr* aITemp = aTemp->children[ 1 ]->children[ 0 ]->children[ 0 ];
			argID = aITemp->Lexeme;
			aTemp = aTemp->children[ 0 ];
			argType = findType( aTemp );
		} else {
			nodeptr* aITemp = aTemp->children[ 2 ]->children[ 0 ]->children[ 0 ];
			argID = aITemp->Lexeme;
			aTemp = aTemp->children[ 1 ];
			argType = findType( aTemp );
		}
		ID id;
		vector<string> a;
		id.varPointer = n;
		id.lexeme = argID;
		id.type = argType;
		id.value = "";
		id.appearances = a;
		return id;
	}

	void getDeclarators( string type, nodeptr* n, map<int, ID> output ) {
		nodeptr* temp = ( nodeptr* )malloc( sizeof( nodeptr ) ); // nodeptr: variabledeclarators
		nodeptr* newTemp = ( nodeptr* )malloc( sizeof( nodeptr ) );
		nodeptr* travTemp = ( nodeptr* )malloc( sizeof( nodeptr ) );
		nodeptr* valTemp = ( nodeptr* )malloc( sizeof( nodeptr ) );
		string v;
		if ( temp->children.size() == 1 ) {
			ID id;
			temp = temp->children[ 0 ]; //curr-> vdec
			newTemp = temp;
			if ( temp->children.size() != 1 ) {
				valTemp = temp->children[ 2 ]; // vdec->vinit
				valTemp = valTemp->children[ 0 ]; // vinit->exp
				valTemp = valTemp->children[ 0 ]; // exp->assexp
				valTemp = valTemp->children[ 0 ]; // assexp->lit
				if ( valTemp->name == "Literal" ) {
					v = valTemp->children[ 0 ]->Lexeme;
				}


			}
			temp = temp->children[ 0 ]; //vdec-> vdecid
			temp = temp->children[ 0 ]; //vdecid-> id
			temp = temp->children[ 0 ]; //id-> ID
			id.lexeme = temp->Lexeme;
			id.type = type;
			id.value = v;
			id.varPointer = newTemp;
			output.insert( pair<int, ID>( id.getKey(), id ) );
		} else {
			while ( temp->children.size() > 1 ) {
				ID id;
				travTemp = temp->children[ 2 ]; //curr-> vdec
				newTemp = travTemp;
				travTemp = travTemp->children[ 0 ]; //vdec-> vdecid
				travTemp = travTemp->children[ 0 ]; //vdecid-> id
				travTemp = travTemp->children[ 0 ]; //id-> ID
				id.lexeme = travTemp->Lexeme;
				id.type = type;
				id.value = "";
				id.varPointer = newTemp;
				output.insert( pair<int, ID>( id.getKey(), id ) );
				temp = temp->children[ 0 ];
			}
			
		}
		return;
	}

	vector< map<int, Method> > getClassMethods ( vector< vector <nodeptr*> > methods ) {
		vector< map<int, Method> > classMethods;
		for ( int i = 0; i < methods.size(); i++ ) {
			map<int, Method> methodObjects;
			for ( int j = 0; j < methods[ i ].size(); j++ ) {
				string type, lexeme;
				string argType, argID;
				map<int, ID> args;
				nodeptr* lTemp = ( nodeptr* ) malloc( sizeof( nodeptr ) );
				nodeptr* tTemp = ( nodeptr* ) malloc( sizeof( nodeptr ) );
				nodeptr* aTemp = ( nodeptr* ) malloc( sizeof( nodeptr ) );
				lTemp = methods[ i ][ j ]->children[ 0 ];
				tTemp = methods[ i ][ j ]->children[ 0 ]; 
				aTemp = methods[ i ][ j ]->children[ 0 ];
				if ( tTemp->children[ 1 ]->name == "" ) {
					type = "VOID";
				} else {
					type = findType( tTemp->children[ 1 ] );
				}
				
				lTemp = lTemp->children[ 2 ]->children[ 0 ]->children[ 0 ]; // the lexeme
				lexeme = lTemp->Lexeme;
				aTemp = aTemp->children[ 2 ]->children[ 2 ]->children[ 0 ];
			
				ID id;
				id.lexeme = "";
				id.type = "";
				id.value = "";
				if ( aTemp->children.size() == 1 ) {
					id = makeIdentifier( aTemp->children[ 0 ] );
				} else {
					id = makeIdentifier( aTemp->children[ 1 ] );
					aTemp = aTemp->children[ 0 ];
				}
				args.insert( pair<int, ID>( id.getKey(), id ) );
				Method mee;
				mee.return_type = type;
				mee.lexeme = lexeme;
				mee.methodPointer = methods[ i ][ j ];
				mee.arguments = args;
				methodObjects.insert( pair<int, Method>( mee.getKey(), mee ) );
			}
			classMethods.push_back( methodObjects);
		}
		return classMethods;
	}

%}

/*
 * %union declares of what kinds of values appear on the value stack
 */
%union{
	struct nodeptr* node;
}

/*
 * each token is declared.  tokens store leaf values on the value stack
 *
 * Back in javalex.l, we put things on the stack by assigning to yylval
 "abstract"           { yylval.node = alcnode(ABSTRACT, 0); return ABSTRACT; }
 *
 */
%token <node> CONST;
%token <node> GOTO;
%token <node> UNDERSCORE;
%token <node> COMMENT;
%token <node> IDENTIFIER;
%token <node> LITERAL;
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
%token <node> NOTEQ;
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
/* %type <node> SwitchStatement SwitchBlock SwitchBlockStatementGroups SwitchBlockStatementGroup SwitchLabels SwitchLabel SwitchLabelsOpt*/
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
%%
Literal: LITERAL        {   $$ = NON_TERMINAL("Literal"); 
                            lex = yytext; 
                            $1 = NODE("LITERAL",lex); 
                            $$->children.push_back($1); 
                        };
Identifier: IDENTIFIER  {   $$ = NON_TERMINAL("Identifier"); 
                            lex=yytext; 
                            $1 = NODE("IDENTIFIER",lex);   
                            $$->children.push_back($1);
                        }
    ;
Type: PrimitiveType     {   $$ = NON_TERMINAL("Type");  
                            if($1)  $$->children.push_back($1);
                        }
		| ReferenceType {   $$ = NON_TERMINAL("Type");  
                            if($1)  $$->children.push_back($1);
                        }; 
PrimitiveType: NumericType  {   $$ = NON_TERMINAL("PrimitiveType"); 
                                if($1)   $$->children.push_back($1);
                                }
		| BOOLEAN           {   $$ = NON_TERMINAL("PrimitiveType");
                                lex=yytext;   
                                $1 = NODE("Keyword",lex);  
                                $$->children.push_back($1);
                            }; 
NumericType: IntegralType   {   $$ = NON_TERMINAL("NumericType"); 
                                if($1)  $$->children.push_back($1);
                                }
		| FloatingPointType {   $$ = NON_TERMINAL("NumericType");  
                                if($1)  $$->children.push_back($1);
                            };
IntegralType:BYTE       {   $$ = NON_TERMINAL("IntegralType");    
                            lex=yytext;    
                             $1 = NODE("Keyword",lex);    
                             $$->children.push_back($1);
                        }
		| SHORT         {   $$ = NON_TERMINAL("IntegralType");    
                            lex=yytext;     
                            $1 = NODE("Keyword",lex);   
                            $$->children.push_back($1);
                        }
		| INT           {   $$ = NON_TERMINAL("IntegralType");    
                            lex=yytext;     
                            $1 = NODE("Keyword",lex);     
                            $$->children.push_back($1);
                            }
		| LONG          {   $$ = NON_TERMINAL("IntegralType");    
                            lex=yytext;     
                            $1 = NODE("Keyword",lex);    
                            $$->children.push_back($1);
                            }
		| CHAR          {   $$ = NON_TERMINAL("IntegralType");    
                            lex=yytext;     
                            $1 = NODE("Keyword",lex);    
                            $$->children.push_back($1);
                            };
FloatingPointType: FLOAT    {   $$ = NON_TERMINAL("FloatingPointType");   
                                lex=yytext; 
                                $1 = NODE("Keyword",lex);    
                                $$->children.push_back($1);
                                }
		| DOUBLE        {   $$ = NON_TERMINAL("FloatingPointType");   
                            lex=yytext; $1 = NODE("Keyword",lex);    
                            $$->children.push_back($1);
                            };

ReferenceType:	  ClassOrInterfaceType  {   $$ = NON_TERMINAL("ReferenceType");   
                                            if($1) $$->children.push_back($1);
                                        }
		| ArrayType                     {   $$ = NON_TERMINAL("ReferenceType");   
                                        if($1) $$->children.push_back($1);
                                        };

ClassOrInterfaceType: Name {   $$ = NON_TERMINAL("ClassOrInterfaceType");  
                               if($1)  $$->children.push_back($1);
                            };

ClassType:	  ClassOrInterfaceType      {   $$ = NON_TERMINAL("ClassType");  
                                            if($1)  $$->children.push_back($1);
                                        };

InterfaceType:	  ClassOrInterfaceType  {   $$ = NON_TERMINAL("InterfaceType");  
                                            if($1)  $$->children.push_back($1);
                                        };

ArrayType:	  PrimitiveType SQ_L SQ_R  {   $$ = NON_TERMINAL("ArrayType");   
                                           $2 = NODE("Separator","[");    
                                           $3 = NODE("Separator","]");  
                                           if($1)  $$->children.push_back($1); 
                                           $$->children.push_back($2); 
                                           $$->children.push_back($3);
                                        }
		| Name SQ_L SQ_R               {   $$ = NON_TERMINAL("ArrayType");   
                                                $2 = NODE("Separator","[");    
                                                $3 = NODE("Separator","]");   
                                                if($1) $$->children.push_back($1); $$->children.push_back($2); 
                                                $$->children.push_back($3);
                                            }
		| ArrayType SQ_L SQ_R          {   $$ = NON_TERMINAL("ArrayType");   
                                                $2 = NODE("Separator","[");    
                                                $3 = NODE("Separator","]");  
                                                if($1)  $$->children.push_back($1); $$->children.push_back($2); 
                                                $$->children.push_back($3);
                                                }
		;

Name:		  SimpleName        {   $$ = NON_TERMINAL("Name");  
                                    if($1)  $$->children.push_back($1);
                                }
		| QualifiedName         {   $$ = NON_TERMINAL("Name");  
                                    if($1)  $$->children.push_back($1);};

SimpleName:	  Identifier        {   $$ = NON_TERMINAL("SimpleName"); 
                                    lex=yytext; 
                                    if($1)  $$->children.push_back($1);
                                };

QualifiedName:	  Name DOT Identifier   {   $$ = NON_TERMINAL("QualifiedName"); 
                                            if($1)  $$->children.push_back($1);    
                                            $2 = NODE("Separator",".");     
                                            $$->children.push_back($2); 
                                            if($3) 
                                            $$->children.push_back($3);
                                        };

CompilationUnit:  PackageDeclarationOpt ImportDeclarationsOpt TypeDeclarationsOpt   {   
                                           $$ = NON_TERMINAL("CompilationUnit"); 
                                           root = $$;  
                                           if($1)  $$->children.push_back($1); 
                                           if($2) $$->children.push_back($2); 
                                           if($3) $$->children.push_back($3);
                                           };

PackageDeclarationOpt: PackageDeclaration   {   $$ = NON_TERMINAL("PackageDeclarationOpt");  
                                                if($1)  $$->children.push_back($1);
                                            }
    |                                       {   $$ = NULL;};

ImportDeclarationsOpt: ImportDeclarations   {   $$ = NON_TERMINAL("ImportDeclarationsOpt");   
                                                if($1) $$->children.push_back($1);
                                            }
    |                                       {   $$ = NULL;};

TypeDeclarationsOpt: TypeDeclarations       {   $$ = NON_TERMINAL("TypeDeclarationsOpt");  
                                                if($1)  $$->children.push_back($1);
                                            }
    |                                       {   $$ = NULL;};

ImportDeclarations: ImportDeclaration       {   $$ = NON_TERMINAL("ImportDeclarations"); 
                                                if($1)   $$->children.push_back($1);
                                            }
		| ImportDeclarations ImportDeclaration  {   $$ = NON_TERMINAL("ImportDeclarations"); 
                                                    if($1)  $$->children.push_back($1); 
                                                    if($2)  $$->children.push_back($2);
                                                };

TypeDeclarations: TypeDeclaration           {   $$ = NON_TERMINAL("TypeDeclarations");  
                                                if($1)  $$->children.push_back($1);
                                            }
		| TypeDeclarations TypeDeclaration  {   $$ = NON_TERMINAL("TypeDeclarations");  
                                                if($1)  $$->children.push_back($1);  
                                                if($2)   $$->children.push_back($2);
                                            };

PackageDeclaration: PACKAGE Name SM_COLON     {   $$ = NON_TERMINAL("PackageDeclarations");    
                                              $$->children.push_back(NODE("Keyword","package")); 
                                            if($2) $$->children.push_back($2); 
											$$->children.push_back(NODE("Separator",";"));
											};

ImportDeclaration: SingleTypeImportDeclaration  {   $$ = NON_TERMINAL("ImportDeclaration");  													if($1)  $$->children.push_back($1);}
		| TypeImportOnDemandDeclaration         {   $$ = NON_TERMINAL("ImportDeclarations");  											  if($1)  $$->children.push_back($1);
												};

SingleTypeImportDeclaration: IMPORT Name SM_COLON {   $$ = NON_TERMINAL("SingleTypeImportDeclaration");    
$$->children.push_back(NODE("Keyword","import"));  
if($2) $$->children.push_back($2); 
$$->children.push_back(NODE("Separator",";"));};

TypeImportOnDemandDeclaration: IMPORT Name DOT MULTI SM_COLON   {   
	$$ = NON_TERMINAL("TypeImportOnDemandDeclaration");    
	$$->children.push_back(NODE("Keyword","import")); 
	if($2)$$->children.push_back($2); 
	$$->children.push_back(NODE("Separator",".")); 
	$$->children.push_back(NODE("Operator","*")); 
	$$->children.push_back(NODE("Separator",";"));
	};

TypeDeclaration:  ClassDeclaration      {   $$ = NON_TERMINAL("TypeDeclaration");  
                                            if($1)  $$->children.push_back($1);}
		| InterfaceDeclaration          {   $$ = NON_TERMINAL("TypeDeclaration");   
		                                    if($1) $$->children.push_back($1);
										};

Modifiers: Modifier                 {   $$ = NON_TERMINAL("Modifiers");   
                                        if($1) $$->children.push_back($1);}
		| Modifiers Modifier        {   $$ = NON_TERMINAL("");   
		                                if($1) $$->children.push_back($1); 
										if($2) $$->children.push_back($2);
									};

Modifier:	  PUBLIC        {   $$ = NON_TERMINAL("Modifier");    
                                $$->children.push_back(NODE("Keyword","public"));
							}
		| PROTECTED     {   $$ = NON_TERMINAL("Modifier");    
		                    $$->children.push_back(NODE("Keyword","protected"));}
		| PRIVATE       {   $$ = NON_TERMINAL("Modifier");    
		                    $$->children.push_back(NODE("Keyword","private"));
						}
		| STATIC        {   $$ = NON_TERMINAL("Modifier");    
		                    $$->children.push_back(NODE("Keyword","static"));
						}
		| ABSTRACT  {   $$ = NON_TERMINAL("Modifier");    
		                $$->children.push_back(NODE("Keyword","abstract"));
					}
		| FINAL {   $$ = NON_TERMINAL("Modifier");    
		            $$->children.push_back(NODE("Keyword","final"));
				}
		| NATIVE    {   $$ = NON_TERMINAL("Modifier");    
		                $$->children.push_back(NODE("Keyword","native"));
					}
		| SYNCHRONIZED  {   $$ = NON_TERMINAL("Modifier");    
		                    $$->children.push_back(NODE("Keyword","synchronized"));
					    }
		| TRANSIENT {   $$ = NON_TERMINAL("Modifier");    
		                $$->children.push_back(NODE("Keyword","transient"));
					}
		| VOLATILE  {   $$ = NON_TERMINAL("Modifier");    
		                $$->children.push_back(NODE("Keyword","volatile"));
					};

ClassDeclaration: ModifiersOpt CLASS Identifier SuperOpt InterfacesOpt ClassBody 
{   $$ = NON_TERMINAL("ClassDeclaration");    
    if($1)$$->children.push_back($1); 
	$$->children.push_back(NODE("Keyword","class")); 
	if($3) $$->children.push_back($3);    
	if($4) $$->children.push_back($4);
	if($5) $$->children.push_back($5); 
	if($6) $$->children.push_back($6);
};
ModifiersOpt: Modifiers {   $$ = NON_TERMINAL("ModifiersOpt");  
                            if($1)  $$->children.push_back($1);
						}
    |       {	$$ = NULL; }
    ;
SuperOpt:	  Super     {   $$ = NON_TERMINAL("SuperOpt");  
                            if($1)  $$->children.push_back($1);
						}
    |       {	$$ = NULL; };
InterfacesOpt:	  Interfaces {   $$ = NON_TERMINAL("InterfaceOpt");  
                                 if($1)  $$->children.push_back($1);
							}
    |   {	$$ = NULL; };
Super:		  EXTENDS ClassType {   $$ = NON_TERMINAL("Super");  
                                    $$->children.push_back(NODE("Keyword","extends")); 
									if($2)  $$->children.push_back($2);};
Interfaces:	  IMPLEMENTS InterfaceTypeList  {   $$ = NON_TERMINAL("Super");  
                                                $$->children.push_back(NODE("Keyword","implements")); 
												if($2)  $$->children.push_back($2);
											};
InterfaceTypeList: InterfaceType    {   $$ = NON_TERMINAL("InterfaceTypeList");  
                                        if($1)  $$->children.push_back($1);
									}
		| InterfaceTypeList COMMA InterfaceType {$$ = NON_TERMINAL("InterfaceTypeList");                                           if($1)  $$->children.push_back($1); 
		                                            $$->children.push_back(NODE("Separator",",")); 
		                                            if($3) $$->children.push_back($3);};

ClassBody:	  Curly_L ClassBodyDeclarationsOpt Curly_R   {   $$ = NON_TERMINAL("ClassBody");                                                           $$->children.push_back(NODE("Separator","{"));  
if($2)  $$->children.push_back($2); 
$$->children.push_back(NODE("Separator","}"));
};

ClassBodyDeclarationsOpt: ClassBodyDeclarations {   $$ = NON_TERMINAL("ClassBodyDeclarationsOpt");  
if($1)  $$->children.push_back($1);}
    |   {	$$ = NULL; };

ClassBodyDeclarations: ClassBodyDeclaration     {   $$ = NON_TERMINAL("ClassBodyDeclarations");  
if($1)  $$->children.push_back($1);}
| ClassBodyDeclarations ClassBodyDeclaration    
{   $$ = NON_TERMINAL("ClassBodyDeclarations");  
    if($1)  $$->children.push_back($1); 
	if($2) $$->children.push_back($2);
};

ClassBodyDeclaration: ClassMemberDeclaration    {   $$ = NON_TERMINAL("ClassBodyDeclaration");  
if($1)  $$->children.push_back($1);
}
		| StaticInitializer     {   $$ = NON_TERMINAL("ClassBodyDeclaration");  
		                            if($1)  $$->children.push_back($1);
								}
		| ConstructorDeclaration        {   $$ = NON_TERMINAL("ClassBodyDeclaration");  
		                                    if($1)  $$->children.push_back($1);
										};

ClassMemberDeclaration: FieldDeclaration        {   $$ = NON_TERMINAL("ClassMemberDeclaration");  
if($1)  $$->children.push_back($1);
}
		| MethodDeclaration    {   $$ = NON_TERMINAL("ClassMemberDeclaration");  
		                            if($1)  $$->children.push_back($1);
								};

FieldDeclaration: ModifiersOpt Type VariableDeclarators SM_COLON  {   
	$$ = NON_TERMINAL("FieldDeclaration");  
	if($1)  $$->children.push_back($1);   
	if($2) $$->children.push_back($2);    
	if($3) $$->children.push_back($3);    
	$$->children.push_back(NODE("Separator",";"));
	};

VariableDeclarators: VariableDeclarator         {   
	$$ = NON_TERMINAL("VariableDeclarators");  
	if($1)  $$->children.push_back($1);}
		| VariableDeclarators COMMA VariableDeclarator ;

VariableDeclarator: VariableDeclaratorId        {   
	$$ = NON_TERMINAL("VariableDeclarator");  
	if($1)  $$->children.push_back($1);}
		| VariableDeclaratorId EQ VariableInitializer   {   
			$$ = NON_TERMINAL("VariableDeclarator");  
			if($1)  $$->children.push_back($1); 
			$$->children.push_back(NODE("Operator","="));  if($3) $$->children.push_back($3);}
		;

VariableDeclaratorId: Identifier    {   $$ = NON_TERMINAL("VariableDeclaratorId");  if($1)	$$->children.push_back($1);}
		| VariableDeclaratorId SQ_L SQ_R   {   $$ = NON_TERMINAL("VariableDeclaratorId");  if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator","["));  $$->children.push_back(NODE("Separator","]"));}
		;

VariableInitializer: Expression     {   $$ = NON_TERMINAL("VariableInitializer");  if($1)  $$->children.push_back($1);}
		| ArrayInitializer          {   $$ = NON_TERMINAL("VariableInitializer");  if($1)  $$->children.push_back($1);}
		;

MethodDeclaration: MethodHeader MethodBody  {   
	$$ = NON_TERMINAL("MethodDeclaration");  
	if($1)  $$->children.push_back($1);      
	if($2) $$->children.push_back($2);
};

MethodHeader: ModifiersOpt Type MethodDeclarator ThrowsOpt  {   
	$$ = NON_TERMINAL("MethodHeader");  
	if($1)  $$->children.push_back($1);   
	if($2) $$->children.push_back($2);    
	if($3) $$->children.push_back($3);    
	if($4) $$->children.push_back($4);
}
		|  ModifiersOpt VOID MethodDeclarator ThrowsOpt     {   
			$$ = NON_TERMINAL("MethodHeader");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Keyword","void")); 
			if($3) $$->children.push_back($3);    
			if($4) $$->children.push_back($4);
};
ThrowsOpt:	  Throws    {   
	$$ = NON_TERMINAL("ThrowsOpt");  
	if($1)  $$->children.push_back($1);
	}
    |       {	$$ = NULL; };
FormalParameterListOpt: FormalParameterList     {   
	$$ = NON_TERMINAL("FormalParameterListOpt");  
	if($1)  $$->children.push_back($1);
	}
    |       {	$$ = NULL; } ;
MethodDeclarator: Identifier LB FormalParameterListOpt RB    {   
	$$ = NON_TERMINAL("MethodDeclarator");    
	if($1)	$$->children.push_back($1);    
	$$->children.push_back(NODE("Separator","("));  
	if($3)  $$->children.push_back($3);    
	$$->children.push_back(NODE("Separator",")"));}
		| MethodDeclarator SQ_L SQ_R   {   
			$$ = NON_TERMINAL("MethodDeclarator");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Separator","["));  
			$$->children.push_back(NODE("Separator","]"));
		};

FormalParameterList: FormalParameter        
{   $$ = NON_TERMINAL("FormalParameterList");  
    if($1)  $$->children.push_back($1);
	}
		| FormalParameterList COMMA FormalParameter {   
			$$ = NON_TERMINAL("FormalParameterList");  
			if($1)  $$->children.push_back($1);    
			$$->children.push_back(NODE("Separator",",")); 
			if($3) $$->children.push_back($3);
		};

FormalParameter: ModifiersOpt Type VariableDeclaratorId {   
	$$ = NON_TERMINAL("FormalParameter");  
	if($1)  $$->children.push_back($1);    
	if($2) $$->children.push_back($2);    
	if($3) $$->children.push_back($3);
};


Throws: THROWS ClassTypeList    {   $$ = NON_TERMINAL("Throws");    
                                    $$->children.push_back(NODE("Keyword","throws"));  
									if($2)  $$->children.push_back($2);
								};


ClassTypeList: ClassType        {   $$ = NON_TERMINAL("ClassTypeList");  
                                    if($1)  $$->children.push_back($1);
								}
		| ClassTypeList COMMA ClassType {   
			$$ = NON_TERMINAL("ClassTypeList");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Separator",",")); 
			if($3) $$->children.push_back($3);
		};

MethodBody: Block       {   $$ = NON_TERMINAL("MethodBody");  
                            if($1)  $$->children.push_back($1);
						};

StaticInitializer: STATIC Block     {   
	      $$ = NON_TERMINAL("StaticInitializer");   
		  $$->children.push_back(NODE("Keyword","static"));  
		  if($2)  $$->children.push_back($2);
		};

ConstructorDeclaration: ModifiersOpt ConstructorDeclarator ThrowsOpt ConstructorBody    {   $$ = NON_TERMINAL("ConstructorDeclaration");  
if($1)  $$->children.push_back($1);     
if($2) $$->children.push_back($2);  
if($3) $$->children.push_back($3);    
if($4) $$->children.push_back($4);
};

ConstructorDeclarator: SimpleName LB FormalParameterListOpt RB   {   
	$$ = NON_TERMINAL("ConstructorDeclarator");  
	if($1)  $$->children.push_back($1);  
	$$->children.push_back(NODE("Separator","(")); 
	if($3) $$->children.push_back($3);  
	$$->children.push_back(NODE("Separator",")"));
	};

ExplicitConstructorInvocationOpt: ExplicitConstructorInvocation {   
	$$ = NON_TERMINAL("ExplicitConstructorInvocationOpt");  
	if($1)  $$->children.push_back($1);
	}
    |       {	$$ = NULL; }
    ;
BlockStatementsOpt: BlockStatements {   $$ = NON_TERMINAL("BlockStatementsOpt");  
                                        if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
ArgumentListOpt:  ArgumentList {   $$ = NON_TERMINAL("ArgumentListOpt");  
                                   if($1)  $$->children.push_back($1);
								}
    |       {	$$ = NULL; };
ConstructorBody: Curly_L ExplicitConstructorInvocationOpt BlockStatementsOpt Curly_R {   
	                                $$ = NON_TERMINAL("ConstructorBody"); 
									$$->children.push_back(NODE("Separator","{"));  
									if($2) $$->children.push_back($2);    
									if($3) $$->children.push_back($3);   
									$$->children.push_back(NODE("Separator","}"));
									};

ExplicitConstructorInvocation: THIS LB ArgumentListOpt RB SM_COLON     {   
	$$ = NON_TERMINAL("ExplicitConstructorInvocation");   
	$$->children.push_back(NODE("Keyword","this"));    
	$$->children.push_back(NODE("Separator","("));  
	if($3)  $$->children.push_back($3);    
	$$->children.push_back(NODE("Separator",")")); 
	$$->children.push_back(NODE("Separator",";"));
	}
		| SUPER LB ArgumentListOpt RB SM_COLON  {   
			$$ = NON_TERMINAL("ExplicitConstructorInvocation");   
			$$->children.push_back(NODE("Keyword","super"));    
			$$->children.push_back(NODE("Separator","("));  
			if($3)  $$->children.push_back($3);    
			$$->children.push_back(NODE("Separator",")")); 
			$$->children.push_back(NODE("Separator",";"));
		};

ExtendsInterfacesOpt: ExtendsInterfaces {   
	$$ = NON_TERMINAL("ExtendsInterfacesOpt");  
	if($1)  $$->children.push_back($1);}
    |   {	$$ = NULL; };
InterfaceDeclaration: ModifiersOpt INTERFACE Identifier ExtendsInterfacesOpt InterfaceBody    {   $$ = NON_TERMINAL("InterfaceDeclaration");  
    if($1)  $$->children.push_back($1);  
	$$->children.push_back(NODE("Keyword","interface"));  
	if($3) $$->children.push_back($3);    
	if($4) $$->children.push_back($4);    
	if($5) $$->children.push_back($5);
	};

ExtendsInterfaces: EXTENDS InterfaceType    {   
	$$ = NON_TERMINAL("ExtendsInterfaces"); 
	$$->children.push_back(NODE("Keyword","extends")); 
	if($2)  $$->children.push_back($2);}
		| ExtendsInterfaces COMMA InterfaceType {   
			$$ = NON_TERMINAL("ExtendsInterfaces");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Separator",","));  
			if($3) $$->children.push_back($3);
		};

InterfaceMemberDeclarationsOpt: InterfaceMemberDeclarations     {   
	$$ = NON_TERMINAL("InterfaceMemberDeclarationsOpt");  
	if($1)  $$->children.push_back($1);}
    |       {	$$ = NULL; };
InterfaceBody: Curly_L InterfaceMemberDeclarationsOpt Curly_R    {   
	$$ = NON_TERMINAL("InterfaceBody");   
	$$->children.push_back(NODE("Separator","{"));  
	if($2)  $$->children.push_back($2);    
	$$->children.push_back(NODE("Separator","}"));
	};

InterfaceMemberDeclarations: InterfaceMemberDeclaration     {   
	$$ = NON_TERMINAL("InterfaceMemberDeclarations");  
	if($1)  $$->children.push_back($1);
	}
		| InterfaceMemberDeclarations InterfaceMemberDeclaration    {   
			$$ = NON_TERMINAL("InterfaceMemberDeclarations");  
			if($1)  $$->children.push_back($1);    
			if($2) $$->children.push_back($2);
		};

InterfaceMemberDeclaration: ConstantDeclaration {   
	$$ = NON_TERMINAL("InterfaceMemberDeclaration");  
	if($1)  $$->children.push_back($1);
	}
		| AbstractMethodDeclaration {   $$ = NON_TERMINAL("InterfaceMemberDeclaration");  
		                                if($1)  $$->children.push_back($1);
									};

ConstantDeclaration: FieldDeclaration   {   
	$$ = NON_TERMINAL("ConstantDeclaration");  
	if($1)  $$->children.push_back($1);
	};

AbstractMethodDeclaration: MethodHeader SM_COLON  {   
	$$ = NON_TERMINAL("AbstractMethodDeclaration");  
	if($1)  $$->children.push_back($1);  
	$$->children.push_back(NODE("Separator",";"));
	};

VariableInitializersOpt: VariableInitializers   {   
	$$ = NON_TERMINAL("VariableInitializersOpt");  
	if($1)  $$->children.push_back($1);
	}
    |           {	$$ = NULL; };
COMMAOpt:	COMMA   {   $$ = NON_TERMINAL("COMMAOpt");  
                        $$->children.push_back(NODE("Separator",","));
					}
    |           {	$$ = NULL; };
ArrayInitializer: Curly_L VariableInitializersOpt COMMAOpt Curly_R   {   
	$$ = NON_TERMINAL("ArrayInitializer"); 
	$$->children.push_back(NODE("Separator","{")); 
	if($2)  $$->children.push_back($2); 
	if($3) $$->children.push_back($3); 
	$$->children.push_back(NODE("Separator","}"));
	};

VariableInitializers: VariableInitializer   {   $$ = NON_TERMINAL("VariableInitializers");  
                                                if($1)  $$->children.push_back($1);
											}
		| VariableInitializers COMMA VariableInitializer    {   
			 $$ = NON_TERMINAL("VariableInitializers");  
			 if($1)  $$->children.push_back($1);   
			 $$->children.push_back(NODE("Separator",","));  
			 if($3) $$->children.push_back($3);
			};

Block:		  Curly_L BlockStatementsOpt Curly_R     {  
	$$ = NON_TERMINAL("Block");                                                      $$->children.push_back(NODE("Separator","{"));    
	if($2)  $$->children.push_back($2);   
	$$->children.push_back(NODE("Separator","}"));};

BlockStatements:  BlockStatement    {   $$ = NON_TERMINAL("BlockStatements");  
                                        if($1)  $$->children.push_back($1);}
		| BlockStatements BlockStatement    {   $$ = NON_TERMINAL("BlockStatements");  
		                                        if($1)  $$->children.push_back($1);   
												if($2) $$->children.push_back($2);
											};

BlockStatement:   LocalVariableDeclarationStatement {   $$ = NON_TERMINAL("BlockStatement"); if($1)  $$->children.push_back($1);}
		| Statement {   $$ = NON_TERMINAL("BlockStatement");  
		                if($1)  $$->children.push_back($1);};

LocalVariableDeclarationStatement: ModifiersOpt LocalVariableDeclaration SM_COLON     {   
	$$ = NON_TERMINAL("LocalVariableDeclarationStatement");  
	if($1)  $$->children.push_back($1);      
	if($2) $$->children.push_back($2);      
	$$->children.push_back(NODE("Separator",";"));};

LocalVariableDeclaration: Type VariableDeclarators  {   

	$$ = NON_TERMINAL("LocalVariableDeclaration"); 
	if($1)  $$->children.push_back($1);   
	if($2) $$->children.push_back($2);};

Statement:	  StatementWithoutTrailingSubstatement  {   
	$$ = NON_TERMINAL("");  
	if($1)  $$->children.push_back($1);
	}
		| LabeledStatement  {   $$ = NON_TERMINAL("Statement");  
		                        if($1)  $$->children.push_back($1);}
		| IfThenStatement   {   $$ = NON_TERMINAL("Statement");  
		                        if($1)  $$->children.push_back($1);}
		| IfThenElseStatement   {   $$ = NON_TERMINAL("Statement");  
		                            if($1)  $$->children.push_back($1);}
		| WhileStatement    {   $$ = NON_TERMINAL("Statement");  
		                        if($1)  $$->children.push_back($1);}
		| ForStatement  {   $$ = NON_TERMINAL("Statement");  
		                    if($1)  $$->children.push_back($1);};

StatementNoShortIf: StatementWithoutTrailingSubstatement    {   
	$$ = NON_TERMINAL("StatementNoShortIf");  
	if($1)  $$->children.push_back($1);
	}
		| LabeledStatementNoShortIf {   $$ = NON_TERMINAL("StatementNoShortIf");  
		                                if($1)  $$->children.push_back($1);
									}
		| IfThenElseStatementNoShortIf  {   $$ = NON_TERMINAL("StatementNoShortIf");  
		                                    if($1)  $$->children.push_back($1);}
		| WhileStatementNoShortIf   {   $$ = NON_TERMINAL("StatementNoShortIf");  
		                                if($1)  $$->children.push_back($1);}
		| ForStatementNoShortIf     {   $$ = NON_TERMINAL("StatementNoShortIf");  
		                                if($1)  $$->children.push_back($1);
									};

StatementWithoutTrailingSubstatement: Block     {   
	$$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
	if($1)  $$->children.push_back($1);
	}
		| EmptyStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                        if($1)  $$->children.push_back($1);}
		| ExpressionStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");                           if($1)  $$->children.push_back($1);
		}
		| DoStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                    if($1)  $$->children.push_back($1);
						}
		| BreakStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                        if($1)  $$->children.push_back($1);
							}
		| ContinueStatement {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                        if($1)  $$->children.push_back($1);
							}
		| ReturnStatement   {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                        if($1)  $$->children.push_back($1);
							}
		| SynchronizedStatement {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");                           if($1)  $$->children.push_back($1);
		}
		| ThrowStatement    {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                        if($1)  $$->children.push_back($1);
							}
		| TryStatement  {   $$ = NON_TERMINAL("StatementWithoutTrailingSubstatement");  
		                    if($1)  $$->children.push_back($1);
						};

EmptyStatement:	  SM_COLON        {   $$ = NON_TERMINAL("EmptyStatement");  
                                      $$->children.push_back(NODE("Separator",";"));
								};

LabeledStatement: Identifier COLON Statement    {   
	$$ = NON_TERMINAL("LabeledStatement");    
	if($1)	$$->children.push_back($1);    
	$$->children.push_back(NODE("Separator",":"));  
	if($3)  $$->children.push_back($3);
	};

LabeledStatementNoShortIf: Identifier COLON StatementNoShortIf      {   
	$$ = NON_TERMINAL("LabeledStatementNoShortIf");    
	if($1)	$$->children.push_back($1);    
	$$->children.push_back(NODE("Separator",":"));  
	if($3)  $$->children.push_back($3);
	};

ExpressionStatement: StatementExpression SM_COLON     {   
	$$ = NON_TERMINAL("ExpressionStatement");  
	if($1)  $$->children.push_back($1);    
	$$->children.push_back(NODE("Separator",";"));
	};

StatementExpression: Assignment     {   
	$$ = NON_TERMINAL("StatementExpression");  
	if($1)  $$->children.push_back($1);}
		| PreIncrementExpression    {   
			$$ = NON_TERMINAL("StatementExpression");  
			if($1)  $$->children.push_back($1);}
		| PreDecrementExpression    {   
			$$ = NON_TERMINAL("StatementExpression");  
			if($1)  $$->children.push_back($1);}
		| PostIncrementExpression   {   
			$$ = NON_TERMINAL("StatementExpression");  
			if($1)  $$->children.push_back($1);}
		| PostDecrementExpression   {   $$ = NON_TERMINAL("StatementExpression");  
		                                if($1)  $$->children.push_back($1);}
		| MethodInvocation          {   $$ = NON_TERMINAL("StatementExpression");  
		                                if($1)  $$->children.push_back($1);}
		| ClassInstanceCreationExpression   {   $$ = NON_TERMINAL("StatementExpression");  
		                                        if($1)  $$->children.push_back($1);};

IfThenStatement:  IF LB Expression RB Statement  {   $$ = NON_TERMINAL("IfThenStatement"); $$->children.push_back(NODE("Keyword","if"));  
$$->children.push_back(NODE("Separator","("));  
scope++;
if($3)  $$->children.push_back($3);    
$$->children.push_back(NODE("Separator",")")); 

if($5) $$->children.push_back($5); scope--;};

IfThenElseStatement:  IF LB Expression RB StatementNoShortIf ELSE Statement  {   
	$$ = NON_TERMINAL("IfThenStatement"); 
	$$->children.push_back(NODE("Keyword","if"));  
	$$->children.push_back(NODE("Separator","("));  
	scope++;
	if($3)  $$->children.push_back($3);    
	$$->children.push_back(NODE("Separator",")")); 
	if($5) $$->children.push_back($5);  
	$$->children.push_back(NODE("Keyword","else"));  
 
	if($7) $$->children.push_back($7);scope--;};
	
IfThenElseStatementNoShortIf:  IF LB Expression RB StatementNoShortIf ELSE StatementNoShortIf    {   $$ = NON_TERMINAL("IfThenStatement"); 
                          $$->children.push_back(NODE("Keyword","if"));  
						  $$->children.push_back(NODE("Separator","("));  
						  scope++;
						  if($3)  $$->children.push_back($3);    
						  $$->children.push_back(NODE("Separator",")")); 
						  if($5) $$->children.push_back($5);  
						  $$->children.push_back(NODE("Keyword","else"));    
						  if($7) $$->children.push_back($7);scope--;};

WhileStatement:	  WHILE LB Expression RB Statement   {   
	$$ = NON_TERMINAL("WhileStatement");  
	$$->children.push_back(NODE("Keyword","while")); 
	scope++;  
	$$->children.push_back(NODE("Separator","("));  
	if($3)  $$->children.push_back($3);    
	$$->children.push_back(NODE("Separator",")")); 
	if($5) $$->children.push_back($5);scope--;};

WhileStatementNoShortIf:  WHILE LB Expression RB StatementNoShortIf  {   
	$$ = NON_TERMINAL("WhileStatement");  
	$$->children.push_back(NODE("Keyword","while")); 
	scope++;  
	$$->children.push_back(NODE("Separator","("));  
	if($3)  $$->children.push_back($3);   
	$$->children.push_back(NODE("Separator",")")); 
	if($5) $$->children.push_back($5);scope--;
	};

DoStatement:	  DO Statement WHILE LB Expression RB SM_COLON     {   
	$$ = NON_TERMINAL("DoStatement");  
	scope++;
	if($2)  $$->children.push_back($2);    
	$$->children.push_back(NODE("Keyword","while"));   
	$$->children.push_back(NODE("Separator","("));  
	if($5) $$->children.push_back($5);   
	$$->children.push_back(NODE("Separator",")"));    
	$$->children.push_back(NODE("Separator",";")); scope--; };

ForInitOpt: ForInit     {   $$ = NON_TERMINAL("ForInitOpt");  
                            if($1)  $$->children.push_back($1);}
    |                   {	$$ = NULL; };
ExpressionOpt: Expression   {   $$ = NON_TERMINAL("ExpressionOpt");  
                                if($1)  $$->children.push_back($1);}
    |                       {	$$ = NULL; };
ForUpdateOpt: ForUpdate     {   $$ = NON_TERMINAL("ForUpdateOpt");  
                                if($1)  $$->children.push_back($1);}
    |                       {	$$ = NULL; };
ForStatement:	  FOR LB ForInitOpt SM_COLON ExpressionOpt SM_COLON ForUpdateOpt RB Statement    {   $$ = NON_TERMINAL("ForStatement");    
                 $$->children.push_back(NODE("Keyword","for")); 
				 scope++;
				 $$->children.push_back(NODE("Separator","("));  
				 if($3)  $$->children.push_back($3);    
				 $$->children.push_back(NODE("Separator",";"));   
				 if($5) $$->children.push_back($5);   
				 $$->children.push_back(NODE("Separator",";"));  
				 if($7) $$->children.push_back($7);   
				 $$->children.push_back(NODE("Separator",")")); 
				 if($9) $$->children.push_back($9); scope--;};
ForStatementNoShortIf:	  FOR LB ForInitOpt SM_COLON ExpressionOpt SM_COLON ForUpdateOpt RB StatementNoShortIf       {   $$ = NON_TERMINAL("ForStatement");    
                             $$->children.push_back(NODE("Keyword","for")); 
							 $$->children.push_back(NODE("Separator","("));  
							 scope++;
							 if($3)  $$->children.push_back($3);    
							 $$->children.push_back(NODE("Separator",";"));  
							 if($5) $$->children.push_back($5);   
							 $$->children.push_back(NODE("Separator",";"));  
							 if($7) $$->children.push_back($7);   
							 $$->children.push_back(NODE("Separator",")")); 
							 if($9) $$->children.push_back($9); scope--;}; 
ForInit: StatementExpressionList    {   $$ = NON_TERMINAL("ForInit");  
                                        if($1)  $$->children.push_back($1);
									}
		| LocalVariableDeclaration  {   $$ = NON_TERMINAL("ForInit");  
		                                if($1)  $$->children.push_back($1);
									};
ForUpdate:	  StatementExpressionList   {   
	$$ = NON_TERMINAL("ForUpdate");  
	if($1)  $$->children.push_back($1);
	};
StatementExpressionList: StatementExpression    {   
	$$ = NON_TERMINAL("StatementExpressionList");  
	if($1)  $$->children.push_back($1);
	}
		| StatementExpressionList COMMA StatementExpression {   
			$$ = NON_TERMINAL("StatementExpressionList");  
			if($1)  $$->children.push_back($1);        
			$$->children.push_back(NODE("Separator",","));     
			if($3) $$->children.push_back($3);
		};
IDENTOpt: Identifier    {   $$ = NON_TERMINAL("IDENTOpt");    
                            if($1) $$->children.push_back($1);}
    |       {	$$ = NULL; }
    ;
BreakStatement:	  BREAK IDENTOpt SM_COLON {   
	$$ = NON_TERMINAL("BreakStatement");  
	$$->children.push_back(NODE("Keyword","break"));  
	if($2)  $$->children.push_back($2);  
	$$->children.push_back(NODE("Separator",";"));};
ContinueStatement: CONTINUE IDENTOpt SM_COLON     {   
	$$ = NON_TERMINAL("ContinueStatement");   
	$$->children.push_back(NODE("Keyword","continue"));  
	if($2)  $$->children.push_back($2);  
	$$->children.push_back(NODE("Separator",";"));
	};
ReturnStatement:  RETURN ExpressionOpt SM_COLON   {   
	$$ = NON_TERMINAL("ReturnStatement");   
	$$->children.push_back(NODE("Keyword","return"));  
	if($2)  $$->children.push_back($2);  
	$$->children.push_back(NODE("Separator",";"));}
    | SUSPEND ExpressionOpt SM_COLON      {   
		$$ = NON_TERMINAL("ReturnStatement");   
		$$->children.push_back(NODE("Keyword","suspend"));  
		if($2)  $$->children.push_back($2);  
		$$->children.push_back(NODE("Separator",";"));
		};
ThrowStatement:  THROW Expression SM_COLON     {   
	$$ = NON_TERMINAL("ThrowStatement");   
	$$->children.push_back(NODE("Keyword","throw"));  
	if($2)  $$->children.push_back($2);  
	$$->children.push_back(NODE("Separator",";"));
	};
SynchronizedStatement:  SYNCHRONIZED LB Expression RB Block  {   
	$$ = NON_TERMINAL("SynchronizedStatement");   
	$$->children.push_back(NODE("Keyword","synchronized"));    
	$$->children.push_back(NODE("Separator","(")); 
	if($3)  $$->children.push_back($3);     
	$$->children.push_back(NODE("Separator",")"));     
	if($5) $$->children.push_back($5);};
CatchesOpt: Catches     {   $$ = NON_TERMINAL("CatchesOpt");  
if($1)  $$->children.push_back($1);}
    |                   {	$$ = NULL; }
    ;
TryStatement:	  TRY Block Catches     {   
	$$ = NON_TERMINAL("TryStatement");    
	$$->children.push_back(NODE("Keyword","try"));  
	if($2)  $$->children.push_back($2);    
	if($3) $$->children.push_back($3);
	}
		| TRY Block CatchesOpt Finally  {   $$ = NON_TERMINAL("TryStatement");
		                                    $$->children.push_back(NODE("Keyword","try"));  
											if($2)  $$->children.push_back($2);    
											if($3) $$->children.push_back($3);  
											if($4) $$->children.push_back($4);
										};
Catches:	  CatchClause   {   $$ = NON_TERMINAL("Catches");  
                                if($1)  $$->children.push_back($1);
							}
		| Catches CatchClause   {   $$ = NON_TERMINAL("Catches");  
		                            if($1)  $$->children.push_back($1);    
									if($2) $$->children.push_back($2);}
		;
CatchClause:	  CATCH LB FormalParameter RB Block      {   
	$$ = NON_TERMINAL("CatchClause");     
	$$->children.push_back(NODE("Keyword","catch"));   
	$$->children.push_back(NODE("Separator","(")); 
	if($3)  $$->children.push_back($3); 
	$$->children.push_back(NODE("Separator",")")); 
	if($5) $$->children.push_back($5);}
	;
Finally:	  FINALLY Block     {   $$ = NON_TERMINAL("Finally"); 
                                    $$->children.push_back(NODE("Keyword","finally"));  
									if($2)  $$->children.push_back($2);}
	;
Primary:	  PrimaryNoNewArray     {   $$ = NON_TERMINAL("Primary");  
                                        if($1)  $$->children.push_back($1);}
		| ArrayCreationExpression   {   $$ = NON_TERMINAL("Primary");  
		                                if($1)  $$->children.push_back($1);};
PrimaryNoNewArray: Literal          {   $$ = NON_TERMINAL("PrimaryNoNewArray");  
                                        if($1)  $$->children.push_back($1);
									}
        | THIS                      {   $$ = NON_TERMINAL("PrimaryNoNewArray");  
		                                $$->children.push_back(NODE("Keyword","THIS"));
									}
		| LB Expression RB
		| ClassInstanceCreationExpression   {   
			          $$ = NON_TERMINAL("PrimaryNoNewArray");  
					  if($1)  $$->children.push_back($1);
					}
		| FieldAccess         {   $$ = NON_TERMINAL("PrimaryNoNewArray");  
		                          if($1)  $$->children.push_back($1);}
		| MethodInvocation          {   $$ = NON_TERMINAL("PrimaryNoNewArray");  
		                                if($1)  $$->children.push_back($1);}
		| ArrayAccess               {   $$ = NON_TERMINAL("PrimaryNoNewArray");  
		                                if($1)  $$->children.push_back($1);}
		;
ClassInstanceCreationExpression: NEW ClassType LB ArgumentListOpt RB     {   
	 $$ = NON_TERMINAL("ClassInstanceCreationExpression"); 
	 $$->children.push_back(NODE("Keyword","new"));  
	 if($2)  $$->children.push_back($2);    
	 $$->children.push_back(NODE("Separator","("));    
	 if($4)  $$->children.push_back($4);  
	 $$->children.push_back(NODE("Separator",")"));
	 }
		;
ArgumentList:	  Expression        {   $$ = NON_TERMINAL("ArgumentList");  
                                        if($1)  $$->children.push_back($1);}
		| ArgumentList COMMA Expression     {   $$ = NON_TERMINAL("ArgumentList");  
		                                        if($1)  $$->children.push_back($1);   $$->children.push_back(NODE("Separator",","));     
												if($3) $$->children.push_back($3);}
		;
DimsOpt: Dims       {   $$ = NON_TERMINAL("DimsOpt");  
                        if($1)  $$->children.push_back($1);}
    |           {	$$ = NULL; }
    ;
ArrayCreationExpression: NEW PrimitiveType DimsOpt      {   
	$$ = NON_TERMINAL("ArrayCreationExpression"); 
	$$->children.push_back(NODE("Keyword","new"));  
	if($2)  $$->children.push_back($2);    
	if($3) $$->children.push_back($3);}
        | ArrayCreationExpression ArrayInitializer      {   
			$$ = NON_TERMINAL("ArrayCreationExpression");  
			if($1)  $$->children.push_back($1);        
			if($2) $$->children.push_back($2);}
		| NEW ClassOrInterfaceType DimsOpt          {   
			$$ = NON_TERMINAL("ArrayCreationExpression"); 
			$$->children.push_back(NODE("Keyword","new"));  
			if($2)  $$->children.push_back($2);        
			if($3)  $$->children.push_back($3);
			}
		;

Dims:		  SQ_L SQ_R        {   $$ = NON_TERMINAL("Dims");    
                                   $$->children.push_back(NODE("Separator","["));  $$->children.push_back(NODE("Separator","]"));  }
		| Dims SQ_L SQ_R       {   $$ = NON_TERMINAL("Dims");  
		                           if($1)  $$->children.push_back($1);   
								   $$->children.push_back(NODE("Separator","["));  $$->children.push_back(NODE("Separator","]"));}
		;
FieldAccess:	  Primary DOT Identifier    {   $$ = NON_TERMINAL("FieldAccess");  
                                                if($1)  $$->children.push_back($1);        $$->children.push_back(NODE("Separator",".")); 
												if($3)	$$->children.push_back($3);
											}
		| SUPER DOT Identifier {   $$ = NON_TERMINAL("FieldAccess");                                       
		$$->children.push_back(NODE("Keyword","super"));                                      $$->children.push_back(NODE("Separator",".")); 
		if($3)	$$->children.push_back($3);
		}
		;
MethodInvocation: Name LB ArgumentListOpt RB     {   
	$$ = NON_TERMINAL("MethodInvocation");  
	if($1)  $$->children.push_back($1);       
	$$->children.push_back(NODE("Separator","(")); 
	if($3) $$->children.push_back($3);  
	$$->children.push_back(NODE("Separator",")"));
	}
		| Primary DOT Identifier LB ArgumentListOpt RB   {   
			$$ = NON_TERMINAL("MethodInvocation");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Separator",".")); 
			if($3)	$$->children.push_back($3);    
			$$->children.push_back(NODE("Separator","(")); 
			if($5) $$->children.push_back($5);  
			$$->children.push_back(NODE("Separator",")"));
			}
		| SUPER DOT Identifier LB ArgumentListOpt RB     {   
			$$ = NON_TERMINAL("MethodInvocation");   
			$$->children.push_back(NODE("Keyword","super"));
			$$->children.push_back(NODE("Separator",".")); 
			if($3)	$$->children.push_back($3);    
			$$->children.push_back(NODE("Separator","(")); 
			if($5) $$->children.push_back($5);  
			$$->children.push_back(NODE("Separator",")"));
			}
		| Name Curly_L ArgumentListOpt Curly_R   {   
			$$ = NON_TERMINAL("MethodInvocation");  
			if($1)  $$->children.push_back($1);       
			$$->children.push_back(NODE("Separator","{")); 
			if($3) $$->children.push_back($3);  
			$$->children.push_back(NODE("Separator","}"));
			}
		| Primary DOT Identifier Curly_L ArgumentListOpt Curly_R {   
			$$ = NON_TERMINAL("MethodInvocation");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Separator",".")); 
			if($3)	$$->children.push_back($3);    
			$$->children.push_back(NODE("Separator","{")); 
			if($5) $$->children.push_back($5);  
			$$->children.push_back(NODE("Separator","}"));
		}
		| SUPER DOT Identifier Curly_L ArgumentListOpt Curly_R        {   
			$$ = NON_TERMINAL("MethodInvocation");   
			$$->children.push_back(NODE("Keyword","super"));
			$$->children.push_back(NODE("Separator",".")); 
			if($3)	$$->children.push_back($3);    
			$$->children.push_back(NODE("Separator","{")); 
			if($5) $$->children.push_back($5);  
			$$->children.push_back(NODE("Separator","}"));}
		;
ArrayAccess:	  Name SQ_L Expression SQ_R        {   
	$$ = NON_TERMINAL("ArrayAccess");  
	if($1)  $$->children.push_back($1);   
	$$->children.push_back(NODE("Separator","["));  
	if($3) $$->children.push_back($3);  
	$$->children.push_back(NODE("Separator","]"));
	}
	| PrimaryNoNewArray SQ_L Expression SQ_R   {   
		$$ = NON_TERMINAL("ArrayAccess");  
		if($1)  $$->children.push_back($1);    
		$$->children.push_back(NODE("Separator","["));  
		if($3) $$->children.push_back($3);  
		$$->children.push_back(NODE("Separator","]"));
		}
		;
PostFixExpression: Primary              {   
	$$ = NON_TERMINAL("PostFixExpression");  
	if($1)  $$->children.push_back($1);
	}
		| Name                          {   
			$$ = NON_TERMINAL("PostFixExpression");  
			if($1)  $$->children.push_back($1);
		}
		| PostIncrementExpression       {   
			$$ = NON_TERMINAL("PostFixExpression");  
			if($1)  $$->children.push_back($1);
		}
		| PostDecrementExpression       {   
			$$ = NON_TERMINAL("PostFixExpression");  
			if($1)  $$->children.push_back($1);
		}
		;
PostIncrementExpression: PostFixExpression INC  {   
	$$ = NON_TERMINAL("PostIncrementExpression");  
	if($1)  $$->children.push_back($1);    
	$$->children.push_back(NODE("Operator","++"));
	}
	;
PostDecrementExpression: PostFixExpression DECINC  {   
	$$ = NON_TERMINAL("PostDecrementExpression");  
	if($1)  $$->children.push_back($1);   
	$$->children.push_back(NODE("Operator","--"));}
		;
UnaryExpression:  PreIncrementExpression        {   
	$$ = NON_TERMINAL("UnaryExpression");  
	if($1)  $$->children.push_back($1);}
		| PreDecrementExpression                {   
			$$ = NON_TERMINAL("UnaryExpression");  
			if($1)  $$->children.push_back($1);}
		| PLUS UnaryExpression                   {   
			$$ = NON_TERMINAL("UnaryExpression"); 
			$$->children.push_back(NODE("Operator","+"));  
			if($2)  $$->children.push_back($2);}
		| MINUS UnaryExpression                   {   
			$$ = NON_TERMINAL("UnaryExpression");     
			$$->children.push_back(NODE("Operator","-")); 
			if($2)  $$->children.push_back($2);
			}
		| UnaryExpressionNotPlusMinus           {   
			$$ = NON_TERMINAL("UnaryExpression");  
			if($1)  $$->children.push_back($1);
		}
		;
PreIncrementExpression: INC UnaryExpression     {   
	$$ = NON_TERMINAL("PreIncrementExpression");  
	$$->children.push_back(NODE("Operator","++"));  
	if($2)  $$->children.push_back($2);
	}
	;
PreDecrementExpression: DECINC UnaryExpression     {   
	$$ = NON_TERMINAL("PreDecrementExpression");  
	$$->children.push_back(NODE("Operator","--"));  
	if($2)  $$->children.push_back($2);
	}
	;
UnaryExpressionNotPlusMinus: PostFixExpression  {   
	$$ = NON_TERMINAL("UnaryExpressionNotPlusMinus");  
	if($1)  $$->children.push_back($1);
	}
		| Tilde UnaryExpression                 {   
			$$ = NON_TERMINAL("UnaryExpressionNotPlusMinus"); 
			$$->children.push_back(NODE("Operator","~"));  
			if($2)  $$->children.push_back($1);
			}
		| NOT UnaryExpression                   {   
			$$ = NON_TERMINAL("UnaryExpressionNotPlusMinus"); 
			$$->children.push_back(NODE("Operator","!"));  
			if($2)  $$->children.push_back($1);}
		| CastExpression                        {   
			$$ = NON_TERMINAL("UnaryExpressionNotPlusMinus");  
			if($1)  $$->children.push_back($1);
			};
CastExpression:   LB PrimitiveType DimsOpt RB UnaryExpression {   
	$$ = NON_TERMINAL("CastExpression"); 
	$$->children.push_back(NODE("Separator","("));  
	if($2)  $$->children.push_back($2);    
	if($3) $$->children.push_back($3);  
	$$->children.push_back(NODE("Separator",")"));  
	if($5) $$->children.push_back($5);
	}
		| LB Expression RB UnaryExpressionNotPlusMinus        {   
			$$ = NON_TERMINAL("CastExpression"); 
			$$->children.push_back(NODE("Separator","("));  
			if($2)  $$->children.push_back($2);    
			$$->children.push_back(NODE("Separator",")"));    
			if($4) $$->children.push_back($4);
			}
		| LB Name Dims RB UnaryExpressionNotPlusMinus         {   
			$$ = NON_TERMINAL("CastExpression"); 
			$$->children.push_back(NODE("Separator","("));  
			if($2)  $$->children.push_back($2);    
			if($3) $$->children.push_back($3);  
			$$->children.push_back(NODE("Separator",")"));  
			if($5) $$->children.push_back($5);
			}
		;
MultiplicativeExpression: UnaryExpression                   {   
	$$ = NON_TERMINAL("MultiplicativeExpression");  
	if($1)  $$->children.push_back($1);}
		| MultiplicativeExpression MULTI UnaryExpression      {   
			$$ = NON_TERMINAL("MultiplicativeExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","*"));   
			if($3) $$->children.push_back($3);}
		| MultiplicativeExpression DIV UnaryExpression      {   
			$$ = NON_TERMINAL("MultiplicativeExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","/"));   
			if($3) $$->children.push_back($3);
			}
		| MultiplicativeExpression MOD UnaryExpression      {   
			$$ = NON_TERMINAL("MultiplicativeExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","%"));   
			if($3) $$->children.push_back($3);
			}
		;
AdditiveExpression: MultiplicativeExpression                {   
	$$ = NON_TERMINAL("AdditiveExpression");  
	if($1)  $$->children.push_back($1);
	}
		| AdditiveExpression PLUS MultiplicativeExpression   {   
			$$ = NON_TERMINAL("AdditiveExpression");  
			if($1)  $$->children.push_back($1); 
			$$->children.push_back(NODE("Operator","+"));   
			if($3) $$->children.push_back($3);
			}
		| AdditiveExpression MINUS MultiplicativeExpression   {   
			$$ = NON_TERMINAL("AdditiveExpression");  
			if($1)  $$->children.push_back($1); 
			$$->children.push_back(NODE("Operator","-"));   
			if($3) $$->children.push_back($3);
			}
		;
ShiftExpression:  AdditiveExpression                    {   
	$$ = NON_TERMINAL("ShiftExpression");  
	if($1)  $$->children.push_back($1);
	}
		| ShiftExpression SHIFT_L AdditiveExpression     {  
			$$ = NON_TERMINAL("ShiftExpression");  
			if($1)  $$->children.push_back($1);
			$$->children.push_back(NODE("Operator","<<"));   
			if($3) $$->children.push_back($3);
			}
		| ShiftExpression SHIFT_R AdditiveExpression     {   
			$$ = NON_TERMINAL("ShiftExpression");  
			if($1)  $$->children.push_back($1);
			$$->children.push_back(NODE("Operator",">>"));   
			if($3) $$->children.push_back($3);
			}
		| ShiftExpression TRIPLE_SHIFT AdditiveExpression    {   
			$$ = NON_TERMINAL("ShiftExpression");  
			if($1)  $$->children.push_back($1);
			$$->children.push_back(NODE("Operator",">>>"));   
			if($3) $$->children.push_back($3);
		}
		;
RelationalExpression: ShiftExpression                       {   
	$$ = NON_TERMINAL("RelationalExpression");  
	if($1)  $$->children.push_back($1);}
		| RelationalExpression LT ShiftExpression           {   
			$$ = NON_TERMINAL("RelationalExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","<"));   
			if($3) $$->children.push_back($3);
			}
		| RelationalExpression GT ShiftExpression           {   
			$$ = NON_TERMINAL("RelationalExpression");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Operator",">"));   
			if($3) $$->children.push_back($3);}
		| RelationalExpression LEQ ShiftExpression         {   
			$$ = NON_TERMINAL("RelationalExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","<="));   
			if($3) $$->children.push_back($3);
			}
		| RelationalExpression GEQ ShiftExpression         {   
			$$ = NON_TERMINAL("RelationalExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator",">="));   
			if($3) $$->children.push_back($3);
			}
		| RelationalExpression INSTANCEOF ReferenceType     {   
			$$ = NON_TERMINAL("RelationalExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Keyword","instanceof"));   
			if($3) $$->children.push_back($3);
			}
		;
EqualityExpression: RelationalExpression                {   
	$$ = NON_TERMINAL("EqualityExpression");  
	if($1)  $$->children.push_back($1);}
		| EqualityExpression DOUBLE_EQ RelationalExpression    {   
			$$ = NON_TERMINAL("EqualityExpression");  
			if($1)  $$->children.push_back($1); 
			$$->children.push_back(NODE("Operator","=="));   
			if($3) $$->children.push_back($3);
		}
		| EqualityExpression NOTEQ RelationalExpression   {   
			$$ = NON_TERMINAL("EqualityExpression");  
			if($1)  $$->children.push_back($1); 
			$$->children.push_back(NODE("Operator","!="));   
			if($3) $$->children.push_back($3);
		}
		;
AndExpression: EqualityExpression                       {   
	$$ = NON_TERMINAL("AndExpression");  
	if($1)  $$->children.push_back($1);
	}
		| AndExpression BITAND EqualityExpression    {   
			$$ = NON_TERMINAL("AndExpression");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Operator","&"));   
			if($3) $$->children.push_back($3);
			}
		;
ExclusiveOrExpression: AndExpression                    {   
	$$ = NON_TERMINAL("ExclusiveOrExpression");  
	if($1)  $$->children.push_back($1);
	}
		| ExclusiveOrExpression XOR AndExpression       {   
			$$ = NON_TERMINAL("ExclusiveOrExpression");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Operator","^"));   
			if($3) $$->children.push_back($3);
			}
		;
InclusiveOrExpression: ExclusiveOrExpression                    {   
	$$ = NON_TERMINAL("InclusiveOrExpression");  
	if($1)  $$->children.push_back($1);
	}
		| InclusiveOrExpression BITOR ExclusiveOrExpression    {   
			$$ = NON_TERMINAL("InclusiveOrExpression");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Operator","|"));   
			if($3) $$->children.push_back($3);
			}
		;
ConditionalAndExpression: InclusiveOrExpression                 {   
	$$ = NON_TERMINAL("ConditionalAndExpression");  
	if($1)  $$->children.push_back($1);}
		| ConditionalAndExpression AND InclusiveOrExpression    {   
			$$ = NON_TERMINAL("ConditionalAndExpression");  
			if($1)  $$->children.push_back($1);   
			$$->children.push_back(NODE("Operator","&&"));   
			if($3) $$->children.push_back($3);
			}
		;
ConditionalOrExpression: ConditionalAndExpression               {   
	$$ = NON_TERMINAL("ConditionalOrExpression");  
	if($1)  $$->children.push_back($1);}
		| ConditionalOrExpression OR ConditionalAndExpression   {   
			$$ = NON_TERMINAL("ConditionalOrExpression");  
			if($1)  $$->children.push_back($1);    
			$$->children.push_back(NODE("Operator","||"));   
			if($3) $$->children.push_back($3);
		}
		;
ConditionalExpression: ConditionalOrExpression                  {   
	$$ = NON_TERMINAL("ConditionalExpression");  
	if($1)  $$->children.push_back($1);
	}
		| ConditionalOrExpression QM Expression COLON ConditionalExpression {   
			$$ = NON_TERMINAL("ConditionalExpression");  
			if($1)  $$->children.push_back($1);  
			$$->children.push_back(NODE("Operator","?"));   
			if($3) $$->children.push_back($3);    
			$$->children.push_back(NODE("Operator",":"));   
			if($5) $$->children.push_back($5);
			}
		;
AssignmentExpression: ConditionalExpression             {   
	$$ = NON_TERMINAL("AssignmentExpression");  
	if($1)  $$->children.push_back($1);
	}
		| Assignment                                    {   
			$$ = NON_TERMINAL("AssignmentExpression");  
			if($1)  $$->children.push_back($1);
	}
		;
Assignment:	  LeftHandSide AssignmentOperator AssignmentExpression  {   
	$$ = NON_TERMINAL("Assignment");  
	if($1)  $$->children.push_back($1); 
	if($2)  $$->children.push_back($2); 
	if($3)  $$->children.push_back($3);
	}
		;
LeftHandSide:	  Name      {   $$ = NON_TERMINAL("LeftHandSide");  
                                if($1)  $$->children.push_back($1);}
		| FieldAccess       {   $$ = NON_TERMINAL("LeftHandSide");  
		                        if($1)  $$->children.push_back($1);}
		| ArrayAccess       {   $$ = NON_TERMINAL("LeftHandSide");  
		                        if($1)  $$->children.push_back($1);}
		;
AssignmentOperator: EQ  {   
	$$ = NON_TERMINAL("AssignmentOperator");  
	$$->children.push_back(NODE("Operator","="));}
	| MULTI_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                            $$->children.push_back(NODE("Operator","*="));}
	| DIV_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                          $$->children.push_back(NODE("Operator","/="));}
	| MOD_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                          $$->children.push_back(NODE("Operator","%="));}
	| PLUS_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                           $$->children.push_back(NODE("Operator","+="));}
	| MINUS_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                            $$->children.push_back(NODE("Operator","-="));}
	| SHIFT_L_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  
	                           $$->children.push_back(NODE("Operator","<<="));}
	| SHIFT_R_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  
	                           $$->children.push_back(NODE("Operator",">>="));}
	| TRIPLE_SHIFT_EQ          {   $$ = NON_TERMINAL("AssignmentOperator");  
	                               $$->children.push_back(NODE("Operator",">>>="));}
	| AND_EQ          {   $$ = NON_TERMINAL("AssignmentOperator");  
	                      $$->children.push_back(NODE("Operator","&="));}
	| XOR_EQ              {   $$ = NON_TERMINAL("AssignmentOperator");  
	                          $$->children.push_back(NODE("Operator","^="));}
	| OR_EQ           {   $$ = NON_TERMINAL("AssignmentOperator");  
	                      $$->children.push_back(NODE("Operator","|="));};
Expression:	  AssignmentExpression      {   $$ = NON_TERMINAL("Expression");  
                                            if($1)  $$->children.push_back($1);}
											;

%%

int main( int argc, char* argv[] )
{
	yyin = fopen (argv[ 1 ],"r" );
	fout1.open( "output.txt", ios::out );
	yyparse();
	fclose(yyin);
	fout.open( "output.dot", ios::out );
	fout << "digraph{\n";
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
        if(t->name!="")
        {
            fout << "node" << t->n << " [ label=\"" << t->name << "\" ]\n";
        }
        else
        {
            if(t->Lexeme[0]=='\"')
            {
                t->Lexeme = "\\" + t->Lexeme;
                t->Lexeme.pop_back();
                /* t->Lexeme += "\""; */
            }
            fout << "node" << t->n << " [ label= \"" << t->Lexeme << "(" << t->Token << ")" << " \" ]\n";
        }
        q.pop();
    }
	fout << "\n}";
	fout.close();
	stack<nodeptr *> s;
	s.push(root);
    nodeptr*t1;
	while(!s.empty()) 
	{
		t1 = s.top();
		s.pop();
		if(t1->name!="")
        {
            fout1 << "node" << t1->n << "(" << t1->name << ")\n";
			if(t1->name == "ClassDeclaration") classes.push_back(t1);

        }
        else
        {
            if(t1->Lexeme[0]=='\"')
            {
                t1->Lexeme = "\\" + t1->Lexeme;
                t1->Lexeme.pop_back();
            }
            fout1 << "node" << t1->n << "(" << t1->Lexeme << ")\tToken: " << "(" << t1->Token << ")\n";
        }
		for ( auto i: t1->children )
        {
			if( t1->name != "" ) 
			{
				if( i->name != "" ) 
				{
					fout1 << "node" << t1->n << "(" << t1->name << ") -> "<<"node" << i->n << "(" << i->name << ")\n";
				} 
				else 
				{
					fout1 << "node" << t1->n << "(" << t1->name << ") -> "<<"node" << i->n << "(" << i->Lexeme << ")\n";
				}
				
			}
			else 
			{
				if( i->name != "" ) 
				{
					fout1 << "node" << t1->n << "(" << t1->Lexeme << ") -> "<<"node" << i->n << "(" << i->name << ")\n";
				} 
				else 
				{
					fout1 << "node" << t1->n << "(" << t1->Lexeme << ") -> "<<"node" << i->n << "(" << i->Lexeme << ")\n";
				}
			}
            s.push(i);
        }
	}
	vector<string> names;
	for ( auto it: classes ) {
		names.push_back( it->children[ 2 ]->children[ 0 ]->Lexeme );
		nodeptr* curr = it->children[ it->children.size() - 1 ];
		vector<nodeptr *> v;
		if ( curr->children[ 1 ] != NULL ) {
			curr = curr->children[ 1 ]->children[ 0 ];
			
			if ( curr->children.size() == 1 ) {
				v.push_back( curr->children[ 0 ]->children[ 0 ]->children[ 0 ] );
			} else {
				while ( curr->children.size() > 1 ) {
					v.push_back( curr->children[ 1 ]->children[ 0 ]->children[ 0 ] );
					curr = curr->children[ 0 ];
				}
				v.push_back( curr->children[ 0 ]->children[ 0 ]->children[ 0 ] );
			}
		}
		classMembers.push_back( v );
	}

	for ( auto it: classMembers ) {
		vector<nodeptr *> vm;
		vector<nodeptr *> vf;
		for ( auto itr: it ) {
			if ( itr->name == "MethodDeclaration" ) vm.push_back( itr );
			else vf.push_back( itr );
		}
		methods.push_back( vm );
		fields.push_back( vf );
	}

	vector< vector< map<int, ID> > > theGiantVariableTable;


	for ( auto method: methods ) {
		vector< map<int, ID> > forAClass;
		for ( auto m: method ) {
			map<int, ID> variables;
			nodeptr* curr = m;
			curr = curr->children[ 1 ]; // method -> methodbody
			curr = curr->children[ 0 ]; // methodbody->block
			curr = curr->children[ 1 ]; // block->blockstatementsopt
			curr = curr->children[ 0 ]; // blockstatementsopt-> blockstatements
			if ( curr->children.size() == 1 ) {
				if ( curr->children[ 0 ]->children[ 0 ]->name == "LocalVariableDeclarationStatement" ) {
					curr = curr->children[ 0 ]->children[ 0 ]->children[ 1 ];
					string type = findType( curr->children[ 0 ] );
					getDeclarators( type, curr->children[ 1 ], variables);
				}

			} else {
				nodeptr* tempCurr = ( nodeptr* ) malloc( sizeof( nodeptr ) );
				while ( curr->children.size() > 1 ) {
						if ( curr->children[ 1 ]->children[ 0 ]->name == "LocalVariableDeclarationStatement" ) {
						tempCurr = curr->children[ 1 ]->children[ 0 ]->children[ 1 ];
						string type = findType( tempCurr->children[ 0 ] );
						getDeclarators( type, tempCurr->children[ 1 ], variables);
					}
					curr = curr->children[ 0 ];
				}
			}
			forAClass.push_back( variables );
		}
		theGiantVariableTable.push_back( forAClass );
	}

	vector< map<int, Method> > classMethods = getClassMethods( methods );
	vector< map<int, ID> > classFields;
	for ( auto field: fields ) {
		map<int, ID> fieldObjects;
		for ( auto f: field ) {
			string type = findType( f->children[ 1 ] );
			getDeclarators( type, f->children[ 2 ], fieldObjects );
		}
		classFields.push_back( fieldObjects );
	} 
	vector< vector<LocalTable*> > classLocalTables;
	
	for ( auto methods: classMethods ) {
		int i = 0;
		vector< LocalTable *> LocalTables;
		int j = 0;
		for ( auto method: methods ){
			LocalTable* lt = ( LocalTable* )malloc( sizeof( LocalTable ) );
			lt->m = &( method.second );
			lt->args = ( method.second ).arguments;
			lt->declarations = theGiantVariableTable[ i ][ j ];
			LocalTables.push_back( lt );
			j++;
		}
		classLocalTables.push_back( LocalTables );
		i++;
		
	}
	vector< GlobalTable* > GlobalTables;
	for ( auto field: classFields ) {
		GlobalTable * gt = ( GlobalTable * ) malloc( sizeof( GlobalTable ) );
		gt->fields = field;
		GlobalTables.push_back( gt );
	} 
	int i = 0;
	for ( auto method: classMethods ) {
		GlobalTables[ i ]->methods = method;
		i++;
	}
	i = 0;
	for ( auto GT1: GlobalTables ) {
		fstream fgt;
		string x = names[ i ] + ".csv";
		fgt.open( x, ios::out );
		fgt << "fields,\n";
		fgt << "name," << "type\n";
		for ( auto f: GT1->fields ) {
			fgt << f.second.lexeme << "," << f.second.type << endl;
		}
		fgt << "methods,\n";
		fgt << "name," << "return_type\n";
		for ( auto m: GT1->methods ) {
			fgt << m.second.lexeme << "," << m.second.return_type << endl;
			
		}
		fgt.close();
		i++;
	}
	i = 0;
	for ( auto clt: classLocalTables ) {
		for ( auto lt: clt ) {
			fstream fgt;
			string x = "meth" + names[ i ] + "_" + to_string( i ) + ".csv";
			fgt.open( x, ios::out );
			fgt << "arguments,\n";
			fgt << "name," << "type\n";
			for ( auto f: lt->args ) {
				fgt << f.second.lexeme << "," << f.second.type << endl;
			}
			fgt << "declarations,\n";
			fgt << "name," << "type\n";
			for ( auto f: lt->declarations ) {
				fgt << f.second.lexeme << "," << f.second.type << endl;
			}
			fgt.close();
		}
		i++;
	}

	fout1.close();
	return 0;
}

void yyerror (const char *s) {
   fprintf (stderr, "%s in line %d in %s!\n", s, yylineno, yytext);
}



