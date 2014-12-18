%{
/* rom.y: Roman numerals, trickier, right-recursive yacc, simpler lex
 * R Dowling 96Oct20
 */

#include <assembler/ast.h>

#define	YYTRACE

extern "C" int yylex();
extern "C" void yyerror(char *fmt, ...);

int last=0;

#define CODE( X )   { }

/**

Syntax

// enum definition
enum <name> {
	<name> [, <name>]
};

// type 
[int|float|string|iterator|boolean|<user defined|class<<name>>|array<<name>>]

// Structure definition
struct <name> [extends <name>] {
	<variable>; 
	[<variable>;]
};

// class definition
class <name> 
	[extends <name>] 	: K_INT		{ $$ = new Node(NT_REFERENCE,"int"); }
	
	[within <name>]
	[native|final|abstract|noproxy|nostub] {
	
	[<variable>|<function>|<state>]
	
	[defaults {
		<variable> = <literal>;[<variable> = <literal>;]
	}]
};

// state
state <name> [extends <name>] {
	[ignores <name>[,<name>];]
	
	[<function>]

	[Begin: <statement_list>]
};

// function
[native][private][virtual,static] <type> [operator[(<number>)|preoperator] <name>( [parameter[,parameter]] ) {
   [statement;][statement;]
}

// function parameter
[const][skip][optional][out] <type> <name> [ = <literal> ]

// Variable declaration
[config[(<name>)|const] <type> 	<name>[ = <literal][, <name>[ =<literal>] ];	

// Member access
<variable>.<name>

// function call
[<variable>.]<function>([<param][,<param>])

// Operator new ( class<Object> )
new class'<name>'
new <class variable>

// Operators
<term> <operator> <term>
<pre-operator> <term>

// Name
'<name>'

// String
"<name>"

// Number
0.<digits>[f]
<digits>[.<digits>][f]

// type cast 
<type>(<variable>)
*/
%}

/*
%union {
	char    *s;
    int 	i;
    float   f;    
    struct Node*		n;
    struct NodeList*	nl;
    struct Type*		type;
    struct CodeBlock*	code;
    struct Modifiers*	modifiers;
    struct Expr*        expr;
    struct Variable*	var;
}

%type <nl> file




//%type <s> string name operator
%type <s> operator
%type <expr> expr term literal cast_or_call
%type <var> variable
%type <n> statement 
%type <nl> statement_list
%type <code> compound_statement
%type <n> return_statement if_statement while_statement do_statement switch_statement for_statement
%type <nl> switch_cases
%type <n>  default_case switch_term

%type <modifiers> class_modifiers function_modifiers state_modifiers
// TODO: struct_modifiers parameter_modifiers variable_modifiers

%type <n> function
%type <code> function_body

%type <nl> types
%type <type> type
%type <n>	user_type

%type <nl> expr_list name_list parameter_list

%type <nl> declaration declaration_names

%type <n> init_declaration_init
%type <nl> init_declaration init_declarations

%token 	K_STRUCT K_CLASS K_STATE K_ENUM
%token  K_NATIVE K_PRIVATE K_VIRTUAL K_STATIC K_CONST
%token  K_OPTIONAL K_SKIP K_OUT K_NOPROXY K_NOSTUB K_NATIVE_REPLICATION
%token  K_FINAL K_ABSTRACT K_EXTENDS K_WITHIN K_CONFIG K_AUTO
%token  K_DEFAULTS K_IGNORES K_OPERATOR K_PREOPERATOR
%token  K_INT K_FLOAT K_VOID K_ARRAY K_ITERATOR K_BOOLEAN
%token  K_FOR K_IF K_WHILE K_DO K_RETURN K_BREAK K_CONTINUE K_SWITCH K_NEW K_TRUE K_FALSE K_NONE K_ELSE K_UNTIL K_CASE K_DEFAULT

%token <i> INT
%token <f> FLOAT
%token <s> STRING NAME OP

%start ROOT

%type <n>	state
%type <n>	default_properties
%type <nl>	state_definition
//TODO %type <n>	replication


// My Alterations
%type <nl>	class_definition
%type <nl>	class_body

// TODO %type <nl>	struct_definition

// UC specific
%type <nl>	uc_class_definition

*/

 %union {
 const char*            s;
 int                    i;
 float                  f;
 struct Node*           n;
 struct NodeList*       nl;
 struct Declaration*    decl;
 struct Type*           type;
 struct Variable*       var;
 struct Literal*        literal;
 struct Modifiers*      modifiers;
 struct Function*       func;
 struct CodeBlock*      code;
 struct Expr*           expr;
 }

 
// intrinsic types
%token  K_INT K_FLOAT K_VOID K_ARRAY K_ITERATOR K_BOOLEAN

// literals
%token <i> L_INT
%token <f> L_FLOAT
%token <s> L_STRING L_NAME
%token <s> L_OP OP_MINUS OP_PLUS OP_LOGICAL_NOT OP_BITWISE_NOT OP_INCREMENT OP_DECREMENT
%token K_FALSE K_TRUE K_NONE

// Compound type keywords
%token K_STRUCT K_CLASS K_STATE K_ENUM K_EXTENDS K_ABSTRACT K_WITHIN

// Modifiers
%token K_NATIVE K_PRIVATE K_VIRTUAL K_STATIC K_CONST
%token K_OPTIONAL K_SKIP K_OUT K_NOPROXY K_NOSTUB K_NATIVE_REPLICATION
%token K_FINAL K_AUTO

%token K_CONFIG K_DEFAULTS K_IGNORES

// Lanugae keywords
%token K_FOR K_IF K_WHILE K_DO K_RETURN K_BREAK K_CONTINUE K_SWITCH K_ELSE K_UNTIL K_CASE K_DEFAULT
%token K_NEW K_OPERATOR K_PREOPERATOR OP_EQUAL



%type <nl>      file         // collection of code

%type <type>    type        // type reference / use

%type <literal> literal
%type <var>     variable
%type <expr>    term        // a simple expression (literal, or variable reference)
%type <decl>    declaration

%type <n>       statement
%type <expr>    expr unary_expr literal_expr const_expr
%type <s>       operator unary_operator
%type <n> return_statement if_statement while_statement do_statement for_statement

%type <code>    compound_statement
%type <code>    statements


%type <func>    function
%type <nl>      function_parameters
%type <modifiers>      function_modifiers

//%left OP_MINUS

%nonassoc UNARY

%start ROOT

%%


ROOT
    :   file                        { setRoot($1); }


file
    :                               { $$ = new NodeList(); }
    |  file declaration             { $$ = $1; $1->add( $2 ); }// if( $2->initializer ) global_init->addStatement( $2->initializer ); }
    |  file function                { $$ = $1; $1->add( $2 ); }
    ;

type
    : K_INT                         { $$ = new Type("int"); }
    | K_FLOAT                       { $$ = new Type("float"); }
    | K_VOID                        { $$ = new Type("void"); }
    | K_ITERATOR                    { $$ = new Type("iterator"); }
    | K_BOOLEAN                     { $$ = new Type("boolean"); }
    | L_NAME                        { $$ = new Type($1); }
    ;

literal
    : L_INT                         { $$ = new Literal($1); }
    | L_FLOAT                       { $$ = new Literal($1); }
    | L_STRING                      { $$ = new Literal($1); }
    | K_NONE                        { $$ = new Literal("None"); }
    | K_FALSE                       { $$ = new Literal(0); }
    | K_TRUE                        { $$ = new Literal(1); }
    ;

literal_expr
    : literal                               { $$ = $1; }
    | unary_operator literal %prec UNARY    { $$ = new UnaryOperator( $2, $1 ); }
    ;

variable
    : L_NAME                        { $$ = new Variable( $1 /*, scope*/ ); }
//    | variable '.' T_NAME	{  }
    ;

term
    : literal                       { $$ = $1; }
    | variable                      { $$ = $1; }
    ;


declaration
    : type L_NAME ';'               { $$ = new Declaration( $1, $2 ); }
    | type L_NAME '=' literal_expr ';'   { $$ = new Declaration( $1, $2, new Assignment( new Variable( $2 ), $4 ) ); }
    ;

statement
    :   ';'						{ $$ = new Node(NT_NOP); }
    |	expr ';'				{ $$ = $1; }
    |   return_statement		{ $$ = $1; }
    |   if_statement			{ $$ = $1; }
    |   while_statement			{ $$ = $1; }
    |   for_statement			{ $$ = $1; }
    |   do_statement			{ $$ = $1; }
//    |   switch_statement		{ $$ = $1; }
    |   compound_statement		{ $$ = $1; }
    |   K_BREAK ';'				{ $$ = new Break(); }
    |   K_CONTINUE ';'			{ $$ = new Continue(); }
    ;

return_statement
    :   K_RETURN ';'			{ $$ = new Return(); }
    |   K_RETURN expr ';'		{ $$ = new Return( $2 ); }
    ;

if_statement
    : K_IF '(' expr ')'	statement			{ $$ = new If( $3, $5 ); }
    | K_IF '(' expr ')'	statement K_ELSE statement	{ $$ = new If( $3, $5, $7 ); }
    ;

while_statement
    : K_WHILE '(' expr ')' statement		{ $$ = new While( $3, $5 ); }
    ;

do_statement
    : K_DO statement K_WHILE '(' expr ')' ';'	{ $$ = new Do( $5, $2 ); }
    | K_DO statement K_UNTIL '(' expr ')' ';'	{ $$ = new Do( $5, $2 ); }
    ;

for_statement
    : K_FOR '(' ';' ';' ')' statement                   { $$=new For(NULL, NULL, NULL, $6); }
    | K_FOR '(' ';' ';' expr ')' statement              { $$=new For(NULL, NULL, $5, $7); }
    | K_FOR '(' ';' expr ';' ')' statement              { $$=new For (NULL, $4, NULL, $7); }
    | K_FOR '(' ';' expr ';' expr ')' statement         { $$=new For (NULL, $4, $6, $8); }
    | K_FOR '(' expr ';' ';' ')' statement              { $$=new For ($3, NULL, NULL, $7); }
    | K_FOR '(' expr ';' ';' expr ')' statement         { $$=new For ($3, NULL, $6, $8); }
    | K_FOR '(' expr ';' expr ';' ')' statement         { $$=new For ($3, $5, NULL, $8); }
    | K_FOR '(' expr ';' expr ';' expr ')' statement    { $$=new For ($3, $5, $7, $9); }
    ;
/*
switch_statement
    : K_SWITCH '(' expr ')' '{' switch_cases '}'                { $$ = new Switch( $3, $6, NULL ); }
    | K_SWITCH '(' expr ')' '{' switch_cases default_case '}'   { $$ = new Switch( $3, $6, (Case*)$7 ); }
;

switch_cases
    :                                                       { $$ = new NodeList(); }
    | switch_cases K_CASE switch_term ':'                   { $$ = $1; $$->add( new Case( $3, NULL ) ); }
    | switch_cases K_CASE switch_term ':' statement_list    { $$ = $1; $$->add( new Case( $3, new CodeBlock($5) ) ); }
;

// For noW only allow literals, later I can add in support for variables
switch_term
    : literal					{ $$ = $1; }
    ;

default_case
    : K_DEFAULT ':'                     { $$ = new Case( NULL, NULL ); }
    | K_DEFAULT ':'	statements          { $$ = new Case( NULL, new CodeBlock($3) ); }
    ;
*/

// TODO figure out how to manage a const expression, eg one that cannot be used as a LHV

expr
    : term                                 { $$ = $1; }
    | unary_operator expr %prec UNARY      { $$ = new UnaryOperator( $2, $1 ); }
    | expr operator expr                    { $$ = new Operator( $2, $1, $3 ); }
    | '(' expr ')'                      { $$ = new Grouping( $2 ); }
//    | variable operator expr            { $$ = new Operator( $2, $1, $3 ); }
//    | term operator expr                { $$ = new Operator( $2, $1, $3 ); }
//    | expr operator expr                { $$ = new Operator( $2, $1, $3 ); }
    // Pre operator to a variable
//    | L_OP variable                     { $$ = new PreOperator( $2, $1 ); }
//    | variable L_OP                     { /* $$ = new PostOperator( $1, $2 );*/ }
    // Assignment
    | variable '=' expr                 { $$ = new Assignment( $1, $3 ); };
    // function call
//    | expr '(' ')'                      { $$ = new FunctionCall( $1 ); }
//    | expr '(' expr_list ')'            { $$ = new FunctionCall( $1, $3 ); }
    ;

const_expr
    : literal_expr                              { $$ = $1; }
    | expr                                      { $$ = $1; }
    | unary_operator const_expr %prec UNARY     { $$ = new UnaryOperator( $2, $1 ); }
    ;

unary_expr
    : OP_PLUS expr                  { $$ = new UnaryOperator( $2, $1 ); }
    | OP_MINUS expr                 { $$ = new UnaryOperator( $2, $1 ); }
    | OP_LOGICAL_NOT expr			{ $$ = new UnaryOperator( $2, $1 ); }
    | OP_BITWISE_NOT expr			{ $$ = new UnaryOperator( $2, $1 ); }
    ;

unary_operator
    : '-'                       { $$ = "-"; }
    | '+'                       { $$ = "+"; }
    | '!'                       { $$ = "!"; }
    | '~'                       { $$ = "~"; }
    ;

operator
    : L_OP						{ $$ = $1; }
    | '-'                       { $$ = "-"; }
    | '+'                       { $$ = "+"; }
    | '*'                       { $$ = "*"; }
    | '/'                       { $$ = "/"; }
    | '!'                       { $$ = "!"; }
    | '~'                       { $$ = "~"; }
    | OP_EQUAL					{ $$ = "=="; }
    ;

statements
    :								{ $$ = new CodeBlock(); }
    | statements declaration
            { $$ = $1; $1->addLocal( $2 ); if( $2->initializer ) {$1->addStatement( $2->initializer ); } }
    | statements statement          { $$ = $1; $1->addStatement( $2 ); }
    ;

compound_statement
    : '{' '}'                       { $$ = new CodeBlock(); }
    | '{' statements '}'            { $$ = $2; }
    ;

function_modifiers
:                               { $$ = new Modifiers(); }
    | function_modifiers K_NATIVE
        { if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "native" ) ); }
    | function_modifiers K_ABSTRACT
        { if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "abstract" ) ); }
    | function_modifiers K_PRIVATE
        { if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "private") ); }
    | function_modifiers K_STATIC
        { if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "static") ); }
    | function_modifiers K_VIRTUAL
        { if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "virtual" ) ); }
    ;

function_parameters
    : type L_NAME
        { $$ = new NodeList(); $$->add( new Declaration( $1, $2 ) ); }
    | function_parameters ',' type L_NAME
        { $$ = $1; $$->add( new Declaration( $3, $4 ) ); }
    ;

function
    : type L_NAME '(' ')' compound_statement
        { $$ = $$ = new Function( $2, $1, new Modifiers(), new NodeList(), $5); }
    | type L_NAME '(' function_parameters ')' compound_statement {}
        { $$ = $$ = new Function( $2, $1, new Modifiers(), $4, $6); }
    | function_modifiers type L_NAME '(' ')' compound_statement
        { $$ = new Function( $3, $2, $1, new NodeList(), $6); }
    | function_modifiers type L_NAME '(' function_parameters ')' compound_statement
        { $$ = $$ = new Function( $3, $2, $1, $5, $7); }
    ;

/*
function
    : function_modifiers type L_NAME '(' ')' '{' statements '}'
        { $$ = new Function( $3, $2, $1, new NodeList(), $7); }
    | function_modifiers type L_NAME '(' function_parameters ')' '{' statements '}'
        { $$ = $$ = new Function( $3, $2, $1, $5, $8); }
    ;
*/


/*

ROOT
    : file					{ setRoot($1); }
	;

types
	: 						{ $$ = new NodeList(); }
	| types user_type		{ $$ = $1; $$->add( $2 ); }
	;
	
	
user_type
	: K_ENUM NAME '{' name_list '}' ';'				
							{ $$ = new Enum( $2, $4 ); }
	| K_CLASS NAME class_modifiers class_definition		
							{ $$ = new Class( $2, NULL, $3, $4 ); }
	| K_CLASS NAME K_EXTENDS NAME class_modifiers class_definition
							{ $$ = new Class( $2, new Type($4), $5, $6 ); }
// TODO
//    | K_STRUCT NAME struct_definition
//							{ $$ = new Struct( $2, NULL, NULL, $3 ); }
//	| K_STRUCT NAME K_EXTENDS NAME struct_definition
//							{ $$ = new Struct( $2, new Type($4), NULL, $5 ); }
	;		

name_list
	: NAME					{ $$ = new NodeList(); $$->add( new Name( $1 ) ); }
	| name_list ',' NAME	{ $$ = $1; $1->add( new Name( $3 ) ); }
	;

file 
	: 						{ $$ = new NodeList(); }
	| file declaration		{ $$ = $1; $1->addAll( $2 ); }
	| file statement		{ $$ = $1; $1->add( $2 ); }
	| file function			{ $$ = $1; $1->add( $2 ); }
	;

init_declaration				// initialized variable declarations
	: type init_declarations ';'	{ $$ = Declaration::setType( $1, $2 ); }
	;

init_declarations				// bob | ralph, cow
	: init_declaration_init							{ $$ = new NodeList(); $$->add( $1 ); }
	| init_declarations ',' init_declaration_init 	{ $$ = $1; $1->add( $3 ); }
	;		
	
init_declaration_init			// bob | bob = 4
	: NAME								{ $$ = new Declaration( NULL, $1 ); }
	| NAME '=' expr						{ $$ = new Declaration( NULL, $1, $3 ); }
	;
/ *
init_declaration
	: type init_declaration_names ';'	{ $$ = Declaration::setType( $1, $2 ); }
	;

init_declaration_names
	: NAME							{ $$ = new NodeList(); $$->add( new Declaration(NULL, $1 ) ); }
	| NAME '=' expr					{ $$ = new NodeList(); $$->add( new Declaration(NULL, $1, $3 ) ); }
	| declaration_names ',' NAME 	{ $$ = $1; $1->add( new Declaration(NULL, $3 ) ); }
	| declaration_names ',' NAME '=' expr 	{ $$ = $1; $1->add( new Declaration(NULL, $3, $5 ) ); }
	;	
* /

declaration
	: type declaration_names ';'	{ $$ = Declaration::setType( $1, $2 ); }
	;

declaration_names
	: NAME							{ $$ = new NodeList(); $$->add( new Declaration(NULL, $1 ) ); }
	| declaration_names ',' NAME 	{ $$ = $1; $1->add( new Declaration(NULL, $3 ) ); }
	;	

function
	: function_modifiers type NAME '(' ')' '{' function_body '}'		{ $$ = new Function( $3, $2, $1, new NodeList(), $7); }
	| function_modifiers type NAME '(' parameter_list ')' '{' function_body '}' { $$ = $$ = new Function( $3, $2, $1, $5, $8); }
	;
	
function_modifiers
	: function_modifiers K_NATIVE
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "native" ) ); } 
	| function_modifiers K_ABSTRACT	
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "abstract" ) ); } 
	| function_modifiers K_PRIVATE
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "private") ); } 
	| function_modifiers K_STATIC
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "static") ); } 
	| function_modifiers K_VIRTUAL
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "virtual" ) ); } 
	;
	
function_body
	:								{ $$ = new CodeBlock(); }
	| function_body init_declaration{ $$ = $1; $1->addLocals( $2 ); }
	| function_body statement		{ $$ = $1; $1->addStatement( $2 ); }
	;

parameter_list
	: type NAME					    { $$ = new NodeList(); $$->add( new Declaration( $1, $2 ) ); }
	| parameter_list ',' type NAME		{ $$ = $1; $$->add( new Declaration( $3, $4 ) ); }
	;	
	

statement_list
	: statement					{ $$ = new NodeList(); $$->add( $1); }
	| statement_list statement  { $$ = $1; $$->add( $2 ); }
	;

statement
    :   ';'						{ $$ = new Node(NT_NOP); }
	|	expr ';'				{ $$ = $1; }
	// Add in return, while, etc
	|   return_statement		{ $$ = $1; }
	|   if_statement			{ $$ = $1; }
	|   while_statement			{ $$ = $1; }
	|   for_statement			{ $$ = $1; }
	|   do_statement			{ $$ = $1; }
	|   switch_statement		{ $$ = $1; }
	|   compound_statement		{ $$ = $1; }
	|   K_BREAK ';'				{ $$ = new Break(); }
	|   K_CONTINUE ';'			{ $$ = new Continue(); }
	;
	
compound_statement
	: '{' '}'					{ $$ = new CodeBlock(); }
	| '{' statement_list '}'	{ $$ = new CodeBlock( $2 ); }
	;	
	
return_statement				
	:   K_RETURN ';'			{ $$ = new Return(); }
	|   K_RETURN expr ';'		{ $$ = new Return( $2 ); }
	;	
	
if_statement
	: K_IF '(' expr ')'	statement			{ $$ = new If( $3, $5 ); }
	| K_IF '(' expr ')'	statement K_ELSE statement	{ $$ = new If( $3, $5, $7 ); }
	;	
	
while_statement
	: K_WHILE '(' expr ')' statement		{ $$ = new While( $3, $5 ); }
	;
	
do_statement
	: K_DO statement K_WHILE '(' expr ')' ';'	{ $$ = new Do( $5, $2 ); }
	| K_DO statement K_UNTIL '(' expr ')' ';'	{ $$ = new Do( $5, $2 ); }
	;		
	
for_statement	
	: K_FOR '(' ';' ';' ')' statement
								{ $$=new For(NULL, NULL, NULL, $6); }
	| K_FOR '(' ';' ';' expr ')' statement
								{ $$=new For(NULL, NULL, $5, $7); }
	| K_FOR '(' ';' expr ';' ')' statement
								{ $$=new For (NULL, $4, NULL, $7); }
	| K_FOR '(' ';' expr ';' expr ')' statement
								{ $$=new For (NULL, $4, $6, $8); }
	| K_FOR '(' expr ';' ';' ')' statement
								{ $$=new For ($3, NULL, NULL, $7); }
	| K_FOR '(' expr ';' ';' expr ')' statement
								{ $$=new For ($3, NULL, $6, $8); }
	| K_FOR '(' expr ';' expr ';' ')' statement
								{ $$=new For ($3, $5, NULL, $8); }
	| K_FOR '(' expr ';' expr ';' expr ')' statement
								{ $$=new For ($3, $5, $7, $9); }
	;	
	
switch_statement
	: K_SWITCH '(' expr ')' '{' switch_cases '}'	
								{ $$ = new Switch( $3, $6, NULL ); }	
	| K_SWITCH '(' expr ')' '{' switch_cases default_case '}'		
								{ $$ = new Switch( $3, $6, (Case*)$7 ); }	
	;								
	
switch_cases
	:							{ $$ = new NodeList(); }
	| switch_cases K_CASE switch_term ':'	
								{ $$ = $1; $$->add( new Case( $3, NULL ) ); }
	| switch_cases K_CASE switch_term ':' statement_list 
								{ $$ = $1; $$->add( new Case( $3, new CodeBlock($5) ) ); }
	;

// For not only allow literals, later I can add in support for variables	
switch_term
	: literal					{ $$ = $1; }
	;	
	
default_case
	: K_DEFAULT ':'				{ $$ = new Case( NULL, NULL ); }
	| K_DEFAULT ':'	statement_list	{ $$ = new Case( NULL, new CodeBlock($3) ); }
	;	
	
expr
	: term 				 		{ $$ = $1; }
	| '(' expr ')'				{ $$ = new Grouping( $2 ); }
	| variable operator expr	{ $$ = new Operator( $2, $1, $3 ); }
	| term operator expr		{ $$ = new Operator( $2, $1, $3 ); }
	| expr operator expr		{ $$ = new Operator( $2, $1, $3 ); }
	// Pre operator to a variable
	| OP variable				{ $$ = new PreOperator( $2, $1 ); }
	| variable OP				{ / * $$ = new PostOperator( $1, $2 );* / }
	// Assignment
	| variable '=' expr			{ $$ = new Assignment( $1, $3 ); };
	// function call
	| expr '(' ')'				{ $$ = new FunctionCall( $1 ); }
	| expr '(' expr_list ')' 	{ $$ = new FunctionCall( $1, $3 ); }
	;	
	
operator
	: OP						{ $$ = $1; }
	| OP_EQUAL					{ $$ = "=="; }
	;
		
cast_or_call				
	// class<Actor>( a )
	: K_CLASS '<' NAME '>' '(' expr ')' {}
	// function call
	| variable '(' ')'					{ $$ = new FunctionCall( $1 ); }
	| variable '(' expr_list ')'		{ $$ = new FunctionCall( $1, $3 ); }
	;
	
expr_list
	: expr							{ $$ = new NodeList(); $$->add( $1 ); }
	| expr_list ',' expr			{ $$ = $1; $$->add( $3 ); }
	
variable	
	: NAME					{ $$ = new Variable( $1/ *, scope* / ); }
	| variable '.' NAME		{  }
	;	
		
term 
	: literal		{ $$ = $1; }
	| variable		{ $$ = $1; }
	;	
	
literal
	: INT			{ $$ = new Literal($1); }
	| FLOAT			{ $$ = new Literal($1); }
	| STRING		{ $$ = new Literal($1); }
	| K_NONE		{ $$ = new Literal("None"); }
	| K_FALSE		{ $$ = new Literal(0); }
	| K_TRUE		{ $$ = new Literal(1); }
	;
		
type
	: K_INT		{ $$ = new Type("int"); }
	| K_FLOAT	{ $$ = new Type("float"); }
	| K_VOID	{ $$ = new Type("void"); }
	| K_ITERATOR{ $$ = new Type("iterator"); }
	| K_BOOLEAN { $$ = new Type("boolean"); }
	| NAME		{ $$ = new Type($1); }
	;
		
class_modifiers
    : class_modifiers K_NATIVE
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "native" ) ); } 
	| class_modifiers K_ABSTRACT	
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "abstract" ) ); } 
	| class_modifiers K_NATIVE_REPLICATION
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "nativereplication") ); } 
	| class_modifiers K_NOPROXY
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "noproxy") ); } 
	| class_modifiers K_NOSTUB
		{ if( ! $1 ) $1 = new Modifiers(); $$ = $1; $$->add( new Name( "nostub" ) ); } 
	;

class_body
	:								{ $$ = new NodeList(); }
	| class_body declaration		{ $$ = $1; $1->addAll( $2 ); }
	| class_body function			{ $$ = $1; $1->add( $2 ); }
	| class_body state				{ $$ = $1; $1->add( $2 ); }
//	| class_body replication		{ $$ = $1; $1->add( $2 ); }
	;

state
	: state_modifiers K_STATE NAME state_definition	
									{ $$ = new State( $3, NULL, $1, $4 ); }
	| state_modifiers K_STATE NAME K_EXTENDS NAME state_definition
									{ $$ = new State( $3, new Type($5), $1, $6 ); }
	;
	
state_modifiers
	:								{ }
	| K_AUTO						{ $$ = new Modifiers(); $$->add( new Name( "auto" ) ); }
	;
	
state_definition
	: '{' '}'						{ }
	;

// Deviation from UC
class_definition
	: '{' class_body '}' ';'					{ $$ = $2; }
	| '{' class_body default_properties '}' ';'	{ $$ = $2; if( $3 ) $$->add( $3 ); }
	;
	
uc_class_definition
	: ';' class_body default_properties		{ $$ = $2; if( $3 ) $$->add( $3 ); }
	;

default_properties
	:								{ $$ = NULL; }
	| K_AUTO '{' '}'            { $$ = NULL; / *?* / }
    ;
*/
%%
//#include "trace.i"	/* for mks yacc debugger */
