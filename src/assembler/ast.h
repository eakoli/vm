#pragma once

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

enum NKind {
	NT_NOP,

	NT_ENUM,
	NT_CLASS,
	NT_STRUCT,
	NT_STATE,

	NT_MODIFIERS,
	NT_NAME,			// Generic Name
	NT_DECLARATION,		// member/local variable
	NT_TYPE_REFERENCE,	// named reference to a type
	NT_FUNCTION_CALL,	// function invocation or a cast
	NT_STRING_LITERAL,
	NT_INT_LITERAL,
    NT_FLOAT_LITERAL,
	NT_OPERATOR,
    NT_UNARY_OPERATOR,
	NT_ASSIGNMENT,
	NT_PREOPERATOR,
	NT_POSTOPERATOR,
	NT_REFERENCE, // NT_SYMBOL instead?
	NT_VARIABLE,

	NT_IF,
	NT_RETURN,
	NT_WHILE,
	NT_DO,
	NT_SWITCH,
	NT_CASE,
	NT_FOR,
	NT_BREAK,
	NT_CONTINUE,

	NT_STATEMENT,
	NT_FUNCTION,
	NT_PARAMETER,
	NT_PARAMETER_LIST,
	NT_TERM,
	NT_GROUPED
};

typedef struct  {
	const char* name;
	int   value;
} variable;

/*extern "C" */const char* safeprintf( const char* s, ... );
/*extern "C" */variable& getVar( const char* name );
/*extern "C" */void setRoot(struct NodeList* root);

struct Node {
public:
	NKind kind;
	char  name[64];
	union {
		int i;
		float f;
		char* s;
	} value;

	Node( NKind kind ) {
		this->kind = kind;
	}

	virtual ~Node() {}

	virtual const char* getTypeName()
	{
		switch( kind )
		{
		case NT_STATEMENT 	: return "statement";
		case NT_ASSIGNMENT	: return "assign";
		case NT_STRING_LITERAL 	: return safeprintf("literal (%s)", value.s);
        case NT_INT_LITERAL 	: return safeprintf("literal (%d)", value.i);
        case NT_FLOAT_LITERAL 	: return safeprintf("literal (%f)", value.f);
		case NT_OPERATOR 	: return safeprintf("operator (%s)", name);
        case NT_UNARY_OPERATOR 	: return safeprintf("unary operator (%s)", name);
		case NT_PREOPERATOR	: return safeprintf("preoperator (%s)", name);
		case NT_TERM		: return "term";
		case NT_GROUPED		: return "grouped";
		case NT_REFERENCE	: return safeprintf("reference (%s)", value.s );
		case NT_PARAMETER_LIST: return "parameter_list";
		case NT_PARAMETER   : return safeprintf("parameter( %s %s )", name, value.s);
		case NT_NOP			: return "nop";
		case NT_POSTOPERATOR: return safeprintf("postop ( %s )", name );
		case NT_VARIABLE    : return safeprintf("var ( %s )", name );
		default:
			return "ERROR";
		}
	}

	virtual void recurse( void callback ( Node* ) )
	{
	//	if( lhv )
		//	callback( lhv );
		//if( rhv )
			//callback( rhv );
	}

	virtual void optimize()
	{
	}

	virtual bool isStatement()
	{
		return false;
	}
};

struct NodeList {
public:
	NodeList()
	:count(0),
	 size(0),
 	 nodes(NULL)
 	{
	}

	~NodeList() {
		if( nodes )
			delete[] nodes;
	}

	Node* get(int idx)
	{
		return nodes[idx];
	}

	int getCount() {
		return count;
	}

	void add( Node* n )
	{
		grow(1);
		nodes[count++] = n;
	}

	void addAll( NodeList* n )
	{
		for( int i =0; i < n->getCount(); i++ )
		{
			add( n->get(i) );
		}
	}

	void recurse( void callback ( Node* ) )
	{
		for( int i = 0; i < count; i++ )
		{
			callback( nodes[i] );
		}
	}

private:
	void grow( int amt )
	{
		if( count + amt >= size )
		{
			Node** n = new Node*[ size + 16 ];
			memcpy(n, nodes, size*sizeof(Node*));
			size += 16;
			delete[] nodes;
			nodes = n;
		}
	}

	int count;
	int size;
	Node** nodes;
};

struct NamedNode : public Node {
public:
	//char* name;

	NamedNode(NKind kind, const char* name)
	:Node( kind )
	{
		//this->name = strdup( name );
		strcpy(this->name,name);
	}

	const char* getName()
	{
		return name;
	}

	const char* getTypeName()
	{
		return name;
	}
};

struct Name : public NamedNode {
public:
	Name( const char* name)
	:NamedNode( NT_NAME, name )
	{
	}

	const char* getTypeName()
	{
		return safeprintf("name (%s)", name );
	}
};

struct Modifiers : public Node {
public:
	NodeList* modifiers;

	Modifiers( NodeList* modifiers )
	:Node( NT_MODIFIERS ),
	modifiers( modifiers )
	{
	}

	Modifiers()
	:Node( NT_MODIFIERS ),
	modifiers( new NodeList() )
	{
	}

	void add( Name* modifier )
	{
		modifiers->add( modifier );
	}

	const char* getTypeName() {
		if( modifiers->getCount() == 0 )
			return safeprintf("");
		else
		{
			const char* fmt = "modifiers( %s %s";
			for( int i=0; i < modifiers->getCount(); i++ )
			{
				fmt = safeprintf(fmt, ((NamedNode*)modifiers->get(i))->getName(), "%s %s");
			}
			return safeprintf(fmt,") ","");
		}

	}

	void recurse( void callback(Node*) )
	{
		modifiers->recurse( callback );
	}
};

// A reference to a defined type
struct Type : public NamedNode {
	public:

	Type( const char* name )
	: NamedNode( NT_TYPE_REFERENCE, name )
	{
	}

	const char* getTypeName() {
		return safeprintf("type (%s)", name);
	}
};

// The definition of a type
struct TypeDefinition : public NamedNode {
public:
	TypeDefinition( NKind kind, const char* name )
	:NamedNode( kind, name )
	{
	}
};

struct Statement: public Node {
public:
	Statement(NKind kind)
	:Node(kind)
	{
	}

	virtual void execute() = 0;

	bool isStatement()
	{
		return true;
	}
};

struct Expr: public Statement {
public:
	Expr(NKind kind)
	:Statement(kind)
	{
	}

	virtual int evaluate() = 0;

	void execute()
	{
		evaluate();
	}
};

// TODO: handle string/float
struct Literal: public Expr{
public:
	//int value;

	Literal( int value)
	:Expr(NT_INT_LITERAL)
	{
		this->value.i = value;
	}

	Literal( const char* value)
	:Expr(NT_STRING_LITERAL)
	{
		this->value.s = strdup(value);
	}

	Literal( float value)
	:Expr(NT_FLOAT_LITERAL)
	{
		this->value.f = value;
	}

	int evaluate()
	{
		return value.i;
	}
};

struct Variable: public Expr {
public:
	//char* name;

	Variable( const char* name )
	:Expr(NT_VARIABLE)
	{
		// Initialize name of Node
		strcpy(this->name,name);
	}

	int evaluate()
	{
		return get().value;
	}

	variable& get()
	{
		return getVar(name);
	}
};

struct Grouping: public Expr {
public:
	Expr* expr;

	Grouping( Expr* expr )
	:Expr(NT_GROUPED),
	expr(expr)
	{
	}

	const char* getTypeName()
	{
		return "grouping";
	}

	void recurse( void callback ( Node* ) )
	{
		callback( expr );
	}

	int evaluate()
	{
		return expr->evaluate();
	}
};

struct Operator: public Expr {
public:
	Expr* lhv;
	Expr* rhv;
	char* op;

	Operator(const char* op, Expr* lhv, Expr* rhv)
	:Expr(NT_OPERATOR),
	lhv(lhv),
	rhv(rhv),
	op(strdup(op))
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "operator (%s)", op );
	}

	void recurse( void callback ( Node* ) )
	{
		callback( lhv );
		callback( rhv );
	}

	int evaluate()
	{
		int l = lhv->evaluate();
		int r = rhv->evaluate();

		switch( *op )
		{
			case '+' : return l + r;
			case '-' : return l - r;
			case '*' : return l * r;
			case '/' : return l / r;
			default: return -1;
		}
	}

	int getPrecidence( char* op )
	{
		switch( *op )
		{
		case '+':
		case '-':
			return 20;
		case '*':
		case '/':
			return 16;
		}

		return 64;
	}

	void swap( Node*& a, Node*& b )
	{
		Node* t = a;
		a = b;
		b = t;
	}

	void optimize()
	{
		regroup();
	}

	void regroup()
	{
		// Only regroup Operator and Assign nodes
		if( rhv->kind != NT_OPERATOR && rhv->kind != NT_ASSIGNMENT )
			return;

		Operator* other = (Operator*)rhv;

		// form  TA <op> TB <op> TC
		int outerP = getPrecidence( op );
		int innerP = getPrecidence( other->op );

		if( outerP <= innerP )
		{
			printf("regrouping T1 %s T2 %s T3 --> (T1 %s T2) %s T3\n", op, other->op, op, other->op);

			Expr* termA = lhv;
			Expr* termB = other->lhv;
			Expr* termC = other->rhv;

			Expr* grouping = new Grouping(new Operator( op, termA, termB ) );

			op = other->op;

			lhv = grouping;
			rhv = termC;

			delete other;

			regroup();

			return;
		}
	}
};

struct UnaryOperator: public Expr {
    Expr* rhv;
    const char* op;
    
    UnaryOperator(Expr* rhv, const char* op)
	:Expr(NT_UNARY_OPERATOR),
	rhv(rhv),
	op(strdup(op))
	{
	}
    
	const char* getTypeName()
	{
		return safeprintf( "unary-operator (%s)", op );
	}
    
	void recurse( void callback ( Node* ) )
	{
		callback( rhv );
	}
    
	int evaluate()
	{
		switch( *op )
		{/*
            case '+':
                return ++lhv->get().value;
            case '-':
                return --lhv->get().value;
            default:
         */
                return 0;
		}
	}

};

struct PreOperator: public Expr {
public:
	Variable* lhv;
	char* op;

	PreOperator(Variable* lhv, const char* op)
	:Expr(NT_OPERATOR),
	lhv(lhv),
	op(strdup(op))
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "pre-operator (%s)", op );
	}

	void recurse( void callback ( Node* ) )
	{
		callback( lhv );
	}

	int evaluate()
	{
		switch( *op )
		{
		case '+':
			return ++lhv->get().value;
		case '-':
			return --lhv->get().value;
		default:
			return 0;
		}
	}
};

struct PostOperator: public Expr {
public:
	Variable* lhv;
	char* op;

	PostOperator(Variable* lhv, const char* op)
	:Expr(NT_OPERATOR),
	lhv(lhv),
	op(strdup(op))
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "post-operator (%s)", op );
	}

	void recurse( void callback ( Node* ) )
	{
		callback( lhv );
	}

	int evaluate()
	{
		switch( *op )
		{
		case '+':
			return lhv->get().value++;
		case '-':
			return lhv->get().value--;
		default:
			return 0;
		}
	}

};

struct Assignment: public Expr {
public:
	Variable* lhv;
	Expr* rhv;

	Assignment(Variable* lhv, Expr* rhv)
	:Expr(NT_ASSIGNMENT),
	lhv(lhv),
	rhv(rhv)
	{
	}

	const char* getTypeName()
	{
		return "assign";
	}

	void recurse( void callback ( Node* ) )
	{
		callback( lhv );
		callback( rhv );
	}

	int evaluate()
	{
		return lhv->get().value = rhv->evaluate();
	}
};

struct Declaration : public NamedNode {
public:
	Node* initializer;
	Type* type;
    int   offset;

	Declaration(Type* type, const char* name)
	:NamedNode(NT_DECLARATION, name),
	 initializer( NULL ),
	 type( type ),
     offset( 0 )
	{
	}

	Declaration(Type* type, const char* name, Node* initializer)
	:NamedNode(NT_DECLARATION, name),
	 initializer( initializer ),
	 type( type ),
     offset( 0 )
	{
	}

	static NodeList* create( Type* type, NodeList* names )
	{
		NodeList* n = new NodeList();

		for( int i=0; i< names->getCount(); i++)
		{
			Name* name = (Name*)names->get(i);
			n->add( new Declaration( type, name->getName() ) );
		}

		return n;
	}

	static NodeList* setType( Type* type, NodeList* declarations )
	{
		for( int i=0; i< declarations->getCount(); i++)
		{
			Declaration* decl = (Declaration*)declarations->get(i);
			decl->type = type;
		}

		return declarations;
	}

	const char* getTypeName()
	{
		return safeprintf( "declaration (%s %s)", name, type->getName() );
	}

	void recurse( void callback ( Node* ) )
	{
		if( type )
			callback( type );
		if( initializer )
			callback( initializer );
	}
};

struct Return : public Statement {
public:
	Expr* expr;

	Return()
	:Statement(NT_RETURN)
	{
	}

	Return(Expr* expr)
	:Statement(NT_RETURN),
	expr(expr)
	{
	}

	const char* getTypeName()
	{
		return "return";
	}

	void recurse( void callback ( Node* ) )
	{
		if( expr )
			callback( expr );
	}

	void execute()
	{
		printf("return not implemented");
	}
};

struct CodeBlock : public Node {
public:
	NodeList* code;
	NodeList* locals;

	CodeBlock()
	:Node(NT_STATEMENT),
	 code(new NodeList()),
	 locals( new NodeList() )
	{
	}

	CodeBlock(Node* code)
	:Node(NT_STATEMENT),
	 code(new NodeList()),
	 locals( new NodeList() )
	{
		this->code->add( code );
	}

	CodeBlock(NodeList* code)
	:Node(NT_STATEMENT),
	 code(code),
	 locals( new NodeList() )
	{
	}

	const char* getTypeName()
	{
		return "code_block";
	}

	void addStatement( Node* node )
	{
		code->add( node );
	}

	void addLocal( Declaration* node )
	{
		locals->add( node );
	}

	void addLocals( NodeList* nodes )
	{
		locals->addAll( nodes );
	}

	void recurse( void callback ( Node* ) )
	{
		if( locals )
			locals->recurse(callback);
		if( code )
			code->recurse(callback);
	}
};


struct If : public Node
{
public:
	Node* condition;
	Node* on_true;
	Node* on_false;

	If( Node* condition, Node* on_true, Node* on_false )
	: Node( NT_IF ),
	condition(condition),
	on_true(on_true),
	on_false(on_false)
	{
	}

	If( Node* condition, Node* on_true )
	: Node( NT_IF ),
	condition(condition),
	on_true(on_true),
	on_false(NULL)
	{
	}

	const char* getTypeName()
	{
		return safeprintf( on_false ? "if_else" : "if" );
	}

	void recurse( void callback(Node*) )
	{
		callback( condition );
		callback( on_true );
		if( on_false )
			callback( on_false );
	}
};

struct While : public Node
{
public:
	Node* condition;
	Node* on_true;

	While( Node* condition, Node* on_true )
	: Node( NT_WHILE ),
	condition(condition),
	on_true(on_true)
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "while-do" );
	}

	void recurse( void callback(Node*) )
	{
		callback( condition );
		callback( on_true );
	}
};

struct Do : public Node
{
public:
	Node* condition;
	Node* on_true;

	Do( Node* condition, Node* on_true )
	: Node( NT_DO ),
	condition(condition),
	on_true(on_true)
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "do-while" );
	}

	void recurse( void callback(Node*) )
	{
		callback( condition );
		callback( on_true );
	}
};

struct For : public Node
{
public:
	Node* initializer;
	Node* condition;
	Node* postcondition;
	Node* statement;

	For( Node* initializer, Node* condition, Node* postcondition, Node* statement )
	:Node(NT_FOR),
	initializer(initializer),
	condition(condition),
	postcondition(postcondition),
	statement( statement )
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "for ( %s ; %s ; %s )", initializer ? "expr" : "", condition ? "expr" : "", postcondition ? "expr" : "" );
	}

	void recurse( void callback(Node*) )
	{
		if( initializer )
			callback( initializer );
		if( initializer )
			callback( condition );
		if( postcondition )
			callback( postcondition );
		if( statement )
			callback( statement );
	}

};

struct Break: public Node {
public:
	Break()
	:Node(NT_BREAK)
	{
	}

	const char* getTypeName()
	{
		return "break";
	}
};

struct Continue: public Node {
public:
	Continue()
	:Node(NT_CONTINUE)
	{
	}

	const char* getTypeName()
	{
		return "break";
	}
};

struct Case: public Node {
public:
	Node* expr;
	CodeBlock* code;

	Case(Node* expr, CodeBlock* code )
	:Node(NT_CASE),
	expr(expr),
	code(code)
	{
	}

	const char* getTypeName()
	{
		return safeprintf( "case" );
	}

	void recurse( void callback(Node*) )
	{
		if( expr )
			callback( expr );
		if( code )
			callback( code );
	}
};

struct Switch: public Node {
public:
	Node* expr;
	NodeList* cases;
	Case* default_case;

	Switch( Node* expr, NodeList* cases, Case* default_case )
	:Node(NT_SWITCH),
	expr(expr),
	cases(cases),
	default_case(default_case)
	{
	}

	void addCase( Case* c )
	{
		cases->add( c );
	}

	const char* getTypeName()
	{
		return safeprintf( "switch" );
	}

	void recurse( void callback(Node*) )
	{
		callback( expr );
		cases->recurse( callback );
		if( default_case )
			callback( default_case );
	}
};

struct Enum : public TypeDefinition {
public:
	NodeList* values;

	Enum(const char* name, NodeList* values )
	:TypeDefinition(NT_ENUM, name),
	values( values )
	{
	}

	const char* getTypeName() {
		return safeprintf("enum (%s)", name);
	}

	void recurse( void callback(Node*) )
	{
		values->recurse( callback );
	}

};

struct Class : public TypeDefinition {
public:
	Type*      extends;
	Modifiers* modifiers;
	NodeList*  members;

	Class(const char* name, Type* extends, Modifiers* modifiers, NodeList* members )
	:TypeDefinition(NT_CLASS, name),
	extends( extends ),
	modifiers( modifiers ),
	members( members )
	{
	}

	const char* getTypeName() {
		return safeprintf("class (%s extends %s)", name, extends ? extends->getName() : "Object");
	}

	void recurse( void callback(Node*) )
	{
		if( extends )
			callback( extends );
		if( modifiers )
			callback( modifiers );
		members->recurse( callback );
	}
};

struct Struct : public TypeDefinition {
public:
	Type*      extends;
	Modifiers* modifiers;
	NodeList*  members;

	Struct(const char* name, Type* extends, Modifiers* modifiers, NodeList* members )
	:TypeDefinition(NT_STRUCT, name),
	extends( extends ),
	modifiers( modifiers ),
	members( members )
	{
	}

	const char* getTypeName() {
		return safeprintf("struct (%s extends %s)", name, extends ? extends->getName() : "None");
	}

	void recurse( void callback(Node*) )
	{
		if( extends )
			callback( extends );
		if( modifiers )
			callback( modifiers );
		members->recurse( callback );
	}
};

struct State : public TypeDefinition {
public:
	Type*      extends;
	Modifiers* modifiers;
	NodeList*  members;

	State(const char* name, Type* extends, Modifiers* modifiers, NodeList* members )
	:TypeDefinition(NT_STATE, name),
	extends( extends ),
	modifiers( modifiers ),
	members( members )
	{
	}
	const char* getTypeName() {
		return safeprintf("state (%s extends %s)", name, extends ? extends->getName() : name);
	}

	void recurse( void callback(Node*) )
	{
		if( extends )
			callback( extends );
		if( modifiers )
			callback( modifiers );
		members->recurse( callback );
	}
};

struct Function : public NamedNode {
	public :

	Type* return_type;
	NodeList* parameters;
	CodeBlock* code;
	Modifiers* modifiers;

	Function( const char* name, Type* return_type, Modifiers* modifiers, NodeList* parameters, CodeBlock* code )
	: NamedNode(NT_FUNCTION, name),
	  return_type( return_type ),
	  parameters( parameters ),
	  code( code ),
	  modifiers(modifiers)
	{
	}

	const char* getTypeName() {
		return safeprintf("%sfunction (%s)", modifiers->getTypeName(), name);
	}

	void recurse( void callback ( Node* ) )
	{
		callback( return_type );
		parameters->recurse( callback );
		callback( code );
	}
};

struct FunctionCall : public Expr {
public:
	NamedNode* func;
	NodeList* args;

	FunctionCall( Node* func )
	:Expr( NT_FUNCTION_CALL ),
	 func( (NamedNode*)func ),
	 args( NULL )
	{
	}

	FunctionCall( Node* func, NodeList* args )
	:Expr( NT_FUNCTION_CALL ),
	 func( (NamedNode*)func ),
	 args( args )
	{
	}

	const char* getTypeName()
	{
		return safeprintf(" call ( %s )", func->getName() );
	}

	void recurse( void callback( Node* ) )
	{
		callback( func );
		if( args )
			args->recurse( callback );
	}

	int evaluate()
	{
		printf("Function execution not implemented\n");
		return 0;
	}
};


