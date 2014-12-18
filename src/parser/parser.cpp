#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <assembler/ast.h>

const char* safeprintf( const char* s, ... )
{
	static char bufs[64][1024];
	static int idx = 0;

	idx = (idx+1) % 63;
	char* buf = bufs[idx];

	va_list args;
	va_start(args,s);

	vsprintf( buf, s, args);

	va_end(args);
	return buf;
}

variable& getVar( const char* name ) {
	static variable vars[100];

	int i;
	for( i = 0; i < 100; i++ )
	{
		if( ! vars[i].name )
			break;

		if( strcmp( vars[i].name, name ) == 0 )
			return vars[i];
	}

	vars[i].name = name;

	return vars[i];
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


/**
 * Regroup based on precidence. This routine inserts ()
 * explicit () to regroup based on the precidences of each operator
 *
 * so:
 * 2 * 3 * 4 * 5 + 6 + 7  			== ((((2*3)*4)*5)+6)+7)
 * 2 * 3 * 4 * 5 + 6 + 7 * 8 * 9  	== (((((2*3)*4)*5)+6)+7)+(8*9))
 *
 */
void regroup( Node* node )
{
	if( node->kind == NT_OPERATOR )
	{
		// form  TA <op> TB <op> TC
		/*
		if( node->rhv->kind == NT_OPERATOR )
		{
			int outerP = getPrecidence( node->name );
			int innerP = getPrecidence( node->rhv->name );

			if( outerP <= innerP )
			{
				printf("regrouping T1 %s T2 %s T3 --> (T1 %s T2) %s T3\n", node->name, node->rhv->name,node->name, node->rhv->name);

				Node* termA = node->lhv;
				Node* termB = node->rhv->lhv;
				Node* termC = node->rhv->rhv;
				Node* obsoleted = node->rhv;

				Node* grouping = new Node(NT_GROUPED, new Node( NT_OPERATOR, node->name, termA, termB ) );

				strcpy( node->name, obsoleted->name );
				node->lhv = grouping;
				node->rhv = termC;

				delete obsoleted;

				regroup( node );

				return;
			}
		}
		*/
	}
	/*
	if( node->lhv )
		regroup( node->lhv );
	if( node->rhv )
		regroup( node->rhv );
	*/
}

/*
int eval( Node* node )
{
	if( node->kind == NT_OPERATOR )
	{
		int lhv = eval( node->lhv );
		int rhv = eval( node->rhv );

		//printf("%d %s %d\n", lhv, node->name, rhv);

		switch( *node->name )
		{
			case '+' : return lhv + rhv;
			case '-' : return lhv - rhv;
			case '*' : return lhv * rhv;
			case '/' : return lhv / rhv;
			default: return -1;
		}
	}
	else if( node->kind == NT_PREOPERATOR )
	{
		int value = eval( node->lhv );

		return *node->name == '+' ? ++value : --value;
	}
	else if( node->kind == NT_REFERENCE )
	{
		return getVar( node->value.s ).value;
	}
	else if( node->kind == NT_LITERAL )
	{
		return node->value.i;
	}
	else if( node->kind == NT_GROUPED )
	{
		return eval( node->lhv );
	}
	else
		return -1;
}


void evaluate( Node* node )
{
	if( node->kind == NT_ASSIGN )
	{
		int rhv = eval( node->rhv );
		getVar( node->lhv->value.s ).value = rhv;
		printf("Assign %s -> %d\n", node->lhv->value.s, rhv);
	}
	else if( node->kind == NT_OPERATOR )
	{
		printf( "Evaluated to : %d\n", eval( node ) );
	}
	else
	{
		if( node->lhv )
			evaluate( node->lhv );
		if( node->rhv )
			evaluate( node->rhv );
	}
}
*/



void evaluate( Expr* node )
{
	if( node->kind == NT_ASSIGNMENT )
	{
		printf("Assign %s -> %d\n", node->name, node->evaluate());
	}
	else if( node->kind == NT_OPERATOR )
	{
		printf( "Evaluated to : %d\n", node->evaluate() );
	}
}

struct {
	char* name;
	NKind kind;
	Node* found;
} find_state;

void find( Node* node )
{
	if( find_state.found )
		return;

	if( node->kind == find_state.kind )
	{
		if( strcmp( find_state.name, ((NamedNode*)node)->getName() ) == 0 )
		{
			find_state.found = node;
			return;
		}
	}

	node->recurse( &find );
}

Node* find( Node* root, char* name, NKind kind )
{
	find_state.found = NULL;
	find_state.name = name;
	find_state.kind = kind;

	find( root );
}

int execute( Node* root, char* name )
{
	Function* f = (Function*)find( root, name, NT_FUNCTION );
	if( f )
		return 1;//f->evaluate();
}


extern "C" {
    
    void uc_error( const char* msg, ... );
    
int uc_wrap() {
    return 1;
}
    
}
