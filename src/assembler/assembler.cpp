
#include <vm/vm.h>
#include "ast.h"

#include <time.h>
#include <list>

// JIT Library
// http://www.gnu.org/software/dotgnu/libjit-doc/libjit.html#Top


/**
 * Processing the AST.
 *
 * As part of the AST pre-processing, all lookups and type checking needs to be done. This would involve
 * 1) creating the graph of all types just defined.
 * 2) locating all imported types (this will require loading external libaries)
 * 3) resolving the base type for all classes/structs
 * 4) resolving the types for all variables
 * 5) resolve all variable references
 * 6) resolve all method references (this may require coercing arguments -- yuck)
 * 7) validating type compatibility
 *
 * Since this can be done in multiple passes to reduce complexity, the AST must contain enough
 * information about the source files to generate meaningful errors. Possibly include Source AST nodes
 * that carry file/line number references.
 *
 */

struct opcode {
	OPCode opcode;
	const char* name;
	int params;
};

opcode opcodes[] = {
	{OP_ENTER,"OP_ENTER",1},
	{OP_LEAVE,"OP_LEAVE",1},

	{OP_CONST,"OP_CONST",1},

	{OP_LOAD_W4,"OP_LOAD_W4",1},	// Load 4 bytes from working_stack
	{OP_STOR_W4,"OP_STOR_W4",1},	// Store 4 bytes to working_stack

	{OP_LOAD_P4,"OP_LOAD_P4",1}, // Load 4 bytes from a pointer
	{OP_STOR_P4,"OP_STOR_P4",1}, // Store 4 bytes to a pointer

	{OP_LEA_W,"OP_LEA_W",1},	// Load the effective address for the location on the working_stack

	{OP_JMP,"OP_JMP",1},		// jmp
	{OP_JZ,"OP_JZ",1},		// jmp if zero
	{OP_JNZ,"OP_JNZ",1}, 	// jmp if not zero

	{OP_CALL,"OP_CALL",1},	// call function

	{OP_EQ,"OP_EQ",0},		// equal
	{OP_NEQ,"OP_NEQ",0},		// not equal

	{OP_MULI,"OP_MULI",0},
	{OP_DIVI,"OP_DIVI",0},
	{OP_ADDI,"OP_ADDI",0},
	{OP_SUBI,"OP_SUBI",0},

	{OP_DUP,"OP_DUP",0}, // duplicate the top item on the stack -- can be used to store a result for use again
	{OP_SWAP,"OP_SWAP",0} 	// swap top item in stack with the Nth back.

};

/**
 * Trigger node optimization.
 */
void optimize( Node* node )
{
	node->optimize();
	node->recurse( &optimize );
}

Function* scope_Function;
std::list<CodeBlock*> scopes;

/**
 * Preprocess all the nodes.
 *
 *      optmize expressions
 *      assign addresses to all declaration
 *      resolve variable references (this, locals, etc)
 *      verify last statement of all functions is a return w/ apropriate return value
 *      validate expression types / coersion.
 */
void pre_process( Node* node )
{
	if( node->kind == NT_FUNCTION )
	{
		node->recurse( &pre_process );
	}
	else if( node->kind == NT_OPERATOR || node->kind == NT_ASSIGNMENT )
	{
		optimize( node );

		printf(" ----------------------------- \n");
	}
	else
	{
		node->recurse( &pre_process );
	}
}

int depth = 0;

void report( Node* node )
{
	for( int i=0; i < depth; i++)
		printf("\t");
	printf("%s\n", node->getTypeName() );

	depth++;
	node->recurse( &report );
	depth--;
}

struct vm_byte_compiler_t {
	int pcode[512];
	int pcode_index;

	vm_byte_compiler_t() {
		pcode_index = 0;
	}

	vm_byte_compiler_t& emit( int code ) {
		pcode[pcode_index++] = code;

		return *this;
	}

	void reset() {
		pcode_index = 0;
	}

	void report() {
		for( int i = 0; i < pcode_index; i++ )
		{
			opcode* o = &opcodes[pcode[i]];

			printf(!o->params ? "%s\n" : "%s %d\n", o->name, pcode[i+1]);

			i += o->params;
		}
	}
};

vm_byte_compiler_t bytes;

#define LOCAL( ofs ) (-ofs )
#define ARG( ofs )   ( ofs )

void compile_statement( Node* node );

void load_variable( Variable* var, bool reference ) {
    // find variable
    for( std::list<CodeBlock*>::iterator it = scopes.begin(); it != scopes.end(); ++it ) {
        CodeBlock* cb = *it;
        for( int i=0; i < cb->locals->getCount(); i++ ) {
            Declaration* d = (Declaration*)cb->locals->get( i );
            if( 0 == strcmp( d->getName(), var->name ) ) {
                bytes.emit( reference ? OP_LEA_W : OP_LOAD_W4 ).emit( LOCAL( d->offset ) );
                return;
            }
        }
    }
    
    if( scope_Function && scope_Function->parameters ) {
        for( int i=0; i < scope_Function->parameters->getCount(); i++ ) {
            Declaration* d = (Declaration*)scope_Function->parameters->get( i );
            if( 0 == strcmp( d->getName(), var->name ) ) {
                bytes.emit( reference ? OP_LEA_W : OP_LOAD_W4 ).emit( ARG( i+1 ) );
                return;
            }
        }
    }
}


// This compiles the expression for 'reading', eg the value is loaded onto the stack
void compile_expression( Expr * expr, bool lhv ) {
    if( lhv && expr->kind != NT_VARIABLE ) {
        // TODO: this will need to be expanded inorder to support things like :
        //
        //    int* a;  a[6] = 56;
        //
        //    int *a;  *a = 56;
        printf("Expression is not assignable: %s", expr->getTypeName() );
    }
    
    if( expr->kind == NT_VARIABLE ) {
        load_variable( (Variable*)expr, lhv );
    } else {
        compile_statement( expr );
    }
}

// core compiler loop
void compile_statement( Node* node )
{
    switch( node->kind )
    {
    case NT_STATEMENT:{
        CodeBlock* cb = (CodeBlock*)node;
        cb->recurse( &compile_statement );

        break;
    }
    case NT_ASSIGNMENT:{
        // proces RHV then emit assigment to RHV storage..
        Assignment* a = (Assignment*)node;
        compile_expression( a->rhv, false );
        compile_expression( a->lhv, true );
        bytes.emit( OP_STOR_P4 ).emit( 0 );
        break;
    }
    case NT_DECLARATION:{
    /*
        // if the variable has an initializer, emit it.
        Declaration* d = (Declaration*)node;
        if( d->initializer ) {
            compile_statement( d->initializer );
        }
    */
        break;
    }
    case NT_INT_LITERAL:{
        Literal* l = (Literal*)node;
        bytes.emit( OP_CONST );
        bytes.emit( l->value.i );
        break;
    }
    case NT_FLOAT_LITERAL:{
        Literal* l = (Literal*)node;
        bytes.emit( OP_CONST );
        bytes.emit( l->value.f );
        break;
    }
/*
    case NT_STRING_LITERAL:{
        Literal* l = (Literal*)node;
        bytes.emit( OP_CONST );
        bytes.emit( l->value.s );
        break;
    }
*/
/*
    case NT_VARIABLE:{
        // load the address, then load the value
        load_address( node );
        //bytes.emit( OP_LOAD_P4 );
        break;
    }
*/
/*
    case NT_EQUALS:{
        // a == b load a,b,compare
        Equals* e = (Equals*)node;
        compile_statement( e->lvh );
        compile_statement( e->rhv );
        bytes.emit( OP_EQ );
        break;
    }
*/
    case NT_PREOPERATOR:{
        // ++/--i - load the value, inc/dec it, storeit, return new value
        PreOperator* p = (PreOperator*)node;
        compile_expression( p->lhv, true );
        // stack A
        bytes.emit( OP_DUP );// dup the address, so it can be used to store to later
        bytes.emit( OP_DUP );// dup the address, so it can be used to store to later
        // stack A,A,A
        bytes.emit( OP_LOAD_P4 ).emit( 0 );
        // stack A,A,OV
        bytes.emit( OP_CONST ).emit( 1 );

        if( *p->op == '+' )
            bytes.emit( OP_ADDI );
        else
            bytes.emit( OP_SUBI );

        // stack A,A,NV
        bytes.emit( OP_STOR_P4 ).emit( 0 );
        // stack A
        bytes.emit( OP_LOAD_P4 ).emit( 0 );
        // stack NV

        break;
    }
    case NT_POSTOPERATOR:{
        // i++/-- load the value, return it, then inc/dec
        PostOperator* p = (PostOperator*)node;
        compile_expression( p->lhv, true );
        // stack A
        bytes.emit( OP_DUP );
        // stack A,A
        bytes.emit( OP_LOAD_P4 );
        // stack A,OV
        bytes.emit( OP_DUP );
        // stack A,OV,OV
        bytes.emit( OP_SWAP ).emit( 2 );
        // stack OV,OV,A
        bytes.emit( OP_SWAP ).emit( 1 );
        // stack OV,A,OV

        // inc/dec value
        bytes.emit( OP_CONST ).emit( 1 );

        if( *p->op == '+' )
            bytes.emit( OP_ADDI );
        else
            bytes.emit( OP_SUBI );

        // stack OV,A,NV
        bytes.emit( OP_STOR_P4 );
        // stack OV
        break;
    }
    case NT_OPERATOR:{
        // eval LHV, RHV, then execute operator
        Operator* o = (Operator*)node;
        compile_expression( o->lhv, false ); // while this is the value on the left, its not a LHV.
        compile_expression( o->rhv, false );
        // TODO: Should operator contain byte instruction?
        // this would allow extended operators, but would also
        // pose issues with typeing.
        // Possibly provide a callback method?
        switch( *o->op )
        {
        case '*':
            bytes.emit( OP_MULI );
            break;
        case '%':
            bytes.emit( OP_DIVI );
            break;
        case '+':
            bytes.emit( OP_ADDI );
            break;
        case '-':
            bytes.emit( OP_SUBI );
            break;
        case '=': // ==
            bytes.emit( OP_EQ );
            break;
        case '!': // !=
            bytes.emit( OP_NEQ );
            break;
        default:
                printf("unhandled operator: %s\n", o->op );
        }
        break;
    }
    case NT_UNARY_OPERATOR:{
        UnaryOperator* o = (UnaryOperator*)node;
        
        switch( *o->op ) {
            case '-' : // 0 - expr
                bytes.emit( OP_CONST ).emit( 0 );
                compile_expression( o->rhv, false );
                bytes.emit( OP_SUBI );
                break;
            case '+' : // bascially a no-op
                compile_expression( o->rhv, false );
                break;
            case '!' :
                compile_expression( o->rhv, false );
                bytes.emit( OP_CONST ).emit( 0 );
                bytes.emit( OP_EQ );
                break;
            default:
                printf("unhandled unary operator: %s\n", o->op );
        }
        
        break;
    }
    case NT_GROUPED: {
        Grouping* g = (Grouping*)node;
        compile_statement(g->expr);
        break;
    }
    case NT_RETURN: {
        Return* r = (Return*)node;
        if( r->expr ) {
            compile_expression( r->expr, false );
            bytes.emit( OP_STOR_W4 ).emit( 0 );
        }
        bytes.emit( OP_LEAVE ).emit(0);
        break;
    }
    default:
        printf("Unhandled node : %s", node->getTypeName() );
    }
}

static int wsize;

void count_locals( Node* node ) {
    if( node->kind == NT_DECLARATION ) {
        ((Declaration*)node)->offset = ++ wsize;
    }
    node->recurse( &count_locals );
}

void compile_function( Function* func )
{
	// count localvars and max # of params passed
	bytes.pcode_index = 0;
    scopes.push_front( func->code );
    scope_Function = func;
    wsize = 0;
    func->code->recurse(&count_locals);
    bytes.emit(OP_ENTER).emit( wsize ); // locals
 	func->code->recurse(&compile_statement);
    scope_Function = NULL;
    scopes.pop_front();
    // implicit return
    bytes.emit(OP_LEAVE).emit( wsize );
	bytes.report();
}

void generate_byte_code( Node* node )
{
	if( node->kind == NT_FUNCTION )
	{
		compile_function( (Function*)node );
	}
	else
		node->recurse(&generate_byte_code);
}

NodeList* lastParsed;

void setRoot(NodeList* root)
{
	lastParsed = root;
	root->recurse(&pre_process);
	root->recurse(&report);
	root->recurse(&generate_byte_code);
}

int* compile( NodeList* root ) {
	if( root == NULL )
		return NULL;
	root->recurse(&pre_process);
	bytes.reset();
	root->recurse(&compile_statement);
	bytes.report();
	return bytes.pcode;
}

int uc_parse();

NodeList* parse() {
	bytes.reset();
	lastParsed = NULL;
	uc_parse();
	return lastParsed;
}
