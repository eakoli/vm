
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <stdarg.h>

#include <assembler/assembler.h>
#include <vm/vm.h>

#ifndef NULL
	#define NULL (void*)0
#endif

extern "C" FILE *uc_in;  // the input stream
extern int uc_parse ();
extern "C" void uc_restart (FILE*);

// The main program: just report all tokens found
int uc_main (int argc, char *argv[])  {
   // Set the input stream (either a file from the command line or stdin)

   uc_in = NULL;
   if (argc == 2)
      uc_in = fopen (argv[1], "rt");
   if (uc_in == NULL)
      uc_in = stdin;

   uc_parse (); // call the parser

   return 0;
}

extern int test_vm( int argc, char *argv[] );

struct s_test {
	int a,b,c,d,e,g;
};

s_test z,w;

extern "C" void pass_s_test( s_test s )
{
	s.a = 1;
}

extern "C" s_test get_s_test()
{

	//pass_s_test(s);

	return w;
}

extern "C" int foo(int a,int b,int c,int d,int e,int f,int g)
{
	int q,r,s,t;
	return a;
}

void test()
{
	int a,b,c,d;

	a = foo(1,2,3,foo(1,2,3,4,5,6,7),foo(1,2,3,4,5,6,7),foo(1,2,3,4,5,6,7),5);
	c = foo(1,2,3,4,5,6,7);
	//s = get_s_test();
	w = get_s_test();
	w = z;
	//w = s;
	//printf("%d", s.a);
}


int call( int args, ... ) {

}

static const char* yy_string = NULL;
static FILE* yy_file = NULL;

int yy_input_file(char* buffer, int max ){
	int err = 0;
	int result;
	while( (result = fread(buffer,1,max,yy_file))==0 && ferror(yy_file))
	{
		if( err != EINTR ){
			printf("input reading failed !!!");
			return 0;
		}
		err = 0;
		clearerr(yy_file);
	}
	return result;
}

int yy_input_string(char* buffer, int max ){
	if( !*yy_string )
		return 0;

	int read = strlen( yy_string );
	if( read > max )
		read = max;

	strncpy(buffer, yy_string, read);

	yy_string += read;

	return read;
}

/**
 * implements yyinput
 */
extern "C" int yy_input(char* buffer, int max ){
	if( yy_file != NULL )
		return yy_input_file( buffer, max );
	else if( yy_string != NULL )
		return yy_input_string( buffer, max );
	else
		return 0;
}

extern "C" void yyreset();

int* compile() {
	uc_parse();
}

int* compile( const char* script ) {
    yy_file = NULL;
	yy_string = script;
	uc_restart(NULL);
	yyreset();

	return compile(parse());
}

int* compile( FILE* script ) {
	yy_file = script;
    yy_string = NULL;
	uc_restart(NULL);
	yyreset();
	//uc_parse();
    
    return compile(parse());
}

int run_jit( int* code, ... );

int run( int* code, ... ) {
	vm_context context;

	// execution expects a result parameter as the first arg
	// so we must backup one int before the first arg, save
	// that value and restore it when were done
	// In this case we jsut start the param list at "code", as it will be before the first arg.
	//int* params = ((int*)&code);
    
    // This whole thing wont work well, as floats are passed as dobules, thus take up 64bits not 32.
    // plus its probably compiler dependent as to how its passed.
    // this means we need to know HOW the values are passed, what each is so it can be copied to a correct flat model.
    // OR we do fixups on args when we load the byte code and in jit (YUCK)
    
    // IDEA...
    //
    // union VAR { int; float; etc };
    //
    // run_code( int* code, VAR a = 0, VAR b = 0, VAR c = 0, VAR d = 0, ... );
    //
    // template< type RT, type T>
    // RT run( int* code, T arg1 ) { return (RT)run_code( code, (VAR)arg1 );
    //
    
    
    va_list v_args;
    va_start( v_args, code );
    
    int* params = (int*)( ((int)v_args) - sizeof(int) );

	context.execute(code, params);

	int ret = *params;

	return ret;
}

/**
 * run in interactive mode
 *
 */
void interactive() {
	while(true) {
		printf(">");

		char buffer[4096] = {'\0'};
		char*p = buffer;

		int c;
		while( (c = getchar()) != -1 )
		{
			if( c == '\n' )
				break;

			*p++ = (char)c;
		}
		*p++ = '\0';

		if( c == -1 || strcmp(buffer,"quit")==0 )
		{
			printf("exit requested\n");
			return;
		}

		int* code = compile(buffer);

		//run( code );

		printf("\n");
	}
}

int test_vm( int argc, char* argv[] );

int main( int argc, char *argv[] )
{
	if( argc <= 1 )
		interactive();
	else {
        //test_vm(0,0);
        
		uc_main(argc,argv);
		int * code = compile(fopen(argv[1],"rt"));
		return run_jit( code, 12, 56 );
	}

	return 0;
}
