%{
/* rom.l: Roman numerals, trickier yacc, simpler lex
 * R Dowling 96Oct20
 */

#include "uc_grammer.yacc.hpp"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#define YY_INPUT( buf, result, max_size )\
{ \
    result = yy_input(buf, max_size );\
    if( result == 0 )\
        result = YY_NULL; \
}

int yy_input( char*, int );

#define yylval uc_lval
#define yyltext uc_text
#define yyerror uc_error
int error_code;
void yyerror(const char*, ... );
#define yywarning( ... ) yyerror( "warning: " __VA_ARGS__ )

static void count ();
static void comment(char type);
%}


STRING			\"([^\"]|(\\\"))*\"
NAME			[a-zA-Z_][a-zA-Z_0-9]*
OPERATOR_NAME	[-+*\/~!$%&<>|][-+*\/~!$%&<>|=]{0,1}

%%

[ \t\n]			{ count(); }

\/\/.           { comment(yytext[1]); }
\/\*.           { comment(yytext[1]); }

0x[0-9A-F]{1,4} |
[0-9]+			{ count(); yylval.i = atoi(yytext); return L_INT; }

[0-9]+f			|
[0-9]+\.[0-9]+	{ count(); yylval.f = atof(yytext); return L_FLOAT; }

"struct"		{ count(); return K_STRUCT; }
"class"			{ count(); return K_CLASS; }
"state"			{ count(); return K_STATE; }
"enum"			{ count(); return K_ENUM; }

"native"		{ count(); return K_NATIVE; }
"private"		{ count(); return K_PRIVATE; }
"const"			{ count(); return K_CONST; }
"final"			{ count(); return K_FINAL; }

"noproxy"		{ count(); return K_NOPROXY; }
"nostub"		{ count(); return K_NOSTUB; }
"abstract"		{ count(); return K_ABSTRACT; }
"within"		{ count(); return K_WITHIN; }
"extends"		{ count(); return K_EXTENDS; }
"nativereplication" { count(); return K_NATIVE_REPLICATION; }

"ignores"		{ count(); return K_IGNORES; }

"virtual"		{ count(); return K_VIRTUAL; }
"static"		{ count(); return K_STATIC; }
"operator"		{ count(); return K_OPERATOR; }
"preoperator"	{ count(); return K_PREOPERATOR; }

"optional"		{ count(); return K_OPTIONAL; }
"skip"			{ count(); return K_SKIP; }
"out"			{ count(); return K_OUT; }

"config"		{ count(); return K_CONFIG; }
"defaults"		{ count(); return K_DEFAULTS; }

"new"			{ count(); return K_NEW; }
"while"			{ count(); return K_WHILE; }
"for"			{ count(); return K_FOR; }
"if"			{ count(); return K_IF; }
"else"			{ count(); return K_ELSE; }
"do"			{ count(); return K_DO; }
"switch"		{ count(); return K_SWITCH; }
"case"			{ count(); return K_CASE; }
"default"		{ count(); return K_DEFAULT; }
"return"		{ count(); return K_RETURN; }
"until"			{ count(); return K_UNTIL; }
"false"			{ count(); return K_FALSE; }
"true"			{ count(); return K_TRUE; }
"break"			{ count(); return K_BREAK; }
"continue"		{ count(); return K_CONTINUE; }
"None"			{ count(); return K_NONE; }

"void"			{ count(); return K_VOID; }
"int"			{ count(); return K_INT; }
"float"			{ count(); return K_FLOAT; }
"array"			{ count(); return K_ARRAY; }
"iterator"		{ count(); return K_ITERATOR; }

"=="			{ count(); return OP_EQUAL; }
","				|
"{"				|
"}"				|
"("				|
"="				|
")"				|
"["				|
"]"				|
":"				|
";"				{ count(); return yylval.i = *yytext; }

{NAME}			{ count(); yylval.s = strdup(yytext); return L_NAME; }
{STRING}		{ count(); yylval.s = strdup(yytext); return L_STRING; }

"--"            { count(); yylval.s = strdup(yytext); return OP_DECREMENT; }
"++"            { count(); yylval.s = strdup(yytext); return OP_INCREMENT; }

[-+*\/~!$%&<>|] { count(); yylval.s = strdup(yytext); return yylval.s[0]; }


.				{ count(); yyerror("Unknown character (%c)", *yytext); }

%%

/*
 "-"             | { count(); yylval.s = strdup(yytext); return OP_MINUS; }
 "+"             | { count(); yylval.s = strdup(yytext); return OP_PLUS; }
 "!"             | { count(); yylval.s = strdup(yytext); return OP_LOGICAL_NOT; }
 "~"             | { count(); yylval.s = strdup(yytext); return L_BITWISE_NOT; }
 
 {OPERATOR_NAME}	{ count(); yylval.s = strdup(yytext); return L_OP; }

 */
static int column = 0;
static int line_num = 1;
static char error_line_buffer[256];
static char *elb=error_line_buffer;

/* Count makes pretty error messages by counting characters and saving
 * them in a global error_line_buffer.  yyerror() below knows about the
 * buffer and can print out the partial line with the error.
 */
void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
	{
		if (yytext[i] == '\n')
		{
			column = 0;
			line_num++;
			elb = error_line_buffer;
		}
		else
		{
			*elb++ = yytext[i];
			if (yytext[i] == '\t')
				column += 8 - (column & (8-1));
			else
				column++;
		}
		*elb = 0;
	}
}

void comment(char type ) {
    /* "/*" */
    count();
    
    int have_asterisk = type == '*' && yytext[2] == '*';
    int have_slash = type == '*' && yytext[2] == '/';
    
    yytext[1] = 0;
    
    while( (*yytext=input()) != 0 ) {
        count();
        
        if( have_asterisk && *yytext == '/' )
        {
            // got */
            return;
        } else if( have_slash && *yytext == '*' )
        {
            yywarning( "/* within block comment" );
        }
        have_asterisk = 0;
        have_slash = 0;
        if( type == '*' && *yytext == '/' ) {
            have_slash = 1;
        }
        else if( type == '*' && *yytext == '*' ) {
            have_asterisk = 1;
        }
        else if( type == '/' && *yytext == '\n' )
        {
            return;
        }
    }
    
    if( type == '*' )
    yyerror("unterminated comment");
}

void yyreset() {
    line_num = 1;
    column = 0;
    error_line_buffer[0] = '\0';
}

void yyerror(const char *fmt, ...)
{
	/* extern int yylineno; */
	extern int column, line_num;
	extern char error_line_buffer[256];
	va_list va;

	va_start(va, fmt);
	fprintf (stdout, "%s\n", error_line_buffer);
	fprintf (stdout, "\n%*s ", column, "^");
	vfprintf(stdout, fmt, va);
	fprintf (stdout, ":Line %d\n", line_num);
	error_code = 26;
}

int romanwrap()
{
	return 1; 
}
