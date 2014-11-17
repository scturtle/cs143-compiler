/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

a [aA]
b [bB]
c [cC]
d [dD]
e [eE]
f [fF]
g [gG]
h [hH]
i [iI]
j [jJ]
k [kK]
l [lL]
m [mM]
n [nN]
o [oO]
p [pP]
q [qQ]
r [rR]
s [sS]
t [tT]
u [uU]
v [vV]
w [wW]
x [xX]
y [yY]
z [zZ]

DARROW          =>
ASSIGN          <-
LE              <=

%x COMMENT1 COMMENT STRING RESUME
  int nesting = 0;
  int string_cnt = 0; /* TODO: remove by ptr subtraction */

%%

"--"         { BEGIN(COMMENT1); }
<COMMENT1>\n { BEGIN(INITIAL); ++curr_lineno; }
<COMMENT1>.  ;

 /* digits */
[0-9]+ {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

 /*
  *  Nested comments
  */

"(*"              { BEGIN(COMMENT); nesting = 1; }
"*)"              { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }
<COMMENT>"(*"     { nesting++; }
<COMMENT>"*)"     { if(--nesting == 0) BEGIN(INITIAL); }
<COMMENT>\n       { ++curr_lineno; }
<COMMENT>.        ;
<COMMENT><<EOF>>  { cool_yylval.error_msg = "EOF in comment"; BEGIN(INITIAL); return ERROR; }

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return DARROW; }
{ASSIGN}		{ return ASSIGN; }
{LE}            { return LE; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{c}{l}{a}{s}{s}             { return CLASS; }
{e}{l}{s}{e}                { return ELSE; }
{f}{i}                      { return FI; }
{i}{f}                      { return IF; }
{i}{n}                      { return IN; }
{i}{n}{h}{e}{r}{i}{t}{s}    { return INHERITS; }
{i}{s}{v}{o}{i}{d}          { return ISVOID; }
{l}{e}{t}                   { return LET; }
{l}{o}{o}{p}                { return LOOP; }
{p}{o}{o}{l}                { return POOL; }
{t}{h}{e}{n}                { return THEN; }
{w}{h}{i}{l}{e}             { return WHILE; }
{c}{a}{s}{e}                { return CASE; }
{e}{s}{a}{c}                { return ESAC; }
{n}{e}{w}                   { return NEW; }
{o}{f}                      { return OF; }
{n}{o}{t}                   { return NOT; }

t{r}{u}{e}                  { cool_yylval.boolean = true; return BOOL_CONST; }
f{a}{l}{s}{e}               { cool_yylval.boolean = false; return BOOL_CONST; }

 /* identifier */
[a-zA-Z][a-zA-Z0-9_]* {
    cool_yylval.symbol = idtable.add_string(yytext);
    if(yytext[0] >= 'A' && yytext[0] <= 'Z')
        return TYPEID;
    else
        return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

"\"" {
    BEGIN(STRING);
    string_cnt = 0;
    string_buf_ptr = string_buf;
}

<STRING>\n {
    ++curr_lineno;
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unterminated string constant";
    return ERROR;
}

 /* (escaped) character in string */

<STRING>\\[\0-\255]|[^"\""] {
    if(string_cnt + 1 == MAX_STR_CONST){
        BEGIN(RESUME);
        cool_yylval.error_msg = "String constant too long";
        return ERROR;
    }
    char c;
    if(yytext[0] == '\\'){
        switch(yytext[1]){ // escaped
            case '\0': 
                if(string_cnt == 0){
                    c == 0; break;
                } else {
                    BEGIN(RESUME);
                    cool_yylval.error_msg = "String contains null character";
                    return ERROR;
                }
            case '\n': ++curr_lineno;
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'b': c = '\b'; break;
            case 'f': c = '\f'; break;
            default: c = yytext[1];
        }
    } else {
        if(yytext[0] == '\0'){
            BEGIN(RESUME);
            cool_yylval.error_msg = "String contains null character";
            return ERROR;
        } else {
            c = yytext[0];
        }
    }
    ++string_cnt;
    *string_buf_ptr = c;
    ++string_buf_ptr;
}

<STRING><<EOF>> { cool_yylval.error_msg = "EOF in string constant"; BEGIN(INITIAL); return ERROR; }

<RESUME>\n   { BEGIN(INITIAL); ++curr_lineno; }
<RESUME>"\"" { BEGIN(INITIAL); }
<RESUME>.    ;

<STRING>"\"" {
    string_buf[string_cnt] = 0;
    cool_yylval.symbol = stringtable.add_string(string_buf);
    BEGIN(INITIAL);
    return STR_CONST;
}

 /* symbol */
[-+*/~<\(\){}@=\.,:;] { return yytext[0]; }

 /* white space */
\n          { ++curr_lineno; }
[ \f\r\t\v] ;

 /* invalid character */
. {
    char *s = new char[2];
    strcpy(s, yytext);
    cool_yylval.error_msg = s;
    return ERROR;
}

%%
