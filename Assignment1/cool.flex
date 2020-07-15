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
 
/*
* "self" {}
* "SELF_TYPE" {}
*/ 

int comment_deep = 0;
int error_string_flag = 0;

int g_buff_count = 0;

int check_copy_buf_ptr_char(char *ptr, char value) {
	g_buff_count++;
	*ptr = value;
	if (g_buff_count >= MAX_STR_CONST) {
		error_string_flag = 1;
		g_buff_count = 0;
		return 0;
	}
	return 1;
}
%}

/*
 * Define names for regular expressions here.
 */

%x COMMENT
%x str

 
DARROW          =>
ASSIGN			<-
LE		 		<=

DIGITS	[0-9]+
NAME [0-9a-zA-Z]+
TYPE_IDENTIFIER [A-Z][a-zA-Z0-9|_]*
OBJECT_IDENTIFIER [a-z][0-9a-zA-Z|_]*
STRING \"[^\"]*\"
MULTILINE_COMMENTS \(\*[^\*\)]*\*\)
SINGLE_LINE_COMMENT --.*

FALSE	f[aA][lL][sS][eE]
TRUE	t[rR][uU][eE]
WHITESPACE [ \t\r\f\v]*



%%

"(*"            { comment_deep++ ; BEGIN(COMMENT); }
<COMMENT>\n             { curr_lineno++; }
<COMMENT>"("+"*"        { comment_deep++; }
<COMMENT>"*"+")"        { comment_deep--; if(!comment_deep) { BEGIN(INITIAL); } }
<COMMENT>.              { }
<COMMENT><<EOF>>  {	
					cool_yylval.error_msg = "EOF in comment"; 
					BEGIN(INITIAL); 
					return ERROR;				
				}

{SINGLE_LINE_COMMENT}	{ }

"*)"  { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }


"(" 					{  return '('; }
")" 					{  return ')'; }
";" 					{  return ';'; }
"," 					{  return ','; }
"{" 					{  return '{'; }
"}" 					{  return '}'; }
":" 					{  return ':'; }
"+" 					{  return '+'; }
"-" 					{  return '-'; }
"*" 					{  return '*'; }
"/" 					{  return '/'; }
"." 					{  return '.'; }
"@" 					{  return '@'; }
"~" 					{  return '~'; }
"<" 					{  return '<'; }
"=" 					{  return '='; }
{ASSIGN}				{ return ASSIGN; } 
{LE}					{ return LE; }

"!"						{ cool_yylval.error_msg = "!";  return ERROR; }
"#"						{ cool_yylval.error_msg = "#";  return ERROR; }
"$"						{ cool_yylval.error_msg = "$";  return ERROR; }
"%"						{ cool_yylval.error_msg = "%";  return ERROR; }
"^"						{ cool_yylval.error_msg = "^";  return ERROR; }
"&"						{ cool_yylval.error_msg = "&";  return ERROR; }
"_"						{ cool_yylval.error_msg = "_";  return ERROR; }
">"						{ cool_yylval.error_msg = ">";  return ERROR; }
"?"						{ cool_yylval.error_msg = "?";  return ERROR; }
"`"						{ cool_yylval.error_msg = "`";  return ERROR; }
"["						{ cool_yylval.error_msg = "[";  return ERROR; }
"]"						{ cool_yylval.error_msg = "]";  return ERROR; }
"\\"					{ cool_yylval.error_msg = "\\";  return ERROR; }
"|"						{ cool_yylval.error_msg = "|";  return ERROR; }


\1  { cool_yylval.error_msg = "\\001";  return ERROR; }
\2  { cool_yylval.error_msg = "\\002";  return ERROR; }
\3  { cool_yylval.error_msg = "\\003";  return ERROR; }
\4  { cool_yylval.error_msg = "\\004";  return ERROR; }

{TRUE} { cool_yylval.boolean = 1; return BOOL_CONST; }
{FALSE} { cool_yylval.boolean = 0; return BOOL_CONST; }


^(?i:"class")    		{ return CLASS;}
(?i:"else")  			{ return ELSE; }
(?i:"fi")				{ return FI; }
(?i:"if")				{ return IF; }
(?i:"in") 				{ return IN; }
(?i:"inherits") 		{ return INHERITS; }
(?i:"let") 				{ return LET; }
(?i:"loop") 			{ return LOOP; }
(?i:"pool") 			{ return POOL; }
(?i:"then") 			{ return THEN; }
(?i:"while") 			{ return WHILE; }
(?i:"case")  			{ return CASE; }
(?i:"esac") 			{ return ESAC; }
(?i:"of")				{ return OF; }
{DARROW}                {  return (DARROW); }
(?i:"new") 				{  return NEW; }
(?i:"isvoid")			{  return ISVOID; }
(?i:"not")				{  return NOT; }


{DIGITS}  { cool_yylval.symbol = inttable.add_string(yytext); return INT_CONST; }

{TYPE_IDENTIFIER}   {
	 cool_yylval.symbol = idtable.add_string(yytext); return TYPEID;  break;				
	}


{OBJECT_IDENTIFIER} {
		cool_yylval.symbol = idtable.add_string(yytext); return OBJECTID;  break;	
}


\"    string_buf_ptr = string_buf;  g_buff_count = 0; BEGIN(str);

    <str>\"        { /* saw closing quote - all done */
            BEGIN(INITIAL);
            *string_buf_ptr = '\0';
            /* return string constant token type and
             * value to parser
             */
			if (error_string_flag) {
				error_string_flag = 0;				
			} else {
				cool_yylval.symbol = stringtable.add_string(string_buf);
				return STR_CONST;
			}
           }
			
   
			
	<str>\n  { 
			BEGIN(INITIAL);
			if (!error_string_flag) {
				cool_yylval.error_msg = "Unterminated string constant"; 
				return ERROR; 
			}
	}
	
	<str>\\\0  { cool_yylval.error_msg = "String contains escaped null character."; error_string_flag = 1; return ERROR; }
	
	<str>\0  { cool_yylval.error_msg = "String contains null character."; error_string_flag = 1; return ERROR; }
	
	<str><<EOF>>  {	
					cool_yylval.error_msg = "EOF in string constant"; 
					BEGIN(INITIAL); 
					return ERROR;				
				}
	
	
    <str>\\n  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, '\n')) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 					
					return ERROR;
					}
				} 
				
    <str>\\t  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, '\t')) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 
					return ERROR;
					}
				}
    <str>\\b  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, '\b')) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 					
					return ERROR;
					}
				}
    <str>\\f  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, '\f')) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 				
					return ERROR;
					}
				}
	<str>\\v  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, '\v')) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 					
					return ERROR;
					}
				}

    <str>\\(.|\n)  { 
				if (check_copy_buf_ptr_char(string_buf_ptr, yytext[1])) {
					string_buf_ptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 					
					return ERROR;
					}
				}

    <str>[^\\\n\"\0]+        {
            char *yptr = yytext;
			int i = 0;
			while ( *yptr ) {
				if (check_copy_buf_ptr_char(string_buf_ptr, *yptr)) {
					string_buf_ptr++;
					*yptr++;
					}
				else {
					cool_yylval.error_msg = "String constant too long"; 					
					return ERROR;
					}
				}				
			}
	
			

\n		{ curr_lineno++; }
	
{WHITESPACE} {}




%%
