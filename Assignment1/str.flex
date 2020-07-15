
{STRING}   {
	  char *x = yytext; int l = strlen(x); x[l-1] = '\0'; x++; cool_yylval.symbol = stringtable.add_string(x); return STR_CONST;  break;
	}




\"    string_buf_ptr = string_buf; BEGIN(str);

    <str>\"        { /* saw closing quote - all done */
            BEGIN(INITIAL);
            *string_buf_ptr = '\0';
            /* return string constant token type and
             * value to parser
             */
			cool_yylval.symbol = stringtable.add_string(string_buf);
			return STR_CONST;
            }

    <str>\n        {
            /* error - unterminated string constant */
            /* generate error message */
            }

    <str>\\[0-7]{1,3} {
            /* octal escape sequence */
            int result;

            (void) sscanf( yytext + 1, "%o", &result );

            if ( result > 0xff )
                    /* error, constant is out-of-bounds */

            *string_buf_ptr++ = result;
            }

    <str>\\[0-9]+ {
            /* generate error - bad escape sequence; something
             * like '\48' or '\0777777'
             */
            }

    <str>\\n  { cool_yylval.error_msg = "Unterminated string constant"; BEGIN(INITIAL); return ERROR; }
    <str>\\t  { }
    <str>\\r  { }
    <str>\\v  { }
    <str>\\f  { }

    <str>\\(.|\n)  *string_buf_ptr++ = yytext[1];

    <str>[^\\\n\"]+        {
            char *yptr = yytext;

            while ( *yptr )
                    *string_buf_ptr++ = *yptr++;

            }