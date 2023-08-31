%{
/*
 * File Name   : subc.l
 * Description : a skeleton flex input
 */

#include "subc.h"
#include <stdarg.h>
#include "subc.tab.h"
static int comment_depth = 0;
static int lineno = 1;
static int error_line = 0;
static int inside_array_def = 0;
static char *filename;
static FILE *output;

%}

/* regular definitions */
letter          [A-Za-z_]
digit           [0-9]
id              {letter}({letter}|{digit})*
integer_const   ([1-9]{digit}*)|"0"
white_space     [ \t]+
special_char    \\[nt]
relop           [\<\>]=?
equop           [\=\!]=
miscop          [\*\/\%\+\-\&\!\[\]\{\}\(\)\;\=\,\.]

%x COMMENT
%%
"void"  { return VOID; }
{id} {
           struct id *entered  = enter(ID, yytext, yyleng);
           yylval.id = entered;
           return entered->tokenType;
        }
{integer_const} {
           yylval.intVal = atoi(yytext);
           return INTEGER_CONST;
        }
\"({special_char}|[^\\\"\n])*\" {
           /* String Variable */
           yylval.id = enter(STRING, yytext, yyleng);
           return STRING;
        }
'({special_char}|[^\\'\n])' {
           /* Character Variable */
           char c;
           if (yytext[1] == '\\') {
               if (yytext[2] == 'n')
                   c = '\n';
               else
                   c = '\t';
           } else
               c = yytext[1];

           yylval.charVal = c;
           return CHAR_CONST;
        }
{relop} {
           yylval.id = enter(RELOP, yytext, yyleng);
           return RELOP;
        }
{equop} {
           yylval.id = enter(EQUOP, yytext, yyleng);
           return EQUOP;
        }
"++"    { return INCOP; }
"--"    { return DECOP; }
"&&"    { return LOGICAL_AND; }
"||"    { return LOGICAL_OR; }
"->"     { return STRUCTOP; }
{miscop} { return yytext[0]; }

{white_space} ;
<*>\n   ++lineno;
<INITIAL,COMMENT>"/*" {
           ++comment_depth;
           BEGIN (COMMENT);
        }
<COMMENT>"*/" if(--comment_depth == 0) BEGIN (INITIAL);
<COMMENT>.    ;


%%
void print_error(const char *msg)
{
    if (lineno > error_line) {
        printf("%s:%d:error:%s\n", filename, lineno, msg);
        error_line = lineno;
    }
}

void turn_on_arr(void)
{
    inside_array_def = 1;
}

void turn_off_arr(void)
{
    inside_array_def = 0;
}

void print_code(const char *fmt, ...)
{
    va_list args;
    if (inside_array_def)
        return;
    va_start(args, fmt);
    if (output)
        vfprintf(output, fmt, args);
    else
        vfprintf(stdout, fmt, args);
    va_end(args);
}

int main(int argc, char* argv[])
{
    static char *keyword[] = { "int", "char", "struct", "return", "break",
                              "continue", "if", "else", "while", "for", "NULL",
                              NULL };
    static int tokentype[] = { TYPE, TYPE, STRUCT, RETURN, BREAK,
                               CONTINUE, IF, ELSE, WHILE, FOR, NULLPTR, 0 };
    int i;

    for(i = 0; keyword[i] != NULL; i++)
        enter(tokentype[i], keyword[i], strlen(keyword[i]));

    if (argc >= 2) {
        yyin = fopen(argv[1], "r");
        filename = argv[1];
        if (argc >= 3)
            output = fopen(argv[2], "w");
    } else {
        yyin = stdin;
        filename = "stdin";
    }

    if(!yyin) {
        printf("Can't open input stream!\n");
        exit(1);
    }

    yyparse();
    fclose(yyin);
    if (output)
        fclose(output);
    return 0;
}
