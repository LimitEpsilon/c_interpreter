%option noyywrap

CONSONANT [b-df-hj-np-tv-zB-DF-HJ-NP-TV-Z]

VOWEL [aeiouAEIOU]

ALPHA [a-zA-Z]

WS [ \t\n\r]

%%

{WS} { printf("%s", yytext); }

{CONSONANT}{ALPHA}* { printf("%s%cay", yytext + 1, *yytext); }

{VOWEL}{ALPHA}* { printf("%say", yytext); }

%%

int main (int argc, char **argv)
{
    ++argv, --argc;
    if (argc > 0)
       yyin = fopen(argv[0], "r");
    else
        yyin = stdin;
    yylex();
}
