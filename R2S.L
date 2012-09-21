 /* A FLEX program for translating a language like REBOL into
    a fully parenthesized notation.
    Scanner is 8-bit, case insensitive, and needs no default rule.
    Scanner uses REJECT, so it cannot be generated with `f' option.

 Copyright (c) 1999 Emergent Technologies, Inc.

 Permission to copy this software, to redistribute it, and to use it
 for any purpose is granted, subject to the following restrictions
 and understandings.

 1. Any copy made of this software must include this copyright
    notice in full.

 2. Users of this software agree to make their best efforts (a) to
    return to Emergent Technologies, Inc. any improvements or
    extensions that they make, so that these may be included in future
    releases; and (b) to inform Emergent Technologies, Inc. of
    noteworthy uses of this software.

 3. All materials developed as a consequence of the use of this
    software shall duly acknowledge such use, in accordance with the
    usual standards of acknowledging credit in academic research.

 4. Emergent Technologies, Inc. has made no warrantee or
    representation that the operation of this software will be
    error-free, and MIT is under no obligation to provide any services,
    by way of maintenance, update, or otherwise.

 5. In conjunction with products arising from the use of this
    material, there shall be no use of the name of Emergent
    Technologies, Inc.  nor of any adaptation thereof in any
    advertising, promotional, or sales literature without prior
    written consent from Emergent Technologies, Inc. in each case.

    Emergent Technologies, Inc.  is not affiliated with REBOL Technologies.

    ----------

    This program generates a scanner for a language like REBOL.  The
    generated scanner will tokenize a language which uses lexical
    conventions similar to those adopted by REBOL.  Note that there some
    differences: There are valid REBOL programs that this program will not
    recognize, and there are programs that this program recognizes that
    are not valid REBOL programs.  Nonetheless, there is a substantial
    set of programs that are valid both to REBOL and to this program.

    Some differences are accidental.  REBOL had no formal syntax
    specification at the time this program was written.  Other
    differences are deliberate.  This is to either extend compliance with
    standard convention, or to deprecate certain non-standard lexical 
    conventions.
    
    Here are some of the extensions:

    In a `header', any five letter word beginning with R and having
    at least one L is permitted.  Thus REBOL, REBEL, RUBLE, RUBBL, RBOLA,
    are acceptable.

    Combined ISO dates and times are supported.  

    Extended characters are allowed in words.  This is to support
    correct parsing of Jeff's `Alien' script, even though there are
    invalid words in that script.

    Here are some restrictions:

    The number before a `binary' must be 2, 8, 16, or 64.  If omitted,
    16 is assumed.  Rationale:  other bases are not common and are
    confusing.

    Only URL schemes beginning with standard names such as http, ftp,
    etc. are recognized.

    Ideas for future development

    1. `Issue' should be deprecated.  It is easily subsumed by symbols.

    2.  file should be deprecated, suggest using file://

    3.  email should be deprecated, suggest using mailto: 

    4.  A named quantity type should be developed.  This would be
        able to handle things like `16 tons', `2hr25m18s', `18 cm' 
        and money.

    5.  Dichotomy between strings and long strings should be 
        eliminated.

    6.  A default coding should be used, suggest UTF-8.

    7.  An escape character should be defined.  (and it should be
        `\`) This will allow you to embed unusual characters in elements.

    8.  A multi-character escape should de defined so that
        unusual symbols can be created.

    9.  A locale identifier would be good.  Locale id would allow
        default money amount, choose comma or dot grouping, etc.

   10.  2 digit years should be eliminated.

   11.  A generic read mechanism, so that non-standard things can be
        read.  For instance, a two element dotted, or an email with
        invalid characters. (see 7)

 */

%x BODY QSTR LSTR LBIN
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* The basic token types.  The BOGUS types generally match illegal trailing
** context so as to avoid having things like  foo12:22:33bar get split into
** three tokens.
*/

enum token_type {
    TT_BINARY,
    TT_BLOCK_BEGIN,
    TT_BLOCK_END,
    TT_BOGUS_BINARY,
    TT_BOGUS_DATE,
    TT_BOGUS_DATETIME,
    TT_BOGUS_EMAIL,
    TT_BOGUS_INTEGER,
    TT_BOGUS_ISODATE,
    TT_BOGUS_ISODATETIME,
    TT_BOGUS_ISOTIME,
    TT_BOGUS_QSTRING,
    TT_BOGUS_TIME,
    TT_BOGUS_DOTTED,
    TT_CHAR,
    TT_COMMENT,
    TT_DATE,
    TT_DATETIME,
    TT_DOTTED,
    TT_EMAIL,
    TT_FILE,
    TT_FLOAT,
    TT_HEADER,
    TT_HEADER1,
    TT_INTEGER,
    TT_ISODATE,
    TT_ISODATETIME,
    TT_ISOTIME,
    TT_ISSUE,
    TT_LSTRING,
    TT_MONEY,
    TT_NEWLINE,
    TT_PAREN_BEGIN,
    TT_PAREN_END,
    TT_QSTRING,
    TT_SYMBOL,
    TT_TIME,
    TT_UNMATCHED,
    TT_URL,
    TT_WHITESPACE
};

struct token {
    enum token_type tt;
    int line_number;
    char const *start;
    int length;
};

#define YY_DECL  int yylex(struct token *tok)

#define GOT_TOKEN(type)              \
    do {                             \
        tok->tt = (type);            \
        tok->line_number = yylineno; \
        tok->start = yytext;         \
        tok->length = yyleng;        \
        return 1;                    \
    } while (0)              

#define QSTRING()                                 \
    do {                                          \
        tok->tt = (TT_QSTRING);                   \
        tok->line_number = yylineno;              \
        tok->start = qstring_start;               \
        tok->length = yytext - qstring_start;     \
        return 1;                                 \
    } while (0)              

#define BOGUS_QSTRING()                           \
    do {                                          \
        tok->tt = (TT_BOGUS_QSTRING);             \
        tok->line_number = yylineno;              \
        tok->start = qstring_start;               \
        tok->length = yytext - qstring_start;     \
        return 1;                                 \
    } while (0)              

#define LSTRING()                                 \
    do {                                          \
        tok->tt = (TT_LSTRING);                   \
        tok->line_number = lstring_start_line;    \
        tok->start = qstring_start;               \
        tok->length = yytext - qstring_start;     \
        return 1;                                 \
    } while (0)              

#define BINARY()                                  \
    do {                                          \
        tok->tt = (TT_BINARY);                    \
        tok->line_number = lstring_start_line;    \
        tok->start = qstring_start;               \
        tok->length = yytext - qstring_start + 1; \
        return 1;                                 \
    } while (0)              

#define BOGUS_BINARY()                            \
    do {                                          \
        tok->tt = (TT_BOGUS_BINARY);              \
        tok->line_number = lstring_start_line;    \
        tok->start = qstring_start;               \
        tok->length = yytext - qstring_start + 1; \
        return 1;                                 \
    } while (0)              


#define BLOCK_BEGIN()       GOT_TOKEN(TT_BLOCK_BEGIN)
#define BLOCK_END()         GOT_TOKEN(TT_BLOCK_END)
#define BOGUS_DATE()        GOT_TOKEN(TT_BOGUS_DATE)
#define BOGUS_DATETIME()    GOT_TOKEN(TT_BOGUS_DATETIME)
#define BOGUS_EMAIL()       GOT_TOKEN(TT_BOGUS_EMAIL)
#define BOGUS_INTEGER()     GOT_TOKEN(TT_BOGUS_INTEGER)
#define BOGUS_ISODATE()     GOT_TOKEN(TT_BOGUS_ISODATE)
#define BOGUS_ISODATETIME() GOT_TOKEN(TT_BOGUS_ISODATETIME)
#define BOGUS_ISOTIME()     GOT_TOKEN(TT_BOGUS_ISOTIME)
#define BOGUS_TIME()        GOT_TOKEN(TT_BOGUS_TIME)
#define BOGUS_DOTTED()      GOT_TOKEN(TT_BOGUS_DOTTED)
#define CHAR()              GOT_TOKEN(TT_CHAR)
#define COMMENT()           GOT_TOKEN(TT_COMMENT)
#define DATE()              GOT_TOKEN(TT_DATE)
#define DATETIME()          GOT_TOKEN(TT_DATETIME)
#define DOTTED()            GOT_TOKEN(TT_DOTTED)
#define EMAIL()             GOT_TOKEN(TT_EMAIL)
#define FILE()              GOT_TOKEN(TT_FILE)
#define FLOAT()             GOT_TOKEN(TT_FLOAT)
#define HEADER()            GOT_TOKEN(TT_HEADER)
#define HEADER1()           GOT_TOKEN(TT_HEADER1)   
#define INTEGER()           GOT_TOKEN(TT_INTEGER)
#define ISODATE()           GOT_TOKEN(TT_ISODATE)
#define ISODATETIME()       GOT_TOKEN(TT_ISODATETIME)
#define ISOTIME()           GOT_TOKEN(TT_TIME)
#define ISSUE()             GOT_TOKEN(TT_ISSUE)
#define MONEY()             GOT_TOKEN(TT_MONEY)
#define NEWLINE()           GOT_TOKEN(TT_NEWLINE)
#define PAREN_BEGIN()       GOT_TOKEN(TT_PAREN_BEGIN)
#define PAREN_END()         GOT_TOKEN(TT_PAREN_END)
#define UNMATCHED()         GOT_TOKEN(TT_UNMATCHED) 
#define TIME()              GOT_TOKEN(TT_TIME)
#define URL()               GOT_TOKEN(TT_URL)
#define WHITESPACE()        GOT_TOKEN(TT_WHITESPACE)
#define SYMBOL()            GOT_TOKEN(TT_SYMBOL)

int yylineno;
int lstring_start_line;
int lstring_nesting;
char const *qstring_start;

%}

NONTERMINAL   [\/0-9a-zA-Z%@-]+

/* Valid years */
ISOYEAR       [0-9]{4}

/* Valid years */
YEAR          [0-9]{2,4}

/* Valid days of the month, leading 0 required */
ISODAY        31|30|2[0-9]|1[0-9]|0[1-9]

/* Valid days of the month, leading 0 optional */
DAY           31|30|2[0-9]|1[0-9]|0[1-9]|[1-9]

/* Valid months of the year, leading 0 required */
ISOMONTH      1[0-2]|0[1-9]

/* Valid months of the year, leading 0 optional, or long name */
MONTH         1[0-2]|0[1-9]|[1-9]|[a-zA-Z]{3,}

/* Valid hours, leading 0 required */
ISOHOUR       2[0-4]|1[0-9]|0[0-9]

/* Valid hours, leading 0 optional */
HOUR          2[0-4]|1[0-9]|0[0-9]|[0-9]

/* Valid minutes or seconds, leading 0 required 
   60 is valid because of leap seconds!
*/
ISOSIXTY      60|5[0-9]|4[0-9]|3[0-9]|2[0-9]|1[0-9]|0[0-9]

/* Valid minutes or seconds, leading 0 optional 
   60 is valid because of leap seconds!
*/
SIXTY         60|5[0-9]|4[0-9]|3[0-9]|2[0-9]|1[0-9]|0[0-9]|[0-9]

FRACTIME      [.,][0-9]+

TWOFIFTYFIVE   25[0-5]|2[0-4][0-9]|[0-1][0-9][0-9]|[0-9][0-9]|[0-9]

EXPONENT      e[+-]?[0-9]{1,3}

/* mailto is handled seperately */
URL_SCHEME            file|ftp|gopher|http|https|news

DOMAIN_CONSTITUENT    [a-z0-9-]
EMAIL_CONSTITUENT     [a-z0-9.-]

ISSUE_CONSTITUENT     [a-z0-9@/!&_?|<=>:*~.+-]
FILE_CONSTITUENT      [a-z0-9@/!&_?|<=>:*~.+-]

/* Colon is part of symbol constituent so that things like
**  foo/:bar/baz  will match.
*/
SYMBOL_INITIAL       [a-z/`!&_?|<=>*~.+-]
SYMBOL_CONSTITUENT   [a-z0-9/:`!&_?|<=>*~.+-]

%%

<INITIAL>\[[ \t]*R[a-z]{3}L[ \t]*\[             |
<INITIAL>\[[ \t]*R[a-z]{2}L[a-z][ \t]*\[        |
<INITIAL>\[[ \t]*R[a-z]L[a-z]{2}[ \t]*\[        |
<INITIAL>\[[ \t]*RL[a-z]{3}[ \t]*\[             unput('['); BEGIN(BODY); HEADER1();

<INITIAL>^[ \t]*R[a-z]{3}L[ \t]*\[             |
<INITIAL>^[ \t]*R[a-z]{2}L[a-z][ \t]*\[        |
<INITIAL>^[ \t]*R[a-z]L[a-z]{2}[ \t]*\[        |
<INITIAL>^[ \t]*RL[a-z]{3}[ \t]*\[             unput('['); BEGIN(BODY); HEADER();

<INITIAL>^[ \t]*R[a-z]{3}L[ \t\x0D]*\n[ \t]*\[        |
<INITIAL>^[ \t]*R[a-z]{2}L[a-z][ \t\x0D]*\n[ \t]*\[   |
<INITIAL>^[ \t]*R[a-z]L[a-z]{2}[ \t\x0D]*\n[ \t]*\[   |
<INITIAL>^[ \t]*RL[a-z]{3}[ \t\x0D]*\n[ \t]*\[        yylineno++; unput('['); BEGIN(BODY); HEADER();

<INITIAL>\n                           yylineno++;
<INITIAL>.

<BODY>{ISOYEAR}-{ISOMONTH}-{ISODAY}T{ISOHOUR}:{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?[a-z]{NONTERMINAL}  BOGUS_ISODATETIME();

<BODY>{ISOYEAR}-{ISOMONTH}-{ISODAY}T{ISOHOUR}:{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?  ISODATETIME();

<BODY>{ISOYEAR}-{ISOMONTH}-{ISODAY}[a-z]{NONTERMINAL}    BOGUS_ISODATE();

<BODY>{ISOYEAR}-{ISOMONTH}-{ISODAY}                 ISODATE();

<BODY>{DAY}\/{MONTH}\/{YEAR}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?[a-z]{NONTERMINAL}   |
<BODY>{DAY}-{MONTH}-{YEAR}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?[a-z]{NONTERMINAL}     |
<BODY>[0-9]{4}\/{MONTH}\/{DAY}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?[a-z]{NONTERMINAL} BOGUS_DATETIME();

<BODY>{DAY}\/{MONTH}\/{YEAR}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?   |
<BODY>{DAY}-{MONTH}-{YEAR}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?     |
<BODY>[0-9]{4}\/{MONTH}\/{DAY}\/{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})? DATETIME();

<BODY>{DAY}\/{MONTH}\/[0-9]{4}{NONTERMINAL}           |
<BODY>{DAY}-{MONTH}-[0-9]{4}{NONTERMINAL}             |
<BODY>{DAY}\/{MONTH}\/[0-9]{2,3}[a-z]{NONTERMINAL}    |
<BODY>{DAY}-{MONTH}-[0-9]{2,3}[a-z]{NONTERMINAL}      |
<BODY>{ISOYEAR}\/{MONTH}\/{DAY}[a-z]{NONTERMINAL}     BOGUS_DATE();

<BODY>{DAY}\/{MONTH}\/{YEAR}                        |
<BODY>{DAY}-{MONTH}-{YEAR}                          |
<BODY>[0-9]{4}\/{MONTH}\/{DAY}                      DATE();

<BODY>{ISOHOUR}:{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?[a-z]{NONTERMINAL}  |
<BODY>{ISOHOUR}:{ISOSIXTY}[a-z]{NONTERMINAL}                          |
<BODY>{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?[a-z]{NONTERMINAL}            BOGUS_ISOTIME();

<BODY>{ISOHOUR}:{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?  |
<BODY>{ISOHOUR}:{ISOSIXTY}                          |
<BODY>{ISOSIXTY}:{ISOSIXTY}({FRACTIME})?            ISOTIME();

<BODY>{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?[a-z]{NONTERMINAL} |
<BODY>{HOUR}:{SIXTY}[a-z]{NONTERMINAL}                      |
<BODY>:{SIXTY}:{SIXTY}{FRACTIME}[a-z]{NONTERMINAL}          |
<BODY>:{SIXTY}:{SIXTY}\.[a-z]{NONTERMINAL}                  |
<BODY>:{SIXTY}:{SIXTY}\,[a-z]{NONTERMINAL}                  |
<BODY>{SIXTY}:{SIXTY}{FRACTIME}[a-z]{NONTERMINAL}           |
<BODY>{SIXTY}:{SIXTY}\.[a-z]{NONTERMINAL}                   |
<BODY>{SIXTY}:{SIXTY}\,[a-z]{NONTERMINAL}                   |
<BODY>:{SIXTY}{FRACTIME}[a-z]{NONTERMINAL}                  |
<BODY>:{SIXTY}\.[a-z]{NONTERMINAL}                          |
<BODY>:{SIXTY}\,[a-z]{NONTERMINAL}                          BOGUS_TIME();

<BODY>{HOUR}:{SIXTY}:{SIXTY}({FRACTIME})?           |
<BODY>{HOUR}:{SIXTY}                                |
<BODY>:{SIXTY}:{SIXTY}{FRACTIME}                    |
<BODY>:{SIXTY}:{SIXTY}\.                            |
<BODY>:{SIXTY}:{SIXTY}\,                            |
<BODY>{SIXTY}:{SIXTY}{FRACTIME}                     |
<BODY>{SIXTY}:{SIXTY}\.                             |
<BODY>{SIXTY}:{SIXTY}\,                             |
<BODY>:{SIXTY}{FRACTIME}                            |
<BODY>:{SIXTY}\.                                    |
<BODY>:{SIXTY}\,                                   TIME();

<BODY>2#\{         |
<BODY>8#\{         |
<BODY>(16)?#\{     |
<BODY>64#\{        BEGIN(LBIN); lstring_start_line=yylineno; qstring_start=yytext;

<LBIN>[a-z0-9 ]*
<LBIN>\n            yylineno++;
<LBIN>\}            BEGIN(BODY); BINARY();
<LBIN>.             BEGIN(BODY); BOGUS_BINARY();


<BODY>{TWOFIFTYFIVE}\.{TWOFIFTYFIVE}(\.{TWOFIFTYFIVE})+[a-z]{NONTERMINAL} BOGUS_DOTTED();

<BODY>{TWOFIFTYFIVE}\.{TWOFIFTYFIVE}(\.{TWOFIFTYFIVE})+ DOTTED();

  /* Note, this catches things like .0foo and lets them drop through to 'SYMBOL'
  ** Necesary for `Alien' script */
<BODY>[+-]?[0-9']*[,.][0-9]+e[a-z]    |
<BODY>[+-]?[0-9']*[,.][0-9]+[a-df-z]  REJECT;

<BODY>[+-]?[0-9']{EXPONENT}                        |
<BODY>[+-]?[0-9']+[,.]({EXPONENT})?                |
<BODY>[+-]?[0-9']*[,.][0-9]+({EXPONENT})?          FLOAT();


<BODY>[+-]?[0-9][0-9]?[0-9]?[0-9]?('[0-9][0-9][0-9][0-9])+[a-z:'.,-] |
<BODY>[+-]?[0-9][0-9]?[0-9]?('[0-9][0-9][0-9])+[a-z:'.,-]            |
<BODY>[+-]?[0-9][0-9]?('[0-9][0-9])+[a-z:'.,-]                       |
<BODY>[+-]?[0-9]+[a-z:'.,-]                                          BOGUS_INTEGER();


<BODY>[+-]?[0-9][0-9]?[0-9]?[0-9]?('[0-9][0-9][0-9][0-9])+ |
<BODY>[+-]?[0-9][0-9]?[0-9]?('[0-9][0-9][0-9])+            |
<BODY>[+-]?[0-9][0-9]?('[0-9][0-9])+                       |
<BODY>[+-]?[0-9]+                                          INTEGER();

<BODY>([a-z]{2,3})?\$[+-]?[0-9']{EXPONENT}                                |
<BODY>([a-z]{2,3})?\$[+-]?[0-9']+[,.]({EXPONENT})?                        |
<BODY>([a-z]{2,3})?\$[+-]?[0-9']*[,.][0-9]+({EXPONENT})?                  |
<BODY>([a-z]{2,3})?\$[+-]?[0-9][0-9]?[0-9]?[0-9]?('[0-9][0-9][0-9][0-9])+ |
<BODY>([a-z]{2,3})?\$[+-]?[0-9][0-9]?[0-9]?('[0-9][0-9][0-9])+            |
<BODY>([a-z]{2,3})?\$[+-]?[0-9][0-9]?('[0-9][0-9])+                       |
<BODY>([a-z]{2,3})?\$[+-]?[0-9]+                                          MONEY();

<BODY>\[                                            BLOCK_BEGIN();

<BODY>\]                                            BLOCK_END();

<BODY>\(                                            PAREN_BEGIN();

<BODY>\)                                            PAREN_END();

<BODY>;[^\n\r]*                                     COMMENT();

<BODY>\"        BEGIN(QSTR); qstring_start = yytext+1;
<QSTR>\^\"
<QSTR>\"        BEGIN(BODY); QSTRING();
<QSTR>\n        BEGIN(BODY); unput('\n'); BOGUS_QSTRING();
<QSTR>.


 /* Long strings.  Note that the curly braces *must* properly nest. */

<BODY>\{        {
                    BEGIN(LSTR); 
                    lstring_start_line = yylineno; 
                    lstring_nesting = 1;
                    qstring_start = yytext+1;
                }
                  
<LSTR>\{        lstring_nesting += 1;
<LSTR>\n        yylineno++;
<LSTR>\}        { 
                    lstring_nesting -= 1;
                    if (lstring_nesting==0) {
                        BEGIN(BODY);
                        LSTRING();
                    }
                }
<LSTR>.

<BODY>{URL_SCHEME}:\/\/{FILE_CONSTITUENT}+    |
<BODY>mailto:{EMAIL_CONSTITUENT}+@{DOMAIN_CONSTITUENT}+(.{DOMAIN_CONSTITUENT}+)*   URL();


<BODY>\%\".*\"                 |
<BODY>\%{FILE_CONSTITUENT}+    FILE();

<BODY>\#{ISSUE_CONSTITUENT}+   ISSUE();

<BODY>\#\".*\"                 CHAR();

<BODY>{EMAIL_CONSTITUENT}+@{DOMAIN_CONSTITUENT}+(.{DOMAIN_CONSTITUENT}+)*   EMAIL();


<BODY>\x0D*\n                              yylineno++; NEWLINE();

<BODY>[ \t]+                                        WHITESPACE();

<BODY>'{SYMBOL_INITIAL}{SYMBOL_CONSTITUENT}*?           |
<BODY>:{SYMBOL_INITIAL}{SYMBOL_CONSTITUENT}*            |
<BODY>{SYMBOL_INITIAL}{SYMBOL_CONSTITUENT}*:            |
<BODY>{SYMBOL_INITIAL}{SYMBOL_CONSTITUENT}*             SYMBOL();

<BODY>[a-zA-Z0-9]+                                  |
<BODY>.                                             UNMATCHED();

%%

void
generic_out(char const *name, char const *value, int length)
{
    int i;

    printf(",(SHERMAN-%s \"", name);
    for (i=length; i>0; i--) {
        if (strchr("\"\\", *value) != 0)
            printf("\\");
        printf("%c", *value++);
    }
    printf("\") ");
}

int
main(void)
{
    int discretionary_space = 0;
    struct token result;
    char const *ptr;
    int i;

    yylineno = 1;

    printf("`(SHERMAN ,(LIST->BLOCK `(");

    while (yylex(&result) != 0) {
        switch (result.tt) {

        case TT_BLOCK_BEGIN:
            printf(",(LIST->BLOCK `(");
            break;

        case TT_BLOCK_END:
            printf(")) ");   /* Space just in case */
            discretionary_space = 1;
            break;

        case TT_BOGUS_BINARY:
            printf("Malformed BINARY, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;

        case TT_BOGUS_DATE:        
            printf("Malformed DATE, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_DATETIME:    
            printf("Malformed DATETIME, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_EMAIL:       
            printf("Malformed EMAIL, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;

        case TT_BOGUS_INTEGER:     
            printf("Malformed INTEGER, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_ISODATE:     
            printf("Malformed ISODATE, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_ISODATETIME: 
            printf("Malformed ISODATETIME, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_ISOTIME:     
            printf("Malformed ISOTIME, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_TIME:        
            printf("Malformed TIME, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;
                                     
        case TT_BOGUS_DOTTED:      
            printf("Malformed DOTTED, line %d", result.line_number);
            exit(EXIT_FAILURE);
            break;

        case TT_COMMENT:
            ptr = result.start;
            for (i=result.length; i>0; i--)
                printf("%c", *ptr++);
            discretionary_space = 0;
            break;

        case TT_DATE:
            generic_out("DATE", result.start, result.length);
            break;

        case TT_DATETIME:
            generic_out("DATETIME", result.start, result.length);
            break;

        case TT_DOTTED:
            printf("(DOTTED #(");
            ptr = result.start;
            for (i=result.length; i>0; i--) {
                char const c = *ptr++;
                if (c == '.') {
                   printf(" ");
                } else {
                   printf("%c", c);
                }
            }
          
            printf(")) ");
            discretionary_space = 1;
            break;

        case TT_EMAIL:
            generic_out("EMAIL", result.start, result.length);
            break;

        case TT_FILE:
            generic_out("FILE", result.start+1, result.length-1);
            break;

        case TT_FLOAT:
            printf("#i");
            ptr = result.start;
            for (i=result.length; i>0; i--) {
                char const c = *ptr++;
                if (c == '\'')
                    continue;
                if (c == ',') {
                    printf("."); 
                } else {
                    printf("%c", c);
                }
            }
            break;

        case TT_HEADER:
            break;

        case TT_HEADER1:
            printf("*** CANNOT RECOGNIZE HEADER1 YET ***"); 
            exit(EXIT_FAILURE);

        case TT_INTEGER:
            ptr = result.start;
            for (i=result.length; i>0; i--) {
                if (*ptr == '\'')
                    continue;
                printf("%c", *ptr++);
            }
            break;

        case TT_ISODATE:
            generic_out("ISODATE", result.start, result.length);
            break;

        case TT_ISODATETIME:
            generic_out("ISODATETIME", result.start, result.length);
            break;

        case TT_ISOTIME:
            generic_out("ISOTIME", result.start, result.length);
            break;

        case TT_NEWLINE:
            printf("\n");
            discretionary_space = 0;
            break;

        case TT_PAREN_BEGIN:
            printf("(");
            break;

        case TT_PAREN_END:
            printf(") ");
            discretionary_space = 1;
            break;

        case TT_QSTRING:
        case TT_LSTRING:
            ptr = result.start;
            printf("\"");
            for (i=result.length; i>0; i--) {
                char c = *ptr++;
                if (strchr("\"\\",c) != 0)
                  printf("\\");
                printf("%c", c);
            }
            printf("\" ");
            discretionary_space = 1;
            break;

        case TT_SYMBOL:
            ptr = result.start;
            for (i=result.length; i>0; i--) {
                char c = *ptr++;
                if (strchr("|.`'\"\\",c) != 0)
                    printf("\\");
                printf("%c", c);
            }
            printf(" "); /* just in case */
            discretionary_space = 1;
            break;

        case TT_TIME:
            generic_out("TIME", result.start, result.length);
            break;

        case TT_URL:
            generic_out("URL", result.start, result.length);
            break;


        case TT_WHITESPACE:
            ptr = result.start;
            for (i=result.length-discretionary_space; i>0; i--) {
                char c = *ptr++;
                printf("%c", c);
            }
            discretionary_space = 0;
            break;

        default:
            printf("LEXER_ERROR");
            exit(EXIT_FAILURE);

        }
    }
    printf("\n))) ;; END of SHERMAN CODE");
    exit(EXIT_SUCCESS);
}
    
