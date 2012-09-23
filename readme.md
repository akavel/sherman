## [A NOTE FROM JOE MARSHALL:](http://code.google.com/p/jrm-code-project/wiki/BitsAndPieces)

Sherman is a compiler that compiled [Rebol](http://www.rebol.com/) to Scheme. Rebol is a lightweight language that was designed by [Carl Sassenrath](http://en.wikipedia.org/wiki/Carl_Sassenrath). I joined Rebol Technologies in the summer of 1998 and wrote the first implementation of the Rebol interpreter. Carl and I had a falling out in December of 1998 (an interesting topic for another time), and I decided to see if I couldn't write a compiler for the language. The main challenge is that the language is context-sensitive and parsing depends on runtime values. It isn't possible to build a useful abstract syntax tree at compile time because it is impossible to determine the role each identifier plays until it's value is known. Furthermore, an identifier may indicate a variable at some times or a function call at others. 

The trick of Sherman is to simply generate the control flow for all the parse options and dispatch to the correct one at runtime. This makes a combinatorical explosion, but it is possible to prune the control flow tree if the compiler can make assumptions about certain identifiers. I added a declaration form to the language for this purpose. The output of Sherman is a Scheme program that is equivalent to the original Rebol program. The Scheme version can be compiled with the Scheme compiler and it will run quite a bit faster than the original Rebol. 

Rebol 1.0 was very Scheme-like, but the context-sensitive parsing allows you to omit most of the grouping (parenthesis). Here is [Ackermann's function](http://en.wikipedia.org/wiki/Ackermann_function) in Rebol:

    ack: func [m n] [
        if zero? :m [
            :n + 1
            ]
        else [
            if zero? :n [
                ack :m - 1 1
                ]
            else [
                ack :m - 1 ack :m :n - 1  ;; Note this line.
                ]
            ]
        ]

Check out the final recursive call to ack. The Rebol interpreter correctly determines that this should be parsed (ack (- m 1) (ack m (- n 1))) rather than any other potential way. Rebol 1.0 was properly tail-recursive, which was a true bear to achieve, and it featured first-class continuations (this latter was trivial to add because of the way I had to do the tail recursion). 

I figured out a neat trick for parsing non-lisp stuff without a lot of effort. The file R2S.L is a [Flex](http://flex.sourceforge.net/) program that parses Rebol. The production rules in the parse simply print the parsed token, but with a few strategically placed parenthesis. The result is a text file that can be easily read by a Lisp system. Here is the output of the Ackermann program from above:

    `(SHERMAN ,(LIST->BLOCK `(,(LIST->BLOCK `()) 

    declare ,(LIST->BLOCK `(standard-definitions )) 

    declare ,(LIST->BLOCK `(arity ack 2)) 
    ack: func ,(LIST->BLOCK `(m n )) ,(LIST->BLOCK `(
        if zero? :m ,(LIST->BLOCK `(
            :n + 1
            )) 
        else ,(LIST->BLOCK `(
            if zero? :n ,(LIST->BLOCK `(
                ack :m - 1 1
                )) 
            else ,(LIST->BLOCK `(
                ack :m - 1 ack :m :n - 1
                )) 
            )) 
        )) 

    print ack 3 5 ; should print 253


    ))) ;; END of SHERMAN CODE

It's a bit ugly, but it is a legal Scheme backquoted list. 

Carl abandoned my implementation after I left Rebol Technologies. He completely rewrote the interpreter and, in the process, substantially changed the semantics of the language. Rebol 2.0 is a quirky and bizarre language. I don't see any opportunity in the language so I never tried to write a compiler for Carl's semantics. Rebol seems to still be popular in France, but I think the company has shrunk down to be just Carl and his wife.

_[Joe Marshall](http://code.google.com/p/jrm-code-project/wiki/BitsAndPieces)_

------

## How to use this project

Below, I collected information which should help you if you want to experiment with this project. Please note this is mostly this, *an experiment* at this moment, so the procedure is not very user-friendly. Enough smalltalk (uhm...) - hackery galore ensues! :P

### Prepare

  * **Download MzScheme** (part of PLT-Scheme project), **version 103p1** (or some older, any version between 52 and 103p1 might work) for your OS from: http://download.plt-scheme.org/mzscheme/all-versions.html then unzip somewhere (for example, to `c:\plt`).
  * (Obviously, download contents of this project.)
  * Open shell (on Windows, [Win]+[R], then type `cmd`), then `cd PATH_TO_SHERMAN`
  * Set up environment. On Windows:
<pre>
set PATH=c:\plt;%PATH%
set PLTCOLLECTS=%CD%\collects;c:\plt\collects
</pre>

### Compile your Rebol 1.0 code

To run any file with `.r` extension using this project, a multistep translation (compilation) must be first performed. Running `sherman.bat compile trivial.r` tries to transform the file in following steps:

  * `trivial.r` - this is an example program in "Rebol 1.0" (or rather, Sherman) source code.
     * Calling `sherman.bat compile trivial.r` tries internally to run `r2s < trivial.r > trivial.rs`. Now, `r2s.exe` seems to ask for Cygwin, and according to Joe's note, is somewhat broken (?). Anyway, for me, this step fails. Source code for [flex lexer generator](http://en.wikipedia.org/wiki/Flex_lexical_analyser) is provided in `r2s.l`, but I didn't try to compile with MinGW.
     * So, Joe provided initially pre-processed files for the sample programs, with `.s` extension. So, to workaround this step, you can run: `copy trivial.s trivial.rs`.
     * Alternatively, I started work porting `r2s.l` to [Lua](http://www.lua.org/)'s [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) library. It's only very initial code, but I tried to make it work for the provided sample programs, so if you want to do this step the funny way, and you have Lua + LPEG installed, you can run: `lua r2s.lua < trivial.r > trivial.rs`.
  * `trivial.rs` - this is result of first step of compilation, and this seems to be a simplest form that is digestible to Scheme parser, so that it can be loaded as "Scheme data tree", or something like that (in current day's buzzwords, you might think of it like what JSON is for JavaScript).
     * `sherman.bat compile trivial.rs` processes this further and generates:
  * `trivial.ss` - this is result of second step of compilation. This seems to be a regular, correct Scheme program, only it references lots of functions from some weird nonstandard library ("sherman namespace").
     * the next progression should work automatically, but if you need to start from previous step explicitly, you can call `sherman.bat compile trivial.ss`.
  * `trivial.zo` - this is result of third step of compilation. This seems to be a binary bytecode form of the `trivial.ss` program, compiled solely by MzScheme (see also: [some pdf about mzc compiler](http://download.plt-scheme.org/doc/371/pdf/mzc.pdf)).

### Run the compiled code

Now, after you got your program compiled (as described above), to run it, you can call either:
  * `sherman.bat run trivial.zo`, or:
  * `sherman.bat run trivial.ss`, whichever you prefer.

Voilà! *Works For Me&trade;!* ;)

Expected output:

<pre>
Sherman runtime version 0.5
Hosted on MzScheme version 103, Copyright (c) 1995-2000 PLT (Matthew Flatt)
Hello world!
3 + 3 =6
>
</pre>

(on Windows, quit with: [Ctrl-Z], [Enter])

Have fun!

*Big thanks to [soegaard, for helping me to make this work](http://stackoverflow.com/a/12551836/98528). --Mateusz Czapliński, 2012*