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