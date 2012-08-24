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

