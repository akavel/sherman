`(SHERMAN ,(LIST->BLOCK `(,(LIST->BLOCK `())

declare ,(LIST->BLOCK `(standard-definitions ))

foo: func ,(LIST->BLOCK `(a )) ,(LIST->BLOCK `(a + 3))
print "Hello world!"
prin "3 + 3 ="
print foo 3



))) ;; END of SHERMAN CODE
