`(SHERMAN ,(LIST->BLOCK `(,(LIST->BLOCK `(
    purpose: "Count the number of ways to make change
              for a given amount"
      ))

declare ,(LIST->BLOCK `(standard-definitions ))

declare ,(LIST->BLOCK `(arity first-denomination 1))

first-denomination: func ,(LIST->BLOCK `(kinds-of-coins )) ,(LIST->BLOCK `(
    if :kinds-of-coins = 1 ,(LIST->BLOCK `(
        1 ))
    else ,(LIST->BLOCK `(
        if :kinds-of-coins = 2 ,(LIST->BLOCK `(
            5 ))
        else ,(LIST->BLOCK `(
            if :kinds-of-coins = 3 ,(LIST->BLOCK `(
                ,(SHERMAN-ISODATE "10")  ))
            else ,(LIST->BLOCK `(
                if :kinds-of-coins = 4 ,(LIST->BLOCK `(
                    ,(SHERMAN-ISODATE "25")  ))
                else ,(LIST->BLOCK `(
                    if :kinds-of-coins = 5 ,(LIST->BLOCK `(
                        ,(SHERMAN-TIME "50")  )) )) )) )) )) ))

declare ,(LIST->BLOCK `(arity cc 2))

cc: func ,(LIST->BLOCK `(amount kinds-of-coins )) ,(LIST->BLOCK `(
    if :amount = 0 ,(LIST->BLOCK `(
        1 ))
    else ,(LIST->BLOCK `(
        if (:amount < 0) or (:kinds-of-coins = 0) ,(LIST->BLOCK `(
            0 ))
        else ,(LIST->BLOCK `(
            (cc :amount - first-denomination :kinds-of-coins :kinds-of-coins ) +
            (cc :amount :kinds-of-coins - 1)
            )) )) ))

print cc 200 5 ; should return 2435



))) ;; END of SHERMAN CODE
