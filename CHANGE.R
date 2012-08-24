rebol [
    purpose: {Count the number of ways to make change
              for a given amount}
      ]

declare [standard-definitions]

declare [arity first-denomination 1]

first-denomination: func [kinds-of-coins] [
    if :kinds-of-coins = 1 [
        1 ]
    else [
        if :kinds-of-coins = 2 [
            5 ]
        else [
            if :kinds-of-coins = 3 [
                10 ] 
            else [
                if :kinds-of-coins = 4 [
                    25 ]
                else [
                    if :kinds-of-coins = 5 [
                        50 ]]]]]]

declare [arity cc 2]

cc: func [amount kinds-of-coins] [
    if :amount = 0 [
        1 ]
    else [
        if (:amount < 0) or (:kinds-of-coins = 0) [
            0 ]
        else [
            (cc :amount - first-denomination :kinds-of-coins :kinds-of-coins) +
            (cc :amount :kinds-of-coins - 1)
            ]]]

print cc 200 5 ; should return 2435

                    
