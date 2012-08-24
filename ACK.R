rebol []

declare [standard-definitions]

declare [arity ack 2]
ack: func [m n] [
    if zero? :m [
        :n + 1
        ]
    else [
        if zero? :n [
            ack :m - 1 1
            ]
        else [
            ack :m - 1 ack :m :n - 1
            ]
        ]
    ]

print ack 3 5 ; should print 253

