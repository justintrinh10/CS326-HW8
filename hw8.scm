;1
(define (fib X)(
    cond
        ((= X 0) 0)
        ((= X 1) 1)
        (else (+ (fib (- X 1)) (fib(- X 2))))
    )
)

;2
(define (double-each L)(
    cond
        ((< 0 (length L)) (
            cons (* 2 (car L)) (double-each (cdr L))
        ))
        (else '())
    )
)

;3
(define (corresp L1 L2)(
    cond
        ((and (< 0 (length L1)) (< 0 (length L2))) (
            cond
                ((eq? (car L1) (car L2)) (
                    + 1 (corresp (cdr L1) (cdr L2))
                ))
                (else (corresp (cdr L1) (cdr L2)))))
        (else 0)
    )
)

;4
(define (get-dupes L)(
    get-dupes-helper L '()
    )
)

(define (get-dupes-helper L seen)(
    cond
        ((< 0 (length L)) (
            cond
                ((< 1 (count (car L) L)) (
                    cond
                        ((= 0 (count (car L) seen)) (
                            cons (car L) (get-dupes-helper (cdr L) (cons (car L) seen))
                        ))
                        (else (get-dupes-helper (cdr L) seen))))
                (else (get-dupes-helper (cdr L) seen))
        ))
        (else '())
    )
)

(define (count X L)(
    cond
        ((< 0 (length L)) (
            cond
                ((eq? X (car L)) (
                    + 1 (count X (cdr L))
                ))
                (else (count X (cdr L)))

        ))
        (else 0)
    )
)