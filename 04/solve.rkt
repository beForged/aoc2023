#lang racket

(define input (file->lines "./04/input"))
(define test (file->lines "./04/sample"))

(define d input)
(define mid (map (lambda (x) (string-split (cadr (string-split x ":")) "|")) d))

(define (winning winning-lst my-lst)
    (foldl 
        (lambda (x acc) (if (member x winning-lst) 
                            (if (eq? acc 0) 1 (* acc 2)) 
                            acc)) 0 my-lst)
)
(define (string->list-nums str)
    (filter-map string->number (string-split str " ")))

(apply + (map (lambda (x) (winning (string->list-nums (car x)) (string->list-nums (cadr x)))) mid))

;; ----
(define (diff-win winning-lst my-lst)
    (foldl 
        (lambda (x acc) (if (member x winning-lst) 
                            (if (eq? acc 0) 1 (add1 acc)) 
                            acc)) 0 my-lst)
)

(define (winning2 lst cardlst)
  (if (empty? lst) '()
    (let* (
          [curr-lst (car lst)]
          [winning-lst (string->list-nums (car curr-lst))]
          [my-lst (string->list-nums (cadr curr-lst))]
          [wins (diff-win winning-lst my-lst)]
          [cards (add1 (length cardlst))] ;add1 to include the current card
          [new-list (filter (lambda (x) (not (zero? x))) (flatten (cons (map sub1 cardlst) (make-list cards wins))))]
    )
        (cons cards (winning2 (cdr lst) new-list))
    )
))
(apply + (winning2 mid '()))
