#lang racket
(require (only-in srfi/13 string-contains))

(define input (file->lines "input"))
(define test (file->lines "test2"))

(define d input)




;(define (getNumber str i)
;  (list-ref (filter char-numeric? str) i))

;(define (getLast str)
;  (last (filter char-numeric? str)))


;(define fst (map (lambda (n)
;    (getNumber (string->list n) 0) )d ))

;(define last-int (map (lambda (n)
;    (getLast (string->list n)) )d ))

;(define numlist (map string->number (map list->string (map (lambda (one two)
;     (flatten (cons one two))) fst last-int))))

;(foldr + 0 numlist)

(define spelled (list (list "one" 1) (list "two" 2) (list "three" 3) (list "four" 4) (list "five" 5) (list "six" 6) (list "seven" 7) (list "eight" 8) (list "nine" 9)))
;(assoc  "one" spelled)

(define (t str s2 i end ival)
  (let ([x (string-contains str s2 i end)])
    (if x
        (cons (cons x ival) (t str s2 (+ 1 x) end ival))
        '()
        )))

(define (has-number? str)
  (map (lambda (a)  (cons
                     (t str (car a) 0 (string-length str) (car (cdr a)))
                     (t str (number->string (car (cdr a))) 0 (string-length str) (car (cdr a)))))
       spelled))
;-----
; i needed to spoiler to find this exception: to be fair its almost 2am, im tired
(define replacer (list
                  (list "one" "one1one")
                  (list "two" "two2two")
                  (list "three" "three3three")
                  (list "four" "four4four")
                  (list "five" "five5five")
                  (list "six" "six6six")
                  (list "seven" "seven7seven")
                  (list "eight" "eight8eight")
                  (list "nine" "nine9nine")))

(define (repl str) (foldr (lambda (a acc) (string-replace acc (car a) (car (cdr a)))) str replacer))

(define (getNumber str i)
  (list-ref (filter char-numeric? (string->list (repl str))) i))

(define (getLast str)
  (last (filter char-numeric? (string->list (repl str)))))


(define fst (map (lambda (n)
    (getNumber  n 0) )d ))

(define last-int (map (lambda (n)
    (getLast  n) )d ))

(define numlist (map string->number (map list->string (map (lambda (one two)
     (flatten (cons one two))) fst last-int))))

(foldr + 0 numlist)
;-----


(define (to-pair lst)
  (if (null?  lst)
      '()
      (cons (cons (first lst) (second lst)) (to-pair (drop lst 2)))))
  




(define mid (map (lambda (x) (sort x < #:key car)) (map to-pair (map flatten (map has-number? (map repl d))))))
;mid


;get first and last
(define numlist2 (map (lambda (m)
                       (if (eq? (car (car m))
                                (car (car (reverse m))))
                           (cons 0 (cdr (car m)))
                           (cons (cdr (car m)) (cdr (car (reverse m)))))
                       ) mid))

(define tired (map string->number (map (lambda (x) (apply string-append x)) (map (lambda (x) (map number->string x)) (map flatten numlist2)))))
;tired
;(foldr + 0 tired)
       


;(map identity d)
;(map has-number? d)

