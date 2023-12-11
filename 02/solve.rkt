#lang racket
(define input (file->lines "input"))
(define test (file->lines "sample"))

(define d input)

(define (curryr func arg)
  (lambda (x)
    (func x arg)))

(define (switch-pair pair)
  (match pair
    [(list a b) (list b a)]))

(define (>? a b)
  (if (or a b)
      (>= a b)
      #f))

(define (isl color number lst)
  (>? number (if  (assoc color lst)
                  (cadr (assoc color lst))
                  0)))

(define colors (list "red" "green" "blue"))

(define (parse str)
  (letrec
      ([game (string-split str ":")]
       [game-number (string->number (cadr (string-split (car game) " ")))]
       [games (string-split (cadr game) ";")]
       [colors (map (curryr string-split ",") games)]
       [c2 (map (curry map (curryr string-split " ")) colors)]
       )
    (cons game-number c2)))
;(map cdr (map parse d))

(define lst (map
             (curry map
                    (curry map
                           (curry map
                                  (λ (x) (if (string->number x)
                                             (string->number x)
                                             (string->symbol x))))))
             (map cdr (map parse d))))

;(map (curry map switch-pair) lst)

(define (possible r g b game)
  (map (λ (x)
         (and
          (isl 'red r x)
          (isl 'blue b x)
          (isl 'green g x)))
       game))
(define game-state (map (curry map (curry map switch-pair)) lst))

(define game-result (map (curry andmap identity) (map (curry possible 12 13 14) game-state)))


(for/sum ([e game-result]
          [i (in-naturals)])
  ;(if e (print (add1 i)) 0)
  (if e (add1 i) 0))

;-----

;game-state

(define (max-color color game number)

  (if (assoc color game)
      (max-color color (rest game) (cadr (assoc color game)))
      number))

;(car game-state)
;(apply max (map (λ (x) (max-color 'blue  x  0)) (car game-state)))

(define (colors2 game)
  (list
   (apply max (map (λ (x) (max-color 'red x 0)) game))
   (apply max (map (λ (x) (max-color 'green x 0)) game))
   (apply max (map (λ (x) (max-color 'blue x 0)) game))))
 
  
(apply + (map (λ (x) (apply * x)) (map colors2 game-state)))

   