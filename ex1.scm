;;; SICP exercise 1.3
;;; Define a procedure that takes three numbers as arguments and
;;; returns the sum of the squares of the two larger numbers.
(define (square-top-two a b c)
  ;; helper function, sum two.
  (define (sum-of-squares x y)
    (define (square z)
      (* z z))
    (+ (square x) (square y)))
  ;; determine which number is smallest, and sum the other two.
  (cond ((and (< a b) (< a c))
	  (sum-of-squares b c))
	((and (< b a) (< b c))
	  (sum-of-squares a c))
	(else
	  (sum-of-squares a b))))



#|
  (if (< a b)
    (if (< a c)
      ;; a is the smallest
      (sum-of-squares b c)
      ;; c is the smallest
      (sum-of-squares a b))
    (if (< b c)
      ;; b is the smallest
      (sum-of-squares a c)
      ;; c is the smallest
      (sum-of-squares a b))))
|#

