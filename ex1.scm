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

;;; Exercise 1.7: implement square root that works by the proportion
;;; of the change between iterations, rather than a constant value.
(define (sqrt* a)

  ;; The iterative approach used to improve guesses. Adapted from
  ;; Newton's method.
  (define (sqrt-iter guess delta)
    (if (good-enough? guess delta)
      guess
      (let* ((better-guess (improve guess))
	     (delta (- guess better-guess)))
	(sqrt-iter better-guess delta))))

  ;; The method used to improve guesses.
  (define (improve guess)
    (average guess (/ a guess)))

  (define (average a b)
    (/ (+ a b) 2))

  ;; The criteria used to judge accuracy.
  (define (good-enough? guess delta)
    (or (= delta 0)
	(< (abs (/ guess delta)) 0.0001)))

  ;; Invoke the iterative solution!
  (sqrt-iter 1 1))
