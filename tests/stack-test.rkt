#lang racket

(require rackunit
         rackunit/text-ui
         "../stack.rkt")

(require/expose "../stack.rkt" (stack-contents))

(define stack-creation-tests
  (test-suite
   "Test stack creation"
   
   (check-equal? (stack-contents (stack-new)) empty "Empty stack creation")

   (check-equal?
    (stack-contents (stack-new 1)) (list 1) "Single value stack creation")

   (check-equal?
    (stack-contents (stack-new 1 "two" 3))
    (list 3 "two" 1) "Multiple value stack creation")))

(define stack-method-tests
  (test-suite
   "Test stack methods"

   (check-equal?
    (stack-contents (stack-push (stack-new 1 2 3) 4 5 "six"))
    (list "six" 5 4 3 2 1) "Stack push test")

   (test-case
    "Stack pop test"
    (let ([s (stack-new 1 2 2/3)])
      (check = (stack-pop s) 2/3)
      (check-equal? (stack-contents s) (list 2 1))))

   (test-case
    "Stack peek test"
    (let ([s (stack-new 1 2 3)])
      (check = (stack-peek s) 3)
      (check-equal? (stack-contents s) (list 3 2 1))))

   (test-case
    "Stack clear test"
    (let ([s (stack-new 1 2 3)])
      (check-equal? (stack-contents s) (list 3 2 1))
      (stack-clear s)
      (check-equal? (stack-contents s) empty)))

   (check = (stack-length (stack-new 1 2 3 4 5)) 5 "Stack length test")

   (check-equal? (stack-empty? (stack-new 1 2)) #f "Stack empty? test with non-empty stack")

   (check-equal? (stack-empty? (stack-new)) #t "Stack empty? test with empty stack")))

(run-tests stack-creation-tests)
(run-tests stack-method-tests)
