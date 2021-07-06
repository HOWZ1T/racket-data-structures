#lang racket

; Stack structure, #:transparent allows fields to be seen
(struct stack ([contents #:mutable]) #:transparent)

; new stack helper function, takes in variadic args to build stack
(define (stack-new . contents)
  (stack-push-helper (stack empty) contents))

; pushes the list elements to the stack
(define (stack-push-helper stk contents)
  (if (empty? contents)
      stk
      (begin
        (set-stack-contents! stk
                             (cons
                              (first contents)
                              (stack-contents stk)))
        (stack-push-helper stk (rest contents)))))

; sugar method for stack push with variadic arguments
(define (stack-push stk . contents)
  (stack-push-helper stk contents))

; pops the top value from the stack
(define (stack-pop stk)
  (let
    ([x (first (stack-contents stk))])
    (set-stack-contents! stk (rest (stack-contents stk)))
    x))

; peeks the top value from the stack without removing it
(define (stack-peek stk)
  (first (stack-contents stk)))

; clears all the values from the stack
(define (stack-clear stk)
  (set-stack-contents! stk empty))

; returns the number of items in the stack
(define (stack-length stk)
  (length (stack-contents stk)))

; returns a boolean indicating if the stack is empty
(define (stack-empty? stk)
  (= 0 (stack-length stk)))


; exports
(provide
 stack-new
 stack-push
 stack-pop
 stack-peek
 stack-clear
 stack-length
 stack-empty?)
