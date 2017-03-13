;;; BASICS
;; Basic function definition
(define hello-world (lambda () "Hello World"))

(define (hello-world2) "Hello World")

(define hello3
  (case-lambda
    (() "Hello World")
    ((name) (string-append "Hello " name))))

;; Strings
(string-append "Hello " "world!") ; => "Hello world!"

;; A string can be treated like a list of characters
(string-ref "Apple" 0) ; => #\A

;; format can be used to format strings:
;; #t option displays it, use #f for assignment
(format #t "~a can be ~a" "strings" "formatted")

;; Structs
(define-record-type dog (fields name breed age))
(define my-pet
  (make-dog "lassie" "collie" 5))
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"

;; `cons' constructs pairs, `car' and `cdr' extract the first
;; and second elements
(cons 1 2) ; => '(1 . 2)
(car (cons 1 2)) ; => 1
(cdr (cons 1 2)) ; => 2

;; Lists
(cons 1 (cons 2 (cons 3 '()))) ; => '(1 2 3)
;; `list' is a convenience variadic constructor for lists
(list 1 2 3) ; => '(1 2 3)
;; and a quote can also be used for a literal list value
'(1 2 3) ; => '(1 2 3)


(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; EQUALITY
;; for numbers use `=`
(= 3 3.0) ; => #t
(= 2 1) ; => #f

;; for characters use `char=?`
(char=? #\c #\c) ; => #t

;; for objects, use `eq?`
(eq? '(1 2) '(1 2))

;; The difference between `eqv?' and `eq':
;; You can compare numbers and characters with `eqv?'
;; When you use `eqv?' for any objects other than numbers&characters, it's the
;; same with `eq?'.
(eqv? 3 3)     ; => #t
(eqv? #\c #\c) ; => #t
(eqv? 'a 'a)   ; => #t

;; for collections use `equal?'
;; `equal?' will compare all the values in a collections type like record or list.
;; You can compare any objects with `equal?' safely, but inefficiency.
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;;; CONTROL FLOW
(if #t               ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;; `cond' chains a series of tests to select a result
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (else 'ok)) ; => 'ok

(iota 10)   ; => (0 1 2 3 4 5 6 7 8 9)
(iota 5 10) ; => (10 11 12 13 14)

(for-each display '(1 2 3 4 5)) ; => 12345

;; To catch exceptions, use the 'catch' form
(catch 'my-error
  (lambda () (throw 'my-error))
  (lambda e (display "oh~my error!\n")))
; => oh~my error!

;;; MUTATION

(define n 5)
(set! n (1+ n))
n ; => 6


