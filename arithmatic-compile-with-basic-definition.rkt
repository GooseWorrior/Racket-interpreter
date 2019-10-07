;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname arithmatic-compile-with-basic-definition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;basic-define:
(define-struct node(sym val))
(define sym-bank
  (list "+" "-" "*" "/" "expt" "max" "min" "abs" "exp" "define"))


;;Pre-process:
(define (reform-wrap str)
  (reform (string->list str)))

(define (reform loc)
  (cond [(empty? loc) empty]
        [(or (char=? (first loc) #\space)
             (char=? (first loc) #\newline))
         (reform (rest loc))]
        [(or (char=? (first loc) #\()
             (char=? (first loc) #\)))
         (cons (list->string (cons (first loc) empty))
               (reform (rest loc)))]
        [else (cons (find-element loc)
                    (reform (cut-off loc)))]))

(define (find-element loc)
  (list->string (find-element-helper loc)))

(define (find-element-helper loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\space) empty]
        [(char=? (first loc) #\)) empty]
        [else (cons (first loc) (find-element-helper (rest loc)))]))

(define (cut-off loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\space) loc]
        [(char=? (first loc) #\)) loc]
        [else (cut-off (rest loc))]))


;;Tree Part:
(define (start-tree los)
  (cond [(empty? los) "no expression"]
        [(string=? (first los) "(") (create-func (rest los))]
        [(string=? (first los) ")") "expect a function or number, but find )"]
        [else (first los)]))

(define (tree-stream los)
  (cond [(empty? los) empty]
        [else (cons (start-tree los) (tree-stream (cutoff-2 (rest los) 1 0)))]))

;;(+) (*)is not allowed, but it is used in compiler
(define (create-func los)
  (cond [(not (member? (first los) sym-bank))
         "invalid function call"]
        [(or (string=? (first los) "+")
             (string=? (first los) "*")
             (string=? (first los) "max")
             (string=? (first los) "min"))
         (cond [(string=? (second los) ")") "expect at least 1 argument"]
               [(string? (find-val (rest los))) (find-val (rest los))]
               [else (make-node (first los) (find-val (rest los)))])]
        [(or (string=? (first los) "abs")
             (string=? (first los) "exp"))
         (cond [(not (= (count-element (rest los) 1 0) 1)) "expect 1 argument"]
               [(string? (find-val (rest los))) (find-val (rest los))]
               [else (make-node (first los) (find-val (rest los)))])]
        [(or (string=? (first los) "-")
             (string=? (first los) "/")
             (string=? (first los) "define")
             (string=? (first los) "expt"))
         (cond [(not (= (count-element (rest los) 1 0) 2)) "expect 2 arguments"]
               [(string? (find-val (rest los))) (find-val (rest los))]
               [else (make-node (first los) (find-val (rest los)))])]))

(define (find-val los)
  (cond [(empty? los) "expect right bracket"]
        [(string=? (first los) ")")
         empty]
        [(string=? (first los) "(")
         (cond [(not (string? (find-val (cutoff-2 (rest los) 1 0))))
               (cons (create-func (rest los))
                     (find-val (cutoff-2 (rest los) 1 0)))]
               [else (find-val (cutoff-2 (rest los) 1 0))])]
        [else
         (cond [(equal? (string->number (first los)) false)
                (cons (first los) (find-val (rest los)))]
               [(not (string? (find-val (rest los))))
                (cons (string->number (first los)) (find-val (rest los)))]
               [else (find-val (rest los))])]))

(define (cutoff-2 los left-num right-num)
  (cond [(empty? los) empty]
        [(= left-num right-num) los]
        [(string=? (first los) "(")
         (cutoff-2 (rest los) (add1 left-num) right-num)]
        [(string=? (first los) ")")
         (cutoff-2 (rest los) left-num (add1 right-num))]
        [else (cutoff-2 (rest los) left-num right-num)]))

(define (count-element los left-num right-num)
  (cond [(empty? los) 0]
        [(= left-num right-num) 0]
        [(string=? (first los) ")")
         (cond [(= left-num (+ 2 right-num))
                (add1 (count-element (rest los) left-num (add1 right-num)))]
               [else (count-element (rest los) left-num (add1 right-num))])]
        [(string=? (first los) "(") (count-element (rest los) (add1 left-num) right-num)]
        [(= left-num (+ 1 right-num)) (add1 (count-element (rest los) left-num right-num ))]
        [else (count-element (rest los) left-num right-num )]))


;;evaluate
(define (evaluate-nd tree lst)
  (local [(define error-id (my-check-error (node-val tree) lst))]
  (cond [(not (equal? error-id false))
         error-id]
        [(string=? (node-sym tree) "+")
         (eval-t1 + (node-val tree) lst)]
        [(string=? (node-sym tree) "*")
         (eval-t1 * (node-val tree) lst)]
        [(string=? (node-sym tree) "min")
         (eval-t2 max (node-val tree) lst)]
        [(string=? (node-sym tree) "max")
         (eval-t2 min (node-val tree) lst)]
        [(string=? (node-sym tree) "abs")
         (eval-t3 abs (node-val tree) lst)]
        [(string=? (node-sym tree) "exp")
         (eval-t3 exp (node-val tree) lst)]
        [(string=? (node-sym tree) "expt")
         (eval-t4 expt (node-val tree) lst)]
        [(string=? (node-sym tree) "/")
         (eval-t4 / (node-val tree) lst)]
        [(string=? (node-sym tree) "-")
         (eval-t4 - (node-val tree) lst)])))

(define (evaluate val lst)
  (cond [(node? val)
         (evaluate-nd val lst)]
        [(number? val)
         val]
        [(and (string? val)
              (check-key val lst))
         (evaluate (find-key val lst) lst)]
        [else "variable is not defined"]))

(define (evaluate-stream lot def-database)
  (cond [(empty? lot) empty]
        [(node? (first lot))
         (cond [(string=? (node-sym (first lot)) "define")
                (evaluate-stream (rest lot) def-database)]
               [else (cons (evaluate-nd (first lot) def-database)
                     (evaluate-stream (rest lot) def-database))])]
        [else (cons (first lot) (evaluate (rest lot)))]))

(define (eval-t1 op lon lst)
  (cond [(empty? lon) (op)]
        [else (op (evaluate (first lon) lst) (eval-t1 op (rest lon) lst))]))

(define (eval-t2 op lon lst)
  (cond [(empty? (rest lon)) (evaluate (first lon) lst)]
        [else (op (evaluate (first lon) lst) (eval-t2 op (rest lon) lst))]))

(define (eval-t3 op lon lst)
  (op (evaluate (first lon) lst)))

(define (eval-t4 op lon lst)
  (op (evaluate (first lon) lst) (evaluate (second lon) lst)))

(define (my-check-error lst database)
  (cond [(empty? lst) false]
        [(node? (first lst))
         (my-check-error (node-val (first lst)))]
        [(string? (evaluate (first lst) database))
         (evaluate (first lst) database)]
        [else (my-check-error (rest lst) database)]))

;;checking/saving definition
(define (sv-def lot)
  (cond [(empty? lot) empty]
        [(node? (first lot))
         (cond [(string=? (node-sym (first lot)) "define")
                (cons (node-val (first lot))
                      (sv-def (rest lot)))]
               [else (sv-def (rest lot))])]
        [else (sv-def (rest lot))]))

(define (check-key key lst)
  (cond [(empty? lst) false]
        [(string=? key (first (first lst)))
         true]
        [else (check-key key (rest lst))]))

(define (find-key key lst)
  (cond [(empty? (rest lst)) (second (first lst))]
        [(string=? key (first (first lst)))
         (second (first lst))]
        [else (find-key key (rest lst))]))
;;Main:
(define (produce-tree str);; must have one tree
  (start-tree (reform-wrap str)))
(define (produce-tlst str)
  (tree-stream (reform-wrap str)))
(define (solve-RTK str);; must have one expression and no definition
  (evaluate (produce-tree str)))
(define (solve-RKT-stream str)
  (local [(define tlst (produce-tlst str))
          (define def-database (sv-def tlst))]
        (evaluate-stream tlst def-database)))

;;Tests:
(produce-tree "(+  3   4 (*  3 5  6) (+ 3 (+ 3 6) 5) (* 3 2 1))")
(produce-tlst "(+ 2 3) (+ 2 4) (+)")
(count-element (reform-wrap " 2 (+ (())) () (()()) 3") 1 0)
(count-element (reform-wrap ")") 1 0)
(produce-tree "(- 1 (+ 2 3))")
(produce-tlst "(define a 5)
                  (define b (+ a 5))
                   (+ a b)")
(solve-RKT-stream "(define a 5)
                  (define b (+ a 5))
                   (+ a b)
                    (+ a 5)
                    (+ 1 2)")

(sv-def (produce-tlst  "(define a 5)
                  (define b (+ a 5))
                   (+ a b)"))