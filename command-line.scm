;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command line argument handling for R7RS Scheme.
;;;
;;; See README and command-line.sld for more information.
;;;
;;; This software is written by Evan Hanson <evhan@foldling.org> and
;;; placed in the Public Domain. All warranties are disclaimed.
;;;

;;
;; `match-option` matches a single option specification against a list
;; of command line arguments.
;;
;; If the given `arguments` don't match the `specification`, an error is
;; signaled. Otherwise, the matching items in `arguments` are collected
;; into an association pair and the continuation `continue` is called
;; with the list of remaining items and resulting pair as arguments.
;;
;; This procedure is internal to command-line.sld.
;;
(define (match-option specification arguments continue)
  (let lp ((spec (cdr specification))
           (args (cdr arguments))
           (cont (lambda (args vals)
                   (continue args (cons (car specification) vals)))))
    (cond ((null? spec)
           (cont args (list)))
          ((null? args)
           (error "Insufficient arguments for command line option"
                  (car specification)))
          ((string=? "--" (car args))
           (error "Invalid value for command line option"
                  (car specification)))
          ((pair? spec)
           (if (pair? (car spec)) ; Nested option specs aren't supported.
               (error "Invalid command line option specification" specification)
               (lp (car spec)
                   (list (car args))
                   (lambda (_ head)
                     (lp (cdr spec)
                         (cdr args)
                         (lambda (args tail)
                           (cont args (cons head tail))))))))
          ((procedure? spec)
           (cont (cdr args) (spec (car args))))
          (else
           (cont (cdr args) (car args))))))

;;
;; `normalize-grammar` compiles an options grammar into a standardized
;; format. Currently, this means splitting any option specifications
;; whose `car` is a list into multiple entries, allowing the following
;; abbreviated syntax for option aliases:
;;
;;     (normalize-grammar '(((--foo --bar --baz) . qux)))
;;     ; => ((--foo . qux)
;;           (--bar . qux)
;;           (--baz . qux))
;;
;; This procedure is internal to command-line.sld.
;;
(define normalize-grammar
  (letrec ((fold (lambda (f a l)
                   (if (pair? l) (fold f (f a (car l)) (cdr l)) a))))
    (lambda (grammar)
      (fold (lambda (a g)
              (if (pair? g)
                  (let ((n (car g))
                        (s (cdr g)))
                    (if (list? n)
                        (append (map (lambda (k) (cons k s)) n) a)
                        (cons g a)))
                  (error "Invalid command line option specification" g)))
            '()
            (reverse grammar)))))

;;
;; `parse-command-line` parses a program's command line arguments into
;; an association list according to an S-expressive options grammar.
;;
;; It takes one required and two optional arguments: an option matching
;; procedure, an S-expressive options `grammar`, and a list of command
;; line argument strings. If `matcher` is not given a basic string
;; comparison is used, while `arguments` defaults to the value of `(cdr
;; (command-line))`.
;;
;; `grammar` is a finite list of pairs whose `car`s are symbols and
;; whose `cdr`s are pairs or atoms. All other `car`s in the grammar must
;; be atoms; grammars may not be nested.
;;
;; The given `arguments` are matched as symbols against the `car`s of
;; the options grammar. When a match is found, an association from the
;; matched symbol to the arguments immediately following the matched
;; item in the arguments list is added, following the form of the
;; matched pair.
;;
;;     (parse-command-line
;;      '("foo" "bar")
;;      '((foo . bar))) ; => ((foo . "bar")
;;                            (--))
;;
;;     (parse-command-line
;;      '("foo" "bar" "baz" "qux")
;;      '((foo)
;;        (bar baz qux))) ; => ((foo)
;;                              (bar "baz" "qux")
;;                              (--))
;;
;; Unmatched arguments are added to the resulting association list under
;; the key `--`. Similarly, any arguments following a `"--"` in the
;; arguments list are treated as unmatched.
;;
;;     (parse-command-line
;;      '("foo" "bar" "baz")
;;      '((foo . bar))) ; => ((foo . "bar")
;;                            (-- "baz"))
;;
;;     (parse-command-line
;;      '("foo" "bar" "--" "baz" "qux")
;;      '((foo . bar)
;;        (baz . qux))) ; => ((foo . "bar")
;;                            (-- "baz" "qux"))
;;
;; In a matched options form, procedures are replaced by the result of
;; invoking that procedure on the corresponding item in the arguments
;; list. All other objects are replaced by the corresponding argument
;; string directly.
;;
;;     (parse-command-line
;;      '("foo" "bar" "42" "qux")
;;      `((foo ,list ,string->number ,string->symbol)))
;;     ; => ((foo ("bar") 42 qux)
;;           (--))
;;
(define parse-command-line
  (case-lambda
    ((grammar)
     (parse-command-line (lambda _ #f) (cdr (command-line)) grammar))
    ((arguments grammar)
     (parse-command-line (lambda _ #f) arguments grammar))
    ((matcher arguments grammar)
     (let ((grammar (normalize-grammar grammar)))
       (let lp ((args arguments)
                (unmatched (list))
                (matched (list)))
         (if (null? args)
             (reverse (cons (cons '-- (reverse unmatched)) matched))
             (let ((arg (car args))
                   (cont (lambda (args vals)
                           (lp args unmatched (cons vals matched)))))
               (cond ;; Simple match.
                     ((assq (string->symbol arg) grammar) =>
                      (lambda (spec)
                        (match-option spec args cont)))
                     ;; Custom match (via `matcher` procedure).
                     ((matcher arg grammar) =>
                      (lambda (handler)
                        (handler args (lambda (spec args)
                                        (match-option spec args cont)))))
                     ;; Treat all arguments following "--" as unmatched.
                     ((string=? "--" arg)
                      (lp (list) (append (reverse (cdr args)) unmatched) matched))
                     ;; An unmatched argument.
                     (else
                      (lp (cdr args) (cons arg unmatched) matched))))))))))
