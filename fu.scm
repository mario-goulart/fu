(import chicken scheme)
(use data-structures extras irregex files posix srfi-1 srfi-13 utils)

;; for command-line
(define command-line command-line-arguments)

(include "fu-version.scm")
(include "command-line.scm")

(define-record handler cmd help proc)

(define (define-handler cmd help proc)
  (handlers
   (cons (cons cmd (make-handler cmd help proc))
         (handlers))))

;;; Parameters

(define handlers
  (make-parameter '()))

(define match-highlighter
  (make-parameter
   (let ((tty? (terminal-port? (current-output-port))))
     (lambda (match)
       (if tty?
           (string-append "\x1b[31;1m" match "\x1b[0m")
           match)))))

(define fu-editor
  (make-parameter
   (lambda (file)
     (system (sprintf "emacs ~a" (qs file))))))

(define fu-viewer
  (make-parameter
   (lambda (file)
     (system (sprintf "less -R ~a" (qs file))))))

;;; Procedures

(define (remove-handler! cmd)
  (handlers (alist-delete! cmd (handlers))))

(define (die! fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (exit 1))

(define (highlight-match path pattern #!key full-path?)
  (let ((match (irregex-match pattern (if full-path?
                                          path
                                          (pathname-strip-directory path))))
        (pattern (if (sloppy-pattern? pattern)
                     (sloppy->strict-ci-pattern pattern)
                     pattern)))
    (if match
        (irregex-replace/all pattern
                             path
                             (lambda (m)
                               ((match-highlighter)
                                (irregex-match-substring m))))
        path)))

(define (highlight-matches pattern)
  (lambda (option)
    (highlight-match option pattern)))


(define (prompt options option-formatter)

  (define (inner-prompt)
    (let loop ((i 0)
               (options options))
      (cond ((null? options)
             (display "Option (ENTER to abort): ")
             (read-line))
            (else
             (printf "[~a] ~a [~a]\n" i (qs (option-formatter (car options))) i)
             (loop (fx+ i 1) (cdr options))))))

  (let ((len-options (length options)))
    (let loop ()
      (let ((choice (inner-prompt)))
        (if (equal? choice "")
            (exit 0)
            (let ((nchoice (string->number choice)))
              (if (and nchoice (fx< nchoice len-options))
                  nchoice
                  (begin
                    (printf "~a: invalid option.\n" choice)
                    (loop)))))))))

(define (require-positive-integer opt-name)
  (lambda (option)
    (or (and-let* ((n (string->number option))
                   ((exact? n))
                   ((fx> n 0)))
          n)
        (die! "~a: a positive integer is required." opt-name))))

(define (sloppy-pattern sre)
  `(w/nocase (seq (* nonl) ,sre (* nonl))))

(define (sloppy->strict-pattern sre)
  (last (butlast (cadr sre))))

(define (sloppy->strict-ci-pattern sre)
  `(w/nocase ,(sloppy->strict-pattern sre)))

(define (sloppy-pattern? sre)
  (and (pair? sre)
       (eq? 'w/nocase (car sre))))

(define (fu-find-files pattern #!key depth full-path (constraint identity) dir)
  (let ((cwd (current-directory))
        (files
         (find-files (or dir ".")
                     test: (lambda (f)
                             (and (irregex-match pattern
                                                 (pathname-strip-directory f))
                                  (constraint f)))
                     limit: depth)))
    (reverse
     (map (lambda (file)
            (normalize-pathname
             (if full-path
                 (make-pathname cwd file)
                 file)))
          files))))

(define (maybe-prompt-files files pattern op)
  (cond ((null? files)
         (exit 1))
        ((null? (cdr files))
         (op (car files)))
        (else
         (let ((choice (prompt files (highlight-matches pattern))))
           (op (list-ref files choice))))))

(define (prepare-pattern pattern strict?)
  (when (null? pattern)
    (die! "Missing pattern."))
  (unless (null? (cdr pattern))
    (die! "Multiple patterns are not supported."))
  (if strict?
      (string->sre (car pattern))
      (sloppy-pattern (string->sre (car pattern)))))

(define (fu-find/operate op #!key (prompt? #t) (non-dirs-only? #t) (dir "."))
  (lambda args
    (let* ((parsed-args
            (parse-command-line args
                                `((-s)
                                  (-f)
                                  (-d . ,(require-positive-integer '-d)))))
           (get-opt (lambda (opt)
                      (alist-ref opt parsed-args)))
           (pattern (prepare-pattern (get-opt '--) (get-opt '-s)))
           (files (fu-find-files pattern
                                 dir: dir
                                 depth: (get-opt '-d)
                                 full-path: (get-opt '-f)
                                 constraint: (if non-dirs-only?
                                                 (lambda (f)
                                                   (not (directory? f)))
                                                 identity))))
      (if prompt?
          (maybe-prompt-files files pattern op)
          (for-each (lambda (file)
                      (op (qs (highlight-match file pattern))))
                    files)))))

;;;
;;; Config
;;;

(define (load-global-conf)
  (let ((conf "/etc/fu.conf"))
    (when (file-read-access? conf)
      (load conf))))

(define (load-user-conf)
  (let* ((home (get-environment-variable "HOME"))
         (conf (and home (make-pathname home ".fu.conf"))))
    (when (and conf (file-read-access? conf))
      (load conf))))

(define (load-conf)
  (load-global-conf)
  (load-user-conf))

;;;
;;; Handlers
;;;

(define-handler 'f
  #<<EOF
f [-s] [-f] [-d <depth>] <pattern>
  Find files that sloppily match <pattern> (a regular expression).
  Sloppily means <pattern> will be surrounded by ".*" and will be case
  insensitive.
    -s:         strict mode -- disable sloppy mode.
    -f:         print full paths
    -d <depth>: limit search to <depth>
EOF
  (fu-find/operate print prompt?: #f non-dirs-only?: #f))

(define-handler 'v
  #<<EOF
v <f options> <pattern>
  Find files & view.
EOF
  (fu-find/operate (fu-viewer)))

(define-handler 'e
  #<<EOF
e <f options> <pattern>
  Find files & edit.
EOF
  (fu-find/operate (fu-editor)))


;;;
;;; Usage & command line parsing
;;;

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port "Usage: ~a <command> <options>\n\n" this)
    (for-each (lambda (handler)
                (display (handler-help (cdr handler)) port)
                (newline)
                (newline))
              (reverse (handlers))))
  (when exit-code
    (exit exit-code)))


(let ((args (command-line-arguments)))

  (load-conf)

  (when (null? args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (when (or (member "-v" args)
            (member "-version" args)
            (member "--version" args))
    (print fu-version)
    (exit 0))

  (let* ((cmd (string->symbol (car args)))
         (cmd-args (cdr args))
         (handler (alist-ref cmd (handlers))))
    (if handler
        (apply (handler-proc handler) cmd-args)
        (die! "~a: invalid command." cmd))))
