(import chicken scheme)
(use data-structures extras irregex files ports srfi-1 srfi-13 utils)
(use (except posix find-files))

;; for command-line
(define command-line command-line-arguments)

(include "fu-version.scm")
(include "command-line.scm")
(include "find-files.scm")

(define-record handler cmd help proc)

(define (define-command cmd help proc)
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
           (colorize match 'red)
           match)))))

;; A one-argument procedure (predicate) that will be given a file
;; path, and it should return #f or a truthy value.  #f specifies the
;; file path should be excluded from results, and a truthy value
;; specifies the file path should be included in results.
(define constraints (make-parameter identity))

(define fu-editor
  (make-parameter
   (lambda (file)
     (system (sprintf "emacs ~a" (qs file)))
     (print-selected-file file))))

(define fu-viewer
  (make-parameter
   (lambda (file)
     (system (sprintf "less -R ~a" (qs file)))
     (print-selected-file file))))

(define fu-pager
  (make-parameter
   (case (software-type)
     ((windows) "more /s")
     (else "less"))))

(define fu-actions
  (make-parameter
   (lambda (selection)
     (when (terminal-port? (current-output-port))
       (print-selected-file selection)
       (let ((option (prompt '("View" "Edit") identity)))
         (if (zero? option)
             ((fu-viewer) selection)
             ((fu-editor) selection)))))))

;;; Procedures

(define (colorize text color)
  (string-append
   (case color
     ((red) "\x1b[31;1m")
     ((green) "\x1b[32;1m")
     ((blue) "\x1b[34;1m")
     (else ""))
   text
   "\x1b[0m"))

(define (print-selected-file path)
  (print (colorize
          (if (absolute-pathname? path)
              path
              (make-pathname (current-directory) path))
          'blue)))

;; Adapted from chicken-doc (thanks zb)
(define (with-output-to-pager thunk)
  (cond ((get-environment-variable "EMACS")
         (thunk))  ; Don't page in emacs subprocess.
        ((not (terminal-port? (current-output-port)))
         (thunk))  ; Don't page if stdout is not a TTY.
        (else
         (unless (get-environment-variable "LESS")
           (setenv "LESS" "FRXis"))  ; Default 'less' options
         (let ((pager (or (get-environment-variable "PAGER")
                          (fu-pager))))
           (if (or (not pager) (string=? pager "cat"))
               (thunk)
               ;; with-output-to-pipe does not close pipe on
               ;; exception, borking tty
               (let ((pipe (open-output-pipe pager))
                     (rv #f))
                 (handle-exceptions exn
                   (begin (close-output-pipe pipe)
                          (signal exn))
                   ;; Can't reliably detect if pipe open fails.
                   (set! rv (with-output-to-port pipe thunk)))
                 (close-output-pipe pipe)
                 rv))))))

(define (remove-command! cmd)
  (handlers (alist-delete! cmd (handlers))))

(define (die! fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (exit 1))

(define (format-match path pattern #!key (pre-formatter identity))
  ;; pre-formatter is a one argument procedure that will be given the
  ;; raw option, before the coloring.
  (let ((highlighted
         (irregex-replace/all pattern
                              (pre-formatter path)
                              (lambda (m)
                                ((match-highlighter)
                                 (irregex-match-substring m))))))
    (if (directory? path)
        (string-append highlighted "/")
        highlighted)))

(define (format-matches pattern #!key (pre-formatter identity))
  (let ((compiled-pattern
         (irregex
          (if (sloppy-pattern? pattern)
              (sloppy->strict-ci-pattern pattern)
              pattern))))
    (lambda (option)
      (format-match option compiled-pattern pre-formatter: pre-formatter))))


(define (prompt options option-formatter #!key multiple-choices?)

  (define (inner-prompt)
    (with-output-to-pager
     (lambda ()
       (let loop ((i 0)
                  (options options))
         (unless (null? options)
           (printf "[~a] ~a [~a]\n"
                   i
                   (option-formatter (car options)) i)
           (loop (fx+ i 1) (cdr options))))
       (flush-output)))
    (display "Option (ENTER to abort): ")
    (read-line))

  (let ((len-options (length options)))
    (let loop ()
      (let ((choice (inner-prompt)))
        (if (equal? choice "")
            (exit 0)
            (let ((choices (map string->number (string-split choice " ,"))))
              (if (and (every identity choices)
                       (every (lambda (choice)
                                (fx< choice len-options)) choices))
                  (if multiple-choices?
                      choices
                      (if (null? (cdr choices))
                          (car choices)
                          (begin
                            (printf "Multiple selections are not allowed.\n")
                            (loop))))
                  (begin
                    (printf "~a: invalid option.\n" choice)
                    (loop)))))))))

(define (require-positive-integer-or-zero opt-name)
  (lambda (option)
    (or (and-let* ((n (string->number option))
                   ((exact? n))
                   ((fx>= n 0)))
          n)
        (die! "~a: a positive integer or zero is required." opt-name))))

(define (sloppy-pattern sre)
  `(w/nocase (seq (* nonl) ,sre (* nonl))))

(define (sloppy->strict-pattern sre)
  (last (butlast (cadr sre))))

(define (sloppy->strict-ci-pattern sre)
  `(w/nocase ,(sloppy->strict-pattern sre)))

(define (sloppy-pattern? sre)
  (and (pair? sre)
       (eq? 'w/nocase (car sre))))

(define (fu-find-files pattern
                       #!key dir
                             depth
                             (except '())
                             match-full-path?
                             dotfiles?
                             (constraint identity))
  (let ((cwd (current-directory))
        (pattern (irregex pattern))
        (except (map irregex except))
        (files
         (find-files (or dir ".")
                     test: (lambda (f)
                             (and ((constraints) f)
                                  (irregex-match pattern
                                                 (if match-full-path?
                                                     f
                                                     (pathname-strip-directory f)))
                                  (if except
                                      (not (any (lambda (e)
                                                  (irregex-search e f))
                                                except))
                                      #t)
                                  (constraint f)))
                     dotfiles: dotfiles?
                     limit: depth)))
    (reverse (map normalize-pathname (sort files string>?)))))

(define (maybe-prompt-files files pattern op #!key (pre-formatter identity)
                                                   multiple-choices?
                                                   prompt-single-result?)
  (cond ((null? files)
         (exit 1))
        ((and (null? (cdr files)) (not prompt-single-result?))
         (let ((path (car files)))
           (print (format-match path pattern pre-formatter: pre-formatter))
           (unless (directory? path)
             (op path))))
        (else
         (let ((choice (prompt files
                               (format-matches pattern pre-formatter: pre-formatter)
                               multiple-choices?: multiple-choices?)))
           (if multiple-choices?
               (for-each (lambda (choice)
                           (op (list-ref files choice)))
                         choice)
               (op (list-ref files choice)))))))

(define (check-pattern pattern/maybe-dir)
  ;; Return either <pattern> or (<pattern> . <dir>)
  (when (null? pattern/maybe-dir)
    (die! "Missing pattern."))
  (cond ((null? (cdr pattern/maybe-dir))
         (car pattern/maybe-dir))
        ((null? (cddr pattern/maybe-dir))
         (cons (cadr pattern/maybe-dir)
               (car pattern/maybe-dir)))
        (else
         (die! "Multiple patterns are not supported."))))

(define (prepare-pattern pattern strict?)
  (if strict?
      (string->sre pattern)
      (sloppy-pattern (string->sre pattern))))

(define (fu-find/operate op #!key (prompt? #t)
                                  (non-dirs-only? #t)
                                  (dir ".")
                                  multiple-choices?)
  (lambda args
    (let* ((parsed-args
            (parse-command-line args
                                `((-s)
                                  (-f)
                                  (-.)
                                  (-e . except)
                                  (-d . ,(require-positive-integer-or-zero '-d)))))
           (get-opt (lambda (option #!optional multiple?)
                      (if multiple?
                          (filter-map (lambda (opt)
                                        (and (eq? (car opt) option)
                                             (cdr opt)))
                                      parsed-args)
                          (alist-ref option parsed-args))))
           (terminal? (terminal-port? (current-output-port)))
           (str-pattern/maybe-dir (check-pattern (get-opt '--)))
           (str-pattern (if (pair? str-pattern/maybe-dir)
                            (car str-pattern/maybe-dir)
                            str-pattern/maybe-dir))
           (dirs (if (pair? str-pattern/maybe-dir)
                     (list (cdr str-pattern/maybe-dir))
                     ;; Backwards compatibility
                     (if (pair? dir)
                         dir
                         (list dir))))
           (except (get-opt '-e 'multiple))
           (full-path? (substring-index "/" str-pattern))
           (pattern (prepare-pattern str-pattern (get-opt '-s)))
           (files (append-map
                   (lambda (dir)
                     (fu-find-files pattern
                                    dir: dir
                                    depth: (get-opt '-d)
                                    match-full-path?: full-path?
                                    except: (and except (map string->sre except))
                                    dotfiles?: (get-opt '-.')
                                    constraint: (if non-dirs-only?
                                                    (lambda (f)
                                                      (not (directory? f)))
                                                    identity)))
                   dirs)))
      (if (and prompt? terminal?)
          (maybe-prompt-files files pattern op
                              multiple-choices?: multiple-choices?)
          (let ((op (if terminal? op print)))
            (for-each (lambda (file)
                        (op (qs ((format-matches pattern) file))))
                      files))))))

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

(define (define-built-in-commands)

  (define-command 'f
  #<<EOF
f [-s] [-f] [-d <depth>] [<dir>] <pattern>
  Find files that sloppily match <pattern> (a regular expression). If
  <dir> is provided, search in it, otherwise search in the current
  directory.  Sloppily means <pattern> will be surrounded by ".*"
  and will be case insensitive.
    -s:          strict mode -- disable sloppy mode.
    -e <except>: remove files matching <except> (not affected by -s)
    -f:          print full paths
    -d <depth>:  limit search to <depth>
    -.        :  list files whose name start with "."
EOF
  (fu-find/operate (fu-actions) non-dirs-only?: #f))

  (define-command 'v
  #<<EOF
v <f options> [<dir>] <pattern>
  Find files & view.
EOF
  (fu-find/operate (fu-viewer)))

  (define-command 'e
  #<<EOF
e <f options> [<dir>] <pattern>
  Find files & edit.
EOF
  (fu-find/operate (fu-editor))))


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

  ;; Ignore SIGPIPE, otherwise with-output-to-pipe would make fu exit
  ;; without displaying the prompt, in case user terminates the pager.
  (set-signal-handler! signal/pipe void)

  ;; define-built-in-commands should be called after load-conf, to
  ;; honor parameters that might have been configured by users
  (define-built-in-commands)

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
