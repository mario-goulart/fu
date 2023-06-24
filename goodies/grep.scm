(cond-expand
 (chicken-5
  (import (chicken file)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken pathname)
          (chicken port)
          (chicken process)
          (chicken process-context)
          (chicken string))
  (import srfi-1 srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))


(define (remove-ansi-escapes text)
  (irregex-replace/all "\\x1b\\[[0-9;]*[mK]" text ""))

(define program-available?
  (let ((paths (string-split (get-environment-variable "PATH") ":")))
    (lambda (program)
      (let loop ((paths paths))
        (if (null? paths)
            #f
            (let ((path (car paths)))
              (or (file-exists? (make-pathname path program))
                  (loop (cdr paths)))))))))

(define (grep-wrapper cmd-pattern dir grep-options ignored-options pattern)
  (let ((anchor (current-directory)))
    (change-directory dir)
    (let ((matches
           (call-with-input-pipe
            ;; Make filename colorization explicit here so it can be
            ;; removed later
            (sprintf
             cmd-pattern
             (if output-is-terminal? "--color=always" "--color=never")
             (string-intersperse
              (remove (lambda (opt)
                        (member opt ignored-options))
                      grep-options))
             (qs pattern))
            read-lines)))
      (change-directory anchor)
      (map (lambda (match)
             (cons dir match))
           matches))))

(define (run-ripgrep dir grep-options pattern)
  (grep-wrapper
   "rg --no-heading -N ~a ~a ~a *"
   dir
   grep-options
   '("-q" "--quiet" "--heading" "-h" "--no-filename")
   pattern))

(define (run-grep dir grep-options pattern)
  (grep-wrapper
   "grep -r --exclude .git --with-filename ~a ~a ~a *"
   dir
   grep-options
   '("-q" "-quiet" "--silent" "-h" "--no-filename")
   pattern))

(define grepper
  (if (program-available? "rg")
      run-ripgrep
      run-grep))

(define (grep action args #!key (dirs (list (current-directory))))
  ;; Assuming GNU grep
  (let* ((pattern (last args))
         (grep-options (butlast args))
         (options
          (remove null?
                  (append-map (lambda (dir)
                                (grepper dir grep-options pattern))
                              dirs)))
         (get-filename
          ;; Ugly hack to remove ANSI escape sequences to colorize
          ;; the filename
          (lambda (choice)
            (let* ((selection (list-ref options choice))
                   ;; Let's hope filenames don't contain ":"
                   (filename (car (string-split (cdr selection) ":"))))
              (make-pathname (car selection) (remove-ansi-escapes filename))))))
    (cond ((null? options)
           (exit 1))
          ((null? (cdr options))
           (action (get-filename 0)))
          (else
           (let ((matches
                  ;; In case of multiple dirs, prepend dirs to
                  ;; filenames
                  (if (null? (cdr dirs))
                      (map cdr options)
                      (map (lambda (opt)
                             (make-pathname (car opt) (cdr opt)))
                           options))))
             (if (terminal-port? (current-output-port))
                 (action (get-filename (prompt matches identity)))
                 (for-each print matches)))))))

(define-command 'g
  "g <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. Upon finding results,
  this command will prompt for the action."
  (lambda (args)
    (grep (fu-actions) args)))

(define-command 'gv
  "gv <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. The action to be applied
  to the selection is 'view'."
  (lambda (args)
    (grep (fu-viewer) args)))

(define-command 'ge
  "ge <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. The action to be applied
  to the selection is 'edit'."
  (lambda (args)
    (grep (fu-editor) args)))
