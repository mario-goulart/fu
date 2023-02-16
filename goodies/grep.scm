(cond-expand
 (chicken-4
  (void))
 (chicken-5
  (import (chicken file)
          (chicken format)
          (chicken io)
          (chicken pathname)
          (chicken port)
          (chicken process)
          (chicken process-context)
          (chicken string))
  (import srfi-1 srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))

(define (run-grep dir grep-options pattern)
  (let ((anchor (current-directory))
        (ignored-opts '("-q" "-quiet" "--silent" "-h" "--no-filename")))
    (change-directory dir)
    (let ((matches
           (call-with-input-pipe
            ;; Make filename colorization explicit here so it can be
            ;; removed later
            (sprintf
             "GREP_COLORS='fn=35' grep -r --exclude .git --with-filename ~a ~a ~a *"
             (if output-is-terminal? "--color=always" "--color=never")
             (string-intersperse
              (remove (lambda (opt)
                        (member opt ignored-opts))
                      grep-options))
             (qs pattern))
            read-lines)))
      (change-directory anchor)
      matches)))

(define (grep action args #!key (dirs (list (current-directory))))
  ;; Assuming GNU grep
  (let* ((pattern (last args))
         (grep-options (butlast args))
         (options (append-map (lambda (dir)
                                (run-grep dir grep-options pattern))
                              dirs))
         (get-filename
          ;; Ugly hack to remove ANSI escape sequences to colorize
          ;; the filename
          (lambda (choice)
            ;; Let's hope filenames don't contain ":"
            (let ((filename (car (string-split (list-ref options choice) ":"))))
              ;; Remove ansi escape sequences that grep uses to
              ;; colorize filenames
              ;; "\x1b[35m\x1b[K<the-filename>\x1b[m\x1b[K\x1b[36m\x1b[K"
              (string-drop-right (substring filename 8) 14)))))
    (cond ((null? options)
           (exit 1))
          ((null? (cdr options))
           (action (get-filename 0)))
          (else
           (if (terminal-port? (current-output-port))
               (action (get-filename (prompt options identity)))
               (for-each print options))))))

(define-command 'g
  "g <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. Upon finding results,
  this command will prompt for the action."
  (lambda args
    (grep (fu-actions) args)))

(define-command 'gv
  "gv <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. The action to be applied
  to the selection is 'view'."
  (lambda args
    (grep (fu-viewer) args)))

(define-command 'ge
  "ge <grep options> <pattern>
  Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
  will run 'git grep <git grep options> <pattern>. The action to be applied
  to the selection is 'edit'."
  (lambda args
    (grep (fu-editor) args)))
