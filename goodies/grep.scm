(define (git-grep action args)
  (let* ((pattern (last args))
         (opts (butlast args))
         (colorize? (terminal-port? (current-output-port)))
         (options (call-with-input-pipe
                   (sprintf
                    "git grep ~a ~a ~a"
                    (if colorize? "--color=always" "--color=never")
                    (string-intersperse opts)
                    (qs pattern))
                   read-lines))
         (get-filename
          (lambda (choice)
            ;; Let's hope filenames don't contain ":"
            (string-drop-right ;; remove \x1b[36m
             (car (string-split (list-ref options choice) ":"))
             5))))
    (cond ((null? options)
           (exit 1))
          ((null? (cdr options))
           (action (get-filename 0)))
          (else
           (action (get-filename (prompt options identity)))))))

(define (traditional-grep action args)
  ;; Assuming GNU grep
  (let* ((pattern (last args))
         (grep-opts (butlast args))
         (ignored-opts '("-q" "-quiet" "--silent" "-h" "--no-filename"))
         (colorize? (terminal-port? (current-output-port)))
         (options (call-with-input-pipe
                   ;; Make filename colorization explicit here so it
                   ;; can be removed later
                   (sprintf
                    "GREP_COLORS='fn=35' grep -r --with-filename ~a ~a ~a *"
                    (if colorize? "--color=always" "--color=never")
                    (string-intersperse
                     (remove (lambda (opt)
                               (member opt ignored-opts))
                             grep-opts))
                    (qs pattern))
                   read-lines))
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
           (action (get-filename (prompt options identity)))))))

(define (grep action args)
  (let loop ((dir (normalize-pathname (current-directory))))
    (cond ((directory-exists? ".git")
           (git-grep action args))
          ((or (equal? dir "/") (equal? dir "/."))
           (traditional-grep action args))
          (else
           (loop (normalize-pathname (make-pathname dir "..")))))))

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