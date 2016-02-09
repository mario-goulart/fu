;; TODO: parse local.conf to expand MACHINE and DISTRO
;; TODO: x: -e to indicate an expression.  Example: oe x -e '${AVAR}/${BVAR}'

(use data-structures extras files ports posix srfi-1 srfi-13 utils)

(define +build-dir+ #f)
(define +local-conf-file+ #f)
(define +bitbake-data+ #f)
(define +fu-oe-data-dir+ #f)
(define +cached-oe-data-file+ #f)
(define +tracked-config-files+ #f)

(define +cached-oe-variables+
  '(DEPLOY_DIR BBLAYERS TMPDIR PACKAGE_CLASSES))

(define (parse-bitbake-output bitbake-data-file)
  ;; Return a list of unparsed variable context blocks
  (let loop ((lines (read-lines bitbake-data-file))
             (cur-var #f)
             (blocks '())
             (context '()))
    (if (null? lines)
        (reverse (cons context blocks))
        (let ((line (car lines)))
          (if cur-var
              (if (string-prefix? (string-append "# $") line)
                  ;; The end of a context block
                  (loop lines
                        #f
                        (cons (cons (string->symbol cur-var)
                                    (reverse context))
                              blocks)
                        '())
                  ;; The body of a context block
                  (loop (cdr lines)
                        cur-var
                        blocks
                        (cons line context)))
              ;; No variable context block being processed
              (if (string-prefix? (string-append "# $") line)
                  ;; The start of a context block
                  (loop (cdr lines)
                        (substring (cadr (string-split line)) 1)
                        blocks
                        (list line))
                  ;; Unhandled lines
                  (loop (cdr lines)
                        #f
                        blocks
                        '())))))))

(define (parse-variable-context-block var)
  ;; Returns a pair (<pre-expansion-value> . <final-value>)
  (let ((block (alist-ref var +bitbake-data+)))
    (if block
        (let loop ((lines (alist-ref var +bitbake-data+))
                   (pre-expansion-value #f)
                   (final-value #f))
          (if (null? lines)
              (cons (or pre-expansion-value final-value) final-value)
              (let ((line (car lines)))
                (cond
                 ((or (string-prefix? (conc var "=") line)
                      (string-prefix? (conc "export " var "=") line))
                  (loop (cdr lines)
                        pre-expansion-value
                        (substring line
                                   (+ 1 (string-index line
                                                      (cut char=? <> #\=))))))
                 ((eq? pre-expansion-value 'see-next-line)
                  (loop (cdr lines)
                        (string-trim (substring line 1)) ;; Remove "#" and trim
                        final-value))
                 ((equal? line "# pre-expansion value:")
                  (loop (cdr lines)
                        'see-next-line
                        final-value))
                 (else
                  (loop (cdr lines)
                        pre-expansion-value
                        final-value))))))
        (cons #f #f))))

(define (populate-bitbake-data-from-cache!)
  (set! +bitbake-data+ (read-file +cached-oe-data-file+)))

(define (populate-bitbake-data! #!optional recipe)
  (unless +bitbake-data+
    (let ((raw-data-file (create-temporary-file))
          (anchor (current-directory)))
      (handle-exceptions exn
        (begin
          (delete-file* raw-data-file)
          (exit 1))
        (change-directory +build-dir+)
        (system* (sprintf "bitbake -e ~a > ~a" (or recipe "") raw-data-file))
        (change-directory anchor))
      (let ((data (parse-bitbake-output raw-data-file)))
        (delete-file raw-data-file)
        (set! +bitbake-data+ data)))))

(define (string-* str n)
  (let loop ((n n)
             (new-str str))
    (cond ((fx<= n 0) "")
          ((fx= n 1) new-str)
          (else (loop (fx- n 1) (string-append new-str str))))))

(define (indent steps)
  (string-* "    " steps))

(define (show-variable-values var pre-expansion-value final-value)
  (print "== Pre-expansion value")
  (printf "~a${~a} = ~a\n"
          (indent (car pre-expansion-value)) var (cdr pre-expansion-value))
  (print "")
  (print "== Final value")
  (printf "~a${~a} = ~a\n"
          (indent (car final-value)) var (cdr final-value)))

(define (show-variable-expansions var)
  (let* ((vals (parse-variable-context-block var))
         (pre-expansion-value (car vals))
         (final-value (cdr vals)))
    (if (and pre-expansion-value final-value)
        (if (equal? pre-expansion-value final-value)
            (show-variable-values var
                                  (cons 0 pre-expansion-value)
                                  (cons 0 final-value))
            ;; FIXME: recursively expand
            (show-variable-values var
                                  (cons 0 pre-expansion-value)
                                  (cons 0 final-value)))
        (die! "No such variable: ~a" var))))

(define (get-var var)
  (with-input-from-string (cdr (parse-variable-context-block var)) read))

(define get-oe-source-directories
  (let ((dirs #f))
    (lambda ()
      (unless dirs
        (let ((bblayers (get-var 'BBLAYERS)))
          (set! dirs (string-split bblayers)))
        dirs))))

(define get-oe-sysroots-directory
  (let ((dir #f))
    (lambda ()
      (unless dir
        (let ((tmpdir (get-var 'TMPDIR)))
          (set! dir (make-pathname tmpdir "sysroots"))))
      dir)))

(define get-oe-packages-directories
  (let ((dirs #f))
    (lambda ()
      (unless dirs
        (let ((deploy-dir (get-var 'DEPLOY_DIR))
              (pkg-classes (map string->symbol
                                (string-split (get-var 'PACKAGE_CLASSES))))
              (known-pkg-classes
               '(package_rpm package_ipk package_deb package_tar)))
          (set! dirs
            (if (every (lambda (pkg-class)
                         (memq pkg-class known-pkg-classes))
                       pkg-classes)
                (map (lambda (pkg-class)
                       (make-pathname deploy-dir
                                      (substring (symbol->string pkg-class)
                                                 8 ;; length of package_
                                                 )))
                     pkg-classes)
                (list deploy-dir)))))
      dirs)))

(define program-available?
  (let ((paths (string-split (get-environment-variable "PATH") ":")))
    (lambda (program)
      (let loop ((paths paths))
        (if (null? paths)
            #f
            (let ((path (car paths)))
              (or (file-exists? (make-pathname path program))
                  (loop (cdr paths)))))))))

(define (run-if-program-available program fmt . args)
  (if (program-available? program)
      (system* (apply sprintf (cons fmt args)))
      (die! "ERROR: This operation requires the '~a' program to be installed."
            program)))

(define (package-view package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* (sprintf "ar p ~a data.tar.gz | tar tvzf -" package)))
      ((rpm)
       (run-if-program-available "rpm" "rpm -qlvp ~a" package))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-info package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* (sprintf "ar p ~a control.tar.gz | tar xzf - ./control -O"
                         package)))
      ((rpm)
       (run-if-program-available "rpm" "rpm -qip ~a" package))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-scripts package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* (sprintf "ar p ~a control.tar.gz | tar tzf -" package)))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-extract package)
  (let-values (((dir out-dir ext) (decompose-pathname package)))
    (if (file-exists? out-dir)
        (die! "~a already exists.  Won't clobber.  Aborting." out-dir)
        (create-directory out-dir 'recursively))
    (case (string->symbol ext)
      ((deb ipk)
       (system* "ar p data.tar.gz ~a | tar xzf - -C ~a" package out-dir))
      ((rpm)
       (change-directory out-dir)
       (run-if-program-available "rpm2cpio ~a | cpio -id" package))
      (else
       (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-actions)
  (lambda (selection)
    (let ((labels/actions `(("View"    . ,package-view)
                            ("Info"    . ,package-info)
                            ("Scripts" . ,package-scripts)
                            ("Extract" . ,package-extract))))
      (when (terminal-port? (current-output-port))
        (print-selected-file selection)
        (let ((option (prompt (map car labels/actions) identity)))
          ((cdr (list-ref labels/actions option)) selection))))))

(define (sha256-sum file)
  (car
   (string-split
    (with-input-from-pipe (sprintf "sha256sum ~a" (qs file)) read-all))))

(define debug-level
  (make-parameter
   (or (and-let* ((level (get-environment-variable "FU_OE_DEBUG")))
         (or (string->number level) 0))
       0)))

(define debug-formatter
  (make-parameter
   (lambda (level fmt)
     (sprintf "DEBUG[~a] ~a\n" level fmt))))

(define (debug level fmt . args)
  (when (<= level (debug-level))
    (apply fprintf `(,(current-error-port) ,((debug-formatter) level fmt) ,@args))))

(define (config-changed?)
  (handle-exceptions exn
    #t
    (let ((stored-sums
           (read-file (make-pathname +fu-oe-data-dir+ "config-sums.scm")))
          (actual-sums
           (map (lambda (file)
                  (cons file
                        (sha256-sum (make-pathname (list +build-dir+ "conf")
                                                   file))))
                +tracked-config-files+)))
      (any (lambda (file)
             (not (equal? (alist-ref file actual-sums equal?)
                          (alist-ref file stored-sums equal?))))
           +tracked-config-files+))))

(define (write-config-sums!)
  (with-output-to-file (make-pathname +fu-oe-data-dir+ "config-sums.scm")
    (lambda ()
      (for-each
       (lambda (file)
         (pp (cons file
                   (sha256-sum (make-pathname (list +build-dir+ "conf")
                                              file)))))
       +tracked-config-files+))))

(define (write-basic-oe-data!)
  (debug 2 "Writing basic OE data")
  (populate-bitbake-data!)
  (with-output-to-file +cached-oe-data-file+
    (lambda ()
      (for-each (lambda (var)
                  (pp (cons var
                            (cdr (alist-ref var +bitbake-data+)))))
                +cached-oe-variables+)))
  ;; Maybe bitbake must have to be rerun in a recipe-specific basis,
  ;; so we just empty the in-memory data
  (set! +bitbake-data+ #f))

(define (maybe-store-basic-oe-data!)
  (when (or (config-changed?)
            (not (file-exists? +cached-oe-data-file+)))
    (debug 0 "Configuration files have changed.  Updating sums and cache...")
    (write-config-sums!)
    (write-basic-oe-data!)))

(define (oe-git-grep action args)
  (let* ((pattern (last args))
         (opts (butlast args))
         (colorize? (terminal-port? (current-output-port)))
         (options
          (append-map
           (lambda (dir)
             (change-directory dir)
             (map (lambda (line)
                    (cons dir line))
                  (call-with-input-pipe
                   (sprintf
                    "git --no-pager grep ~a ~a ~a"
                    (if colorize? "--color=always" "--color=never")
                    (string-intersperse opts)
                    (qs pattern))
                   read-lines)))
           (string-split (get-var 'BBLAYERS))))
         (get-filename
          (lambda (choice)
            (let ((option (list-ref options choice)))
              ;; Let's hope filenames don't contain ":"
              (make-pathname (car option)
                             (string-drop-right ;; remove \x1b[36m
                              (car (string-split (cdr option) ":"))
                              5))))))
    (cond ((null? options)
           (exit 1))
          ((null? (cdr options))
           (action (get-filename 0)))
          (else
           (if (terminal-port? (current-output-port))
               (action
                (get-filename
                 (prompt
                  (map (lambda (o)
                         (make-pathname (colorize
                                         (pathname-strip-directory (car o))
                                         'blue)
                                        (cdr o)))
                       options)
                  identity)))
               (for-each print options))))))

(define oe-usage
  "Usage: oe <command> <options>

<command>s:

find [-s] [-f] [-d <depth>] <pattern>
  Short command: f.  Find files that sloppily match <pattern> (a regular
  expression) in the sources directory.  Sloppily means <pattern> will
  be surrounded by \".*\" and will be case insensitive.
    -s:          strict mode -- disable sloppy mode.
    -e <except>: remove files matching <except> (not affected by -s)
    -f:          print full paths
    -d <depth>:  limit search to <depth>
    -.        :  list files whose name start with \".\"

view <`find' options> [<dir>] <pattern>
  Short command: v.  Find files & view.

edit <`find' options> [<dir>] <pattern>
  Short command: e.  Find files & edit.

expand [-s] [-u] <variable> [<recipe>]
  Short command: x.  Expand <variable>.  If <recipe> is provided, expand
  <variable> in the context of <recipe>.  If -s (short) is provided, only
  the final value will be printed.  If -u is provided, the raw output of
  'bitbake -e' for the variable in question will be printed.

sysroot-find [-s] [-f] [-d <depth>] <pattern>
  Short command: sf.  Similar to `find', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-view <`sysroot-find' options> <pattern>
  Short command: sv.  Similar to `view', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-edit <`sysroot-find' options> <pattern>
  Short command: se.  Similar to `edit', but instead of searching for files
  in the sources directory, search in the sysroot directories.

pkg-find
pkg-view
pkg-info
pkg-scripts
pkg-extract

log

run
")

(define-command 'oe
  "oe <options>
   Run 'oe help' for the full help message."
  (lambda args
    (when (null? args)
      (die! oe-usage))

    (let ((builddir (get-environment-variable "BUILDDIR")))
      (unless builddir
        (die! "The build environment is not set.  Aborting."))
      (set! +build-dir+ builddir)
      (set! +local-conf-file+
        (make-pathname (list +build-dir+ "conf") "local.conf"))
      (set! +fu-oe-data-dir+ (make-pathname builddir ".fu-oe"))
      (set! +cached-oe-data-file+ (make-pathname +fu-oe-data-dir+
                                                 "cached-variables.scm"))
      (set! +tracked-config-files+
        (filter (lambda (file)
                  (file-exists? (make-pathname (list +build-dir+ "conf")
                                               file)))
                '("local.conf" "bblayers.conf" "site.conf"))))

    (create-directory +fu-oe-data-dir+ 'recursively)
    (maybe-store-basic-oe-data!)

    (let ((cmd (string->symbol (car args)))
          (oe-args (cdr args)))
      (case cmd
        ((help h)
         (print oe-usage)
         (exit))

        ((expand x)
         (if (null? oe-args)
             (die! "Missing variable.  Aborting.")
             (let* ((non-options (remove (lambda (arg)
                                           (string-prefix? "-" arg))
                                         oe-args))
                    (var (string->symbol (car non-options)))
                    (recipe (and (not (null? (cdr non-options)))
                                 (cadr non-options))))
               (populate-bitbake-data! recipe)
               (cond ((member "-s" oe-args)
                      (print (cdr (parse-variable-context-block var))))
                     ((member "-u" oe-args)
                      (for-each print (alist-ref var +bitbake-data+)))
                     (else
                      (show-variable-expansions var))))))

        ((find edit view sysroot-find sysroot-view sysroot-edit
          f e v sf se sv)
         (populate-bitbake-data-from-cache!)
         (apply (fu-find/operate (case cmd
                                   ((find sysroot-find f sf) (fu-actions))
                                   ((edit sysroot-edit e se) (fu-editor))
                                   ((view sysroot-view v sv) (fu-viewer)))
                                 dir: (case cmd
                                        ((find edit view f e v)
                                         (get-oe-source-directories))
                                        ((sysroot-find sysroot-edit sysroot-view sf se sv)
                                         (get-oe-sysroots-directory))))
                (cdr args)))

        ((pkg-find pkg-view pkg-info pkg-scripts pkg-extract
          pf pv pi ps px)
         (populate-bitbake-data-from-cache!)
         (apply (fu-find/operate (case cmd
                                   ((pkg-find pf) (package-actions))
                                   ((pkg-view pv) package-view)
                                   ((pkg-info pi) package-info)
                                   ((pkg-scripts ps) package-scripts)
                                   ((pkg-extract px) package-extract))
                                 dir: (get-oe-packages-directories))
                (cdr args)))

        ((log l)
         (if (null? oe-args)
             (die! "Missing recipe.  Aborting.")
             (let* ((non-options (remove (lambda (arg)
                                           (string-prefix? "-" arg))
                                         oe-args))
                    (recipe (string->symbol (car non-options)))
                    (log (and (not (null? (cdr non-options)))
                              (cadr non-options))))
               (populate-bitbake-data! recipe)
               (let ((log-pattern
                      (prepare-pattern
                       (if log
                           (string-append "log\\.do_" log)
                           "log\\..*")
                       #f)))
                 (maybe-prompt-files
                  (filter symbolic-link?
                          (fu-find-files log-pattern
                                         dir: (make-pathname (get-var 'WORKDIR)
                                                             "temp")))
                  (if log log-pattern ".*")
                  (fu-viewer)
                  pre-formatter: pathname-strip-directory)))))

        ((run r)
         (die! "FIXME"))

        ((grep-view grep-edit gv ge)
         (populate-bitbake-data-from-cache!)
         (oe-git-grep (case cmd
                        ((grep-view gv) (fu-viewer))
                        ((grep-edit ge) (fu-editor)))
                      oe-args))

        ((d)
         (die! "FIXME"))

        (else (die! "Unknown command: ~a" cmd))))))
