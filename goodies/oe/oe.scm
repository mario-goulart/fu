;; TODO: x: -e to indicate an expression.  Example: oe x -e '${AVAR}/${BVAR}'

(use data-structures extras files ports posix srfi-1 srfi-13 utils)
(use html-parser sxml-transforms)

(define *build-dir* #f)
(define *local-conf-file* #f)
(define *bitbake-data* #f)
(define *fu-oe-data-dir* #f)
(define *cached-oe-data-file* #f)
(define *tracked-config-files* #f)
(define *cached-oe-variables*
  '(DEPLOY_DIR BBLAYERS TMPDIR PACKAGE_CLASSES))
(define *documentation-cache-file* #f)

(define (parse-bitbake-output bitbake-data-file)
  ;; Return a list of unparsed variable context blocks
  (let loop ((lines (read-lines bitbake-data-file))
             (cur-var #f)
             (blocks '())
             (context '()))
    (if (null? lines)
        (butlast (reverse (cons context blocks)))
        (let ((line (car lines)))
          (if cur-var
              (cond ((string-prefix? (conc cur-var "=") line)
                     ;; The end of a context block
                     (loop (cdr lines)
                           #f
                           (cons (append (list (string->symbol cur-var))
                                         (reverse context)
                                         (list line))
                                 blocks)
                           '()))
                    ((string-prefix? (string-append "# $") line)
                     ;; The start of the _next_ context block
                     (loop lines
                           #f
                           (cons (cons (string->symbol cur-var)
                                       (reverse context))
                                 blocks)
                           '()))
                    (else
                     ;; The body of a context block
                     (loop (cdr lines)
                           cur-var
                           blocks
                           (cons line context))))
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
  (let ((block (alist-ref var *bitbake-data*)))
    (if block
        (let loop ((lines (alist-ref var *bitbake-data*))
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
  (set! *bitbake-data* (read-file *cached-oe-data-file*)))

(define (populate-bitbake-data! #!optional recipe)
  (unless *bitbake-data*
    (let ((raw-data-file (create-temporary-file))
          (anchor (current-directory)))
      (change-directory *build-dir*)
      (handle-exceptions exn
        (begin
          (delete-file* raw-data-file)
          (fprintf (current-error-port) "Error running bitbake.\n")
          (exit 1))
        (system* "bitbake -e ~a > ~a" (or recipe "") raw-data-file))
      (change-directory anchor)
      (let ((data (parse-bitbake-output raw-data-file)))
        (delete-file raw-data-file)
        (set! *bitbake-data* data)))))

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

(define (reverse-expand str-pattern pre-expansion?)
  (let ((pattern (irregex str-pattern)))
    (for-each (lambda (var)
                (let ((val ((if pre-expansion? car cdr)
                            (parse-variable-context-block var))))
                  (when (and val (irregex-search pattern val))
                    (printf "~a = ~a\n"
                            (colorize (symbol->string var) 'blue)
                            val))))
              (map car *bitbake-data*))))

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
            (if (any (lambda (pkg-class)
                       (memq pkg-class known-pkg-classes))
                     pkg-classes)
                (filter-map
                 (lambda (pkg-class)
                   (let ((dir (make-pathname deploy-dir
                                             (substring (symbol->string pkg-class)
                                                        8 ;; length of package_
                                                        ))))
                     (and (directory-exists? dir)
                          dir)))
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
      (apply system* (cons fmt args))
      (die! "ERROR: This operation requires the '~a' program to be installed."
            program)))

(define (package-view package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* "ar p ~a data.tar.gz | tar tvzf -" package))
      ((rpm)
       (run-if-program-available "rpm" "rpm -qlvp ~a" package))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-info package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* "ar p ~a control.tar.gz | tar xzf - ./control -O" package))
      ((rpm)
       (run-if-program-available "rpm" "rpm -qip ~a" package))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-scripts package)
  (let ((ext (string->symbol (pathname-extension package))))
    (case ext
      ((deb ipk)
       (system* "ar p ~a control.tar.gz | tar tzf -" package))
      (else (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-extract package)
  (let-values (((dir out-dir ext) (decompose-pathname package)))
    (if (file-exists? out-dir)
        (die! "~a already exists.  Won't clobber.  Aborting." out-dir)
        (create-directory out-dir 'recursively))
    (case (string->symbol ext)
      ((deb ipk)
       (system* "ar p ~a data.tar.gz | tar xzf - -C ~a" package out-dir))
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
      (when output-is-terminal?
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
           (read-file (make-pathname *fu-oe-data-dir* "config-sums.scm")))
          (actual-sums
           (map (lambda (file)
                  (cons file
                        (sha256-sum (make-pathname (list *build-dir* "conf")
                                                   file))))
                *tracked-config-files*)))
      (any (lambda (file)
             (not (equal? (alist-ref file actual-sums equal?)
                          (alist-ref file stored-sums equal?))))
           *tracked-config-files*))))

(define (write-config-sums!)
  (with-output-to-file (make-pathname *fu-oe-data-dir* "config-sums.scm")
    (lambda ()
      (for-each
       (lambda (file)
         (pp (cons file
                   (sha256-sum (make-pathname (list *build-dir* "conf")
                                              file)))))
       *tracked-config-files*))))

(define (write-basic-oe-data!)
  (debug 2 "Writing basic OE data")
  (populate-bitbake-data!)
  (with-output-to-file *cached-oe-data-file*
    (lambda ()
      (for-each (lambda (var)
                  (pp (cons var
                            (cdr (alist-ref var *bitbake-data*)))))
                *cached-oe-variables*)))
  ;; Maybe bitbake must have to be rerun in a recipe-specific basis,
  ;; so we just empty the in-memory data
  (set! *bitbake-data* #f))

(define (maybe-store-basic-oe-data!)
  (when (or (config-changed?)
            (not (file-exists? *cached-oe-data-file*)))
    (debug 0 "Configuration files have changed.  Updating sums and cache...")
    (write-config-sums!)
    (write-basic-oe-data!)))

(define (oe-git-grep action args)
  (let* ((pattern (last args))
         (opts (butlast args))
         (options
          (append-map
           (lambda (dir)
             (change-directory dir)
             (map (lambda (line)
                    (cons dir line))
                  (call-with-input-pipe
                   (sprintf
                    "git --no-pager grep ~a ~a ~a"
                    (if output-is-terminal? "--color=always" "--color=never")
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
                              5)))))
         (colorize-layer
          (lambda (option #!optional (interactive? #t))
            (if (and output-is-terminal? interactive?)
                (make-pathname (colorize
                                (pathname-strip-directory (car option))
                                'blue)
                               (cdr option))
                ;; In non-interactive mode, keep the full path
                (make-pathname (car option) (cdr option))))))
    (cond ((null? options)
           (exit 1))
          ((not action)
           (for-each (lambda (option)
                       (print (colorize-layer option #f)))
                     options))
          ((null? (cdr options))
           (action (get-filename 0)))
          (else
           (if output-is-terminal?
               (action
                (get-filename
                 (prompt (map colorize-layer options) identity)))
               (for-each (compose print colorize-layer) options))))))

(define (find-oe-variable str-pattern)
  (let* ((vars (map (compose symbol->string car) *bitbake-data*))
         (pattern (prepare-pattern str-pattern #f))
         (results (filter (lambda (var)
                            (irregex-search pattern var))
                          vars)))
    (if (null? results)
        (die! "No match.")
        (if output-is-terminal?
            (let* ((var-choice (prompt results (format-matches pattern)))
                   (variable (string->symbol (list-ref results var-choice)))
                   (action-choice (prompt '("Expand" "Documentation") identity)))
              (if (zero? action-choice)
                  (show-variable-expansions variable)
                  (show-variable-documentation variable)))
            (for-each print results)))))

(define +variables-to-replace+
  ;; List of varibles to replace in log files mapped to the method to
  ;; be used to obtain their values.
  ;
  ;; Warning: the order of the variables below is important!
  ;; Beware of unintended substring replacements.
  `((B . ,get-var)
    (S . ,get-var)
    (WORKDIR . ,get-var)
    (TMPDIR . ,get-var)
    (HOME . ,(compose get-environment-variable symbol->string))))

(define maybe-replace-variables
  (let ((vars/vals #f))
    (lambda (line)
      (unless vars/vals
        (set! vars/vals
          ;; Warning: the order of the variables below is important!
          ;; Beware of unintended substring replacements.
          (map-in-order (lambda (var/method)
                          (let ((var (car var/method))
                                (method (cdr var/method)))
                            (cons var (method var))))
                        +variables-to-replace+)))
      (let loop ((vars/vals vars/vals)
                 (line line))
        (if (null? vars/vals)
            line
            (let ((var/val (car vars/vals)))
              (loop (cdr vars/vals)
                    (string-translate* line
                                       `((,(cdr var/val) .
                                          ,(string-append "$" (symbol->string
                                                               (car var/val)))))))))))))

(define (format-command-lines file replace-variables? break-compiler-command-lines?)
  (let ((port (open-input-file file))
        (prev #f)
        (show-line (lambda (line)
                     (if replace-variables?
                         (print (maybe-replace-variables line))
                         (print line)))))
    (when replace-variables?
      (print "==========[ Replacements made by oe ]==========")
      (for-each (lambda (var/method)
                  (let ((var (car var/method))
                        (method (cdr var/method)))
                    (print var " = " (method var))))
                +variables-to-replace+)
      (print (make-string 47 #\=) "\n"))
    (let loop ((command-line-counter 0))
      (let ((line (read-line port)))
        (unless (eof-object? line)
          (if break-compiler-command-lines?
              (let ((tokens (string-split line)))
                (if (> (length tokens) 0)
                    (let ((first-token (car tokens))
                          (dashes (string-* "-" 15)))
                      (set! prev #f)
                      (if (any (lambda (suffix)
                                 (string-suffix? suffix first-token))
                               '("gcc" "g++" "xgcc" "clang" "clang++" "javac"))
                          (let ((lines (list first-token)))
                            (printf "~a[ command line ~a ]~a\n"
                                    dashes command-line-counter dashes)
                            (for-each
                             (lambda (token)
                               (if (member prev '("-o" "-isystem" "-include"))
                                   (set-car! lines
                                             (string-append (car lines) " " token))
                                   (set! lines (cons token lines)))
                               (set! prev token))
                             (cdr tokens))
                            (let ((rev-lines (reverse lines)))
                              (show-line (car rev-lines))
                              (for-each (lambda (line)
                                          (show-line (string-append "  " line)))
                                        (cdr rev-lines)))
                            (loop (fx+ 1 command-line-counter)))
                          (show-line line)))
                    (show-line line))
                (loop command-line-counter))
              (begin
                (show-line line)
                (loop (fx+ 1 command-line-counter)))))))
    (close-input-port port)))

(define (find-log-files run-cmd? recipe task break-compiler-command-lines? replace-variables?)
  (let* ((log-type (if run-cmd? "run" "log"))
         (task-pattern
          (prepare-pattern
           (if task
               (string-append log-type "\\.do_" task)
               (string-append log-type "\\..*"))
           #f))
         (recipe-temp-dir (make-pathname (get-var 'WORKDIR) "temp")))
    (when (directory-exists? recipe-temp-dir)
      (maybe-prompt-files
       (filter symbolic-link?
               (fu-find-files task-pattern
                              dir: recipe-temp-dir))
       task-pattern
       (lambda (file)
         (if (or break-compiler-command-lines? replace-variables?)
             ;; with-output-to-pager doesn't seem to be handling SIGPIPE
             ;; as it should, so here an ugly hack goes.
             (begin
               (handle-exceptions exn
                 'ignore
                 (with-output-to-pager
                  (lambda ()
                    (format-command-lines file
                                          replace-variables?
                                          break-compiler-command-lines?))))
               (print-selected-file file))
             ((fu-viewer) file)))
       pre-formatter: pathname-strip-directory))))

;;; Documentation stuff
(define oe-documentation-uri
  (make-parameter
   "https://git.yoctoproject.org/cgit/cgit.cgi/yocto-docs/plain/documentation/ref-manual/ref-variables.xml"))

(define oe-documentation-validity
  (make-parameter (* 24 7 60 60))) ;; 1 week

(define yocto-doc-rules
  (let ((ignore (lambda args '()))
        (return-body (lambda (tag . args) args)))
    `((*COMMENT* . ,ignore)
      (glossentry . ,(lambda (tag . args)
                       (sxml->html ;; from html-parser
                        `(html
                          (body ,args)))))
      (glossterm . ,(lambda (tag . args) `(h1 ,args)))
      (glossdef . ,return-body)
      (para . ,(lambda (tag . args) `(p ,args)))
      (itemizedlist . ,(lambda (tag . args) `(p (ul ,args))))
      (note . ,(lambda (tag . args) `(blockquote "WARNING: " ,args)))
      (listitem . ,(lambda (tag . args) `(li ,args)))
      (link . ,return-body)
      (ulink . ,return-body)
      (linkend . ,return-body)
      (replaceable . ,(lambda (tag arg) `(i ,arg)))
      (literallayout . ,(lambda (tag . args) `(blockquote ,args)))
      (emphasis . ,(lambda (tag . args) `(b ,args)))
      (filename . ,(lambda (tag . f) f))
      (class . ,ignore)
      (role . ,ignore)
      (info . ,ignore)
      (id . ,ignore)
      (title . ,ignore)
      (@ . ,ignore)
      (*text* . ,(lambda (tag . text) text))
      (*default . ,identity)
      )))

(define (get-glossentries sxml)
  (let loop ((sxml sxml))
    (if (null? sxml)
        '()
        (let ((form (car sxml)))
          (if (pair? form)
              (cond ((memq (car form) '(chapter glossary))
                     (get-glossentries (cdr form)))
                    ((eq? (car form) 'glossdiv)
                     (append (get-glossentries (cdr form))
                             (get-glossentries (cdr sxml))))
                    ((eq? (car form) 'glossentry)
                     (cons form (loop (cdr sxml))))
                    (else (loop (cdr sxml))))
              (loop (cdr sxml)))))))

(define (get-glossentry-term glossentry)
  (let loop ((forms (cdr glossentry)))
    (if (null? forms)
        'unknown-glossterm
        (let ((form (car forms)))
          (if (and (pair? form) (eq? (car form) 'glossterm))
              (string->symbol (cadr form))
              (loop (cdr forms)))))))

(define (xml->sexp-glossentries xml-port)
  (let ((sxml (cddr (html->sxml xml-port))))
    (map (lambda (glossentry)
           (cons (get-glossentry-term glossentry)
                 glossentry))
         (get-glossentries sxml))))

(define (maybe-update-documentation!)
  (define (update-documentation!)
    (print "Updating documentation data...")
    (let* ((p (open-input-pipe (sprintf "wget -q -O - ~A 2>&1"
                                        (oe-documentation-uri))))
           (output (read-all p))
           (exit-status (arithmetic-shift (close-input-pipe p) -8)))
      (with-output-to-file "test.out" (cut print output))
      (if (zero? exit-status)
          (let ((glossentries
                 (call-with-input-string output xml->sexp-glossentries)))
            (with-output-to-file *documentation-cache-file*
              (lambda ()
                (pp glossentries))))
          (error 'maybe-update-documentation!
                 "wget error.  Exit code:" exit-status))))
  (if (file-exists? *documentation-cache-file*)
      (let ((cache-last-modified
             (vector-ref (file-stat *documentation-cache-file*) 8))
            (now (current-seconds)))
        (when (< cache-last-modified (- now (oe-documentation-validity)))
          (update-documentation!)))
      (update-documentation!)))

(define get-variables-documentation
  (let ((doc #f))
    (lambda ()
      (unless doc
        (maybe-update-documentation!)
        (set! doc (with-input-from-file *documentation-cache-file* read)))
      doc)))

(define (show-variable-documentation variable)
  (let ((glossentry (alist-ref variable (get-variables-documentation)))
        (rules
         (append
          yocto-doc-rules
          universal-conversion-rules*)))
    (if glossentry
        (with-output-to-pipe (sprintf "lynx -stdin -dump | ~a" (fu-pager))
          (lambda ()
            (SRV:send-reply (pre-post-order* glossentry rules))))
        (die! "Could not find documentation for ~a" variable))))

(define (variable-documentation str-pattern)
  (unless (program-available? "wget")
    (die! "This command requires wget, but it doesn't seem to be available."))
  (unless (program-available? "lynx")
    (die! "This command requires lynx, but it doesn't seem to be available."))
  (let* ((vars (map (compose symbol->string car) (get-variables-documentation)))
         (pattern (prepare-pattern str-pattern #f))
         (results (filter (lambda (var)
                            (irregex-search pattern var))
                          vars)))
    (cond ((null? results)
           (die! "No variable matches ~a" str-pattern))
          ((null? (cdr results))
           (show-variable-documentation (string->symbol (car results))))
          (else
           (if output-is-terminal?
               (let* ((choice (prompt results (format-matches pattern)))
                      (variable (string->symbol (list-ref results choice))))
                 (show-variable-documentation variable))
               (for-each print results))))))

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

reverse-expand [-p] <regex> [<recipe>]
  Short command: rx.  Look for <regex> in values of variables in the BitBake
  metadata.  In other words, given a value, find which variables hold that
  value.  If -p is provided, search will me made in values before their
  expansion.

sysroot-find [-s] [-f] [-d <depth>] <pattern>
  Short command: sf.  Similar to `find', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-view <`sysroot-find' options> <pattern>
  Short command: sv.  Similar to `view', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-edit <`sysroot-find' options> <pattern>
  Short command: se.  Similar to `edit', but instead of searching for files
  in the sources directory, search in the sysroot directories.

doc <pattern>
  Short command: d.  Show documentation for variables matching <pattern>.

pkg-find <pattern>
  Short command: pf.  Find packages that match <pattern>.

pkg-view <pattern>
  Short command: pv.  Find packages & show their content.

pkg-info <pattern>
  Short command: pi.  Find packages & show their information data.

pkg-scripts
  Short command: ps.  Find packages & show their scripts.

pkg-extract
  Short command: px.  Find packages & extract them to the current directory.

log [-f] [-r] <recipe> [<task>]
  Short command: l.  Show log file for <task> executed for <recipe>.  If
  <task> is not provided, all log files will be displayed as options for
  selection.
  -f: format command lines.  Will break compiler command line arguments into
      individual lines, hopefully improving readability.
  -r: replace variable values.  Will replace some values by their
      corresponding variable, hopefully improving readability.

run <recipe> [<task>]
  Short command: r.  Show run scripts for <task> executed for <recipe>. If
  <task> is not provided, all log files will be displayed as options for
  selection.

grep [<grep options>] <pattern>
  Short command: g. Search for <pattern> in the source directories.

grep-view [<grep options>] <pattern>
  Short command: gv.  Search for <pattern> in the source directories and
  call the viewer on the selected option.

grep-edit [<grep options>] <pattern>
  Short command: ge.  Search for <pattern> in the source directories and
  call the editor on the selected option.

variable-find <pattern> [<recipe>]
  Find variables matching <pattern>.
")

(define-command 'oe
  "oe <options>
   Run 'oe help' for the full help message."
  (lambda args
    (when (null? args)
      (die! oe-usage))

    (when (member (car args) '("help" "h"))
      (print oe-usage)
      (exit))

    (let ((builddir (get-environment-variable "BUILDDIR")))
      (unless builddir
        (die! "The build environment is not set.  Aborting."))
      (set! *build-dir* builddir)
      (set! *local-conf-file*
        (make-pathname (list *build-dir* "conf") "local.conf"))
      (set! *fu-oe-data-dir* (make-pathname builddir ".fu-oe"))
      (set! *cached-oe-data-file* (make-pathname *fu-oe-data-dir*
                                                 "cached-variables.scm"))
      (set! *tracked-config-files*
        (filter (lambda (file)
                  (file-exists? (make-pathname (list *build-dir* "conf")
                                               file)))
                '("local.conf" "bblayers.conf" "site.conf")))
      (set! *documentation-cache-file*
        (make-pathname *fu-oe-data-dir* "cached-documentation.scm")))

    (create-directory *fu-oe-data-dir* 'recursively)
    (maybe-store-basic-oe-data!)

    (let ((cmd (string->symbol (car args)))
          (oe-args (cdr args)))

      (case cmd
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
                      (for-each print (alist-ref var *bitbake-data*)))
                     (else
                      (show-variable-expansions var))))))

        ((reverse-expand rx)
         (let* ((non-options (remove (lambda (arg)
                                       (string-prefix? "-" arg))
                                     oe-args))
                (recipe (and (not (null? (cdr non-options)))
                             (cadr non-options))))
           (when (null? non-options)
             (die! "Missing regex."))
           (populate-bitbake-data! recipe)
           (reverse-expand (car non-options) (member "-p" oe-args))))

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
                                         (get-oe-sysroots-directory)))
                                 non-dirs-only?: (not (memq cmd '(find f sysroot-find sf))))
                oe-args))

        ((pkg-find pkg-view pkg-info pkg-scripts pkg-extract
          pf pv pi ps px)
         (populate-bitbake-data-from-cache!)
         (let ((pkg-dirs (get-oe-packages-directories)))
           (unless (null? pkg-dirs)
             (apply (fu-find/operate (case cmd
                                       ((pkg-find pf) (package-actions))
                                       ((pkg-view pv) package-view)
                                       ((pkg-info pi) package-info)
                                       ((pkg-scripts ps) package-scripts)
                                       ((pkg-extract px) package-extract))
                                     interactive-action?: #f
                                     dir: pkg-dirs)
                    oe-args))))

        ((log run l r)
         (let ((run-cmd? (memq cmd '(run r))))
           (if (null? oe-args)
               (die! "Missing recipe.  Aborting.")
               (let* ((non-options (remove (lambda (arg)
                                             (string-prefix? "-" arg))
                                           oe-args))
                      (recipe (string->symbol (car non-options)))
                      (task (and (not (null? (cdr non-options)))
                                 (cadr non-options)))
                      (break-compiler-command-lines? (and (not run-cmd?)
                                                  (member "-f" oe-args)))
                      (replace-variables? (and (not run-cmd?)
                                               (member "-r" oe-args))))
                 (populate-bitbake-data! recipe)
                 (find-log-files run-cmd?
                                 recipe
                                 task
                                 break-compiler-command-lines?
                                 replace-variables?)))))

        ((grep grep-view grep-edit g gv ge)
         (populate-bitbake-data-from-cache!)
         (oe-git-grep (case cmd
                        ((grep g) #f)
                        ((grep-view gv) (fu-viewer))
                        ((grep-edit ge) (fu-editor)))
                      oe-args))

        ((variable-find vf)
         (let ((recipe (and (not (null? (cdr oe-args)))
                            (cadr oe-args))))
           (populate-bitbake-data! recipe)
           (find-oe-variable (car oe-args))))

        ((doc d)
         (if (null? oe-args)
             (die! "Missing variable.")
             (variable-documentation (car oe-args))))

        (else (die! "Unknown command: ~a" cmd))))))
