;; TODO: x: -e to indicate an expression.  Example: oe x -e '${AVAR}/${BVAR}'

(use data-structures extras files ports posix srfi-1 srfi-13 utils)
(use html-parser sxml-transforms)

(define *build-dir* #f)
(define *local-conf-file* #f)
(define *bitbake-data* #f)
(define *fu-oe-data-dir* #f)
(define *cached-oe-data-file* #f)

;; Bump whenever new variables are added to *cached-oe-variables*
(define *cache-version* "3")

(define *cached-oe-variables*
  '(DEPLOY_DIR BBLAYERS TMPDIR PACKAGE_CLASSES BUILDHISTORY_DIR BUILDSTATS_BASE))
(define *documentation-cache-file* #f)

;; Parameters that can be set in the configuration file
(define oe-extra-layers
  ;; List of paths (strings) to extra layers (i.e., those that are not
  ;; in BBLAYERS)
  (make-parameter '()))

(define tracked-config-files
  ;; Files that, when modified, invalidate oe's cache
  (make-parameter
   (let ((build-dir (get-environment-variable "BUILDDIR")))
     (if build-dir ;; if BUILDDIR is not set, oe will abort anyway
         (filter-map (lambda (file)
                       (file-exists? (make-pathname (list build-dir "conf")
                                                    file)))
                     '("local.conf" "bblayers.conf" "site.conf"))
         '()))))

(define (safe-take lst n-items)
  (if n-items
      (if (< (length lst) n-items)
          lst
          (take lst n-items))
      lst))

(define (format-seconds seconds)
  (let ((str (number->string seconds)))
    (if (substring-index "." str)
        (let* ((tokens (string-split str "."))
               (frac (cadr tokens)))
          (string-append (car tokens)
                         "."
                         (if (> (string-length frac) 2)
                             (string-take frac 2)
                             frac)))
        str)))

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
        (let loop ((lines (or (alist-ref var *bitbake-data*) '()))
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
  (let ((val (parse-variable-context-block var)))
    (and (car val)
         (cdr val)
         (with-input-from-string (cdr val) read))))

(define get-oe-source-directories
  (let ((dirs #f))
    (lambda ()
      (unless dirs
        (let ((bblayers (get-var 'BBLAYERS)))
          (set! dirs (delete-duplicates
                      (append (string-split bblayers)
                              (oe-extra-layers))
                      equal?)))
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
       (run-if-program-available "rpm2cpio" "rpm2cpio ~a | cpio -id" package))
      (else
       (die! "ERROR: unsupported package file extension: ~a" ext)))))

(define (package-actions)
  (lambda (selection)
    (let ((labels/actions `(("View"    . ,package-view)
                            ("Info"    . ,package-info)
                            ("Scripts" . ,package-scripts)
                            ("Extract" . ,package-extract))))
      (cond
       (output-is-terminal?
        (print-selected-file selection)
        (let ((option (prompt (map car labels/actions) identity)))
          ((cdr (list-ref labels/actions option)) selection)))
       (else
        (print (qs selection)))))))

(define (sha256-sum file)
  (car
   (string-split
    (with-input-from-pipe (sprintf "sha256sum ~a" (qs file)) read-all))))

(define (config-changed?)
  (handle-exceptions exn
    #t
    (let ((stored-sums
           (read-file (make-pathname *fu-oe-data-dir* "config-sums.scm")))
          (actual-sums
           (map (lambda (file)
                  (cons file (sha256-sum file)))
                (tracked-config-files))))
      (any (lambda (file)
             (not (equal? (alist-ref file actual-sums equal?)
                          (alist-ref file stored-sums equal?))))
           (tracked-config-files)))))

(define (write-config-sums!)
  (with-output-to-file (make-pathname *fu-oe-data-dir* "config-sums.scm")
    (lambda ()
      (for-each
       (lambda (file)
         (pp (cons file (sha256-sum file))))
       (tracked-config-files)))))

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
           (get-oe-source-directories)))
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
  ;; List of variables to replace in log files mapped to the method to
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

(define (find-recipe-task-files task-script-cmd? recipe task
                                break-compiler-command-lines? replace-variables?)
  (let* ((log-type (if task-script-cmd? "run" "log"))
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
      (orderedlist . ,(lambda (tag . args) `(p (ol ,args))))
      (note . ,(lambda (tag . args) `(blockquote "WARNING: " ,args)))
      (tip . ,(lambda (tag . args) `(blockquote "TIP: " ,args)))
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

(define (maybe-load-fu-oe-config-file)
  (maybe-load-conf (make-pathname *fu-oe-data-dir* "fu-oe.conf")))


;;;
;;; Buildhistory
;;;
(define (get-recipe-latest-files bh-dir)
  (fu-find-files ".*/latest$"
                 match-full-path?: #t
                 depth: 2
                 dir: (make-pathname bh-dir "packages")))

(define (get-package-latest-files bh-dir)
  (let loop ((recipe-latest-files (get-recipe-latest-files bh-dir)))
    (if (null? recipe-latest-files)
        '()
        (let* ((latest-file (car recipe-latest-files))
               (recipe-dir (pathname-directory latest-file)))
          (append
           (let loop-dirs ((dirs (filter-map
                                  (lambda (f)
                                    (let ((path (make-pathname recipe-dir f)))
                                      (and (directory? path)
                                           path)))
                                  (directory recipe-dir))))
             (if (null? dirs)
                 '()
                 (let ((latest (make-pathname (car dirs) "latest")))
                   (if (file-read-access? latest)
                       (cons latest (loop-dirs (cdr dirs)))
                       (loop-dirs (cdr dirs))))))
           (loop (cdr recipe-latest-files)))))))

(define parse-latest-file
  (let ((cache '()))
    (lambda (latest-file)
      (let ((cached (alist-ref latest-file cache equal?)))
        (cond
         (cached cached)
         (else
          (let ((data
                 (let loop ((lines (with-input-from-file
                                       latest-file
                                     read-lines)))
                   (if (null? lines)
                       '()
                       (let* ((line (car lines))
                              (tokens (string-split line "=")))
                         (if (null? tokens)
                             (loop (cdr lines))
                             (cons (cons (string->symbol
                                          (string-trim-right (car tokens)))
                                         (string-split (string-trim
                                                        (string-intersperse
                                                         (cdr tokens)
                                                         "="))))
                                   (loop (cdr lines)))))))))
            (set! cache (cons (cons latest-file data) cache))
            data)))))))

(define (latest-file->pkg/recipe latest-file)
  (pathname-strip-directory
   (pathname-directory latest-file)))

(define (bh-pkgs recipe-pattern bh-dir)
  (maybe-prompt-files
   (fu-find-files recipe-pattern
                  dir: (make-pathname bh-dir "packages")
                  depth: 1)
   recipe-pattern
   (lambda (recipe-dir)
     (for-each print
               (or (alist-ref 'PACKAGES
                              (parse-latest-file
                               (make-pathname recipe-dir "latest")))
                   '())))))

(define (bh-latest pkg-pattern bh-dir)
  (maybe-prompt-files
   (fu-find-files pkg-pattern
                  dir: (make-pathname bh-dir "packages")
                  constraint: (lambda (dir)
                                (file-exists? (make-pathname dir "latest"))))
   pkg-pattern
   (lambda (dir)
     (let ((latest (make-pathname dir "latest")))
       (print (read-all latest))
       (print-selected-file latest)))))

(define (bh-pkg-view pkg-pattern bh-dir)
  (maybe-prompt-files
   (fu-find-files pkg-pattern
                  dir: (make-pathname bh-dir "packages")
                  constraint: (lambda (dir)
                                (file-exists?
                                 (make-pathname dir "files-in-package.txt"))))
   pkg-pattern
   (lambda (dir)
     (let ((file-list (make-pathname dir "files-in-package.txt")))
       (print (read-all file-list))
       (print-selected-file file-list)))))

(define (bh-what-depends recipe bh-dir)
  (filter-map
   (lambda (latest-file)
     (let ((depends (or (alist-ref 'DEPENDS
                                   (parse-latest-file latest-file))
                        '())))
       (and (member recipe depends)
            (latest-file->pkg/recipe latest-file))))
   (get-recipe-latest-files bh-dir)))

(define (bh-show-what-depends recipe bh-dir)
  (for-each print (bh-what-depends recipe bh-dir)))

(define (bh-what-rdepends pkg bh-dir)
  (filter-map
   (lambda (latest-file)
     (let ((rdepends (or (alist-ref 'RDEPENDS
                                    (parse-latest-file latest-file))
                         '())))
       (and (member pkg rdepends)
            (latest-file->pkg/recipe latest-file))))
   (get-package-latest-files bh-dir)))

(define (bh-show-what-rdepends pkg bh-dir)
  (for-each print (bh-what-rdepends pkg bh-dir)))

(define (bh-what-rprovides pkg bh-dir)
  (for-each
   (lambda (latest-file)
     (let ((rprovides (or (alist-ref 'RPROVIDES
                                     (parse-latest-file latest-file))
                          '())))
       (when (member pkg rprovides)
         (print (pathname-strip-directory
                 (pathname-directory latest-file))))))
   (get-package-latest-files bh-dir)))

(define (bh-what-*depends-rank latest-files field)
  (let* ((cache
          (map (lambda (latest-file)
                 (cons (latest-file->pkg/recipe latest-file)
                       (or (alist-ref field
                                      (parse-latest-file latest-file))
                           '())))
               latest-files)))
    (let loop-pkgs/recipes ((pkgs/recipes (map car cache))
                    (rank '()))
      (if (null? pkgs/recipes)
          rank
          (let* ((pkg/recipe (car pkgs/recipes))
                 (num-deps
                  (let loop-cache ((cache cache)
                                   (num-deps 0))
                    (if (null? cache)
                        num-deps
                        (let ((cache-item (cdar cache)))
                          (loop-cache (cdr cache)
                                      (if (member pkg/recipe cache-item)
                                          (fx+ 1 num-deps)
                                          num-deps)))))))
            (loop-pkgs/recipes (cdr pkgs/recipes)
                       (cons (cons pkg/recipe num-deps)
                             rank)))))))

(define (bh-what-depends-rank bh-dir)
  (bh-what-*depends-rank (get-recipe-latest-files bh-dir) 'DEPENDS))

(define (bh-what-rdepends-rank bh-dir)
  (bh-what-*depends-rank (get-package-latest-files bh-dir) 'RDEPENDS))

(define (bh-rank criteria n-items bh-dir)
  (define (do-rank proc #!optional items)
    (let ((rank (sort (if items
                          (map proc items)
                          (proc))
                      (lambda (a b)
                        (> (cdr a) (cdr b))))))
      (for-each (lambda (item)
                  (printf "~a\t~a\n" (cdr item) (car item)))
                (safe-take rank n-items))))
  (case criteria
    ((depends rdepends)
     (let* ((depends? (eqv? criteria 'depends))
            (all-latest-files
             (if depends?
                 (get-recipe-latest-files bh-dir)
                 (get-package-latest-files bh-dir))))
       (do-rank (lambda (latest-file)
                  (let ((deps
                         (or (alist-ref (if depends? 'DEPENDS 'RDEPENDS)
                                        (parse-latest-file latest-file))
                             '())))
                    (cons (pathname-strip-directory
                           (pathname-directory latest-file))
                          (length deps))))
                all-latest-files)))
    ((what-depends)
     (do-rank (lambda ()
                (bh-what-depends-rank bh-dir))))
    ((what-rdepends)
     (do-rank (lambda ()
                (bh-what-rdepends-rank bh-dir))))
    (else (die! "buildhistory: invalid criteria: ~a" criteria))))

(define (handle-bh args bh-dir)
  (let ((cmd (string->symbol (car args)))
        (bh-args (cdr args)))
    (when (null? bh-args)
      (die! bh-usage))
    (if (eqv? cmd 'rank)
        (let* ((parsed-args
                (parse-command-line bh-args
                                    `((-n . n))))
               (criteria (get-opt '-- parsed-args))
               (n-items-raw (get-opt '-n parsed-args))
               (n-items (and n-items-raw
                            (string->number n-items-raw))))
          (when (and n-items
                     (not (integer? n-items))
                     (< n-items 1))
            (die! "<n> must be an integer greater than 0."))
          (if (null? (cdr criteria))
              (bh-rank (string->symbol (car criteria)) n-items bh-dir)
              (die! bh-usage)))
        (let* ((recipe/pkg (car bh-args))
               (recipe/pkg-pattern
                (prepare-pattern (car bh-args) #f))) ;; FIXME: implement -s
          (case cmd
            ((pkgs) (bh-pkgs recipe/pkg-pattern bh-dir))
            ((latest) (bh-latest recipe/pkg-pattern bh-dir))
            ((pkg-view pv) (bh-pkg-view recipe/pkg-pattern bh-dir))
            ((what-depends) (bh-show-what-depends recipe/pkg bh-dir))
            ((what-rdepends) (bh-show-what-rdepends recipe/pkg bh-dir))
            ((what-rprovides) (bh-what-rprovides recipe/pkg bh-dir))
            (else (die! bh-usage)))))))

;;;
;;; Buildstats
;;;
(define (bs-parse-duration file)
  (let loop ((lines (read-lines file)))
    (unless (null? lines)
      (let ((line (car lines)))
        (if (string-suffix? "seconds " line)
            (string->number (last (butlast (string-split line))))
            (loop (cdr lines)))))))

(define (task-start/end task-file)
  (let ((start #f)
        (end #f))
    (let loop ((lines (with-input-from-file task-file read-lines)))
      (if (or (null? lines) (and start end))
          (values start end)
          (let* ((line (car lines))
                 (tokens (string-split line)))
            (if (null? (cdr tokens))
                (loop (cdr lines))
                (let ((1st-token (car tokens)))
                  (cond ((and (not start)
                              (string=? 1st-token "Started:"))
                         (set! start (string->number (cadr tokens))))
                        ((and (not end)
                              (string=? 1st-token "Ended:"))
                         (set! end (string->number (cadr tokens)))))
                  (loop (cdr lines)))))))))

(define (bs-tasks recipe bs-dir)
  (let* ((recipe-dir
          (maybe-prompt-files
           (fu-find-files (prepare-pattern recipe #f)
                          match-full-path?: #f
                          depth: 0
                          dir: bs-dir)
           (prepare-pattern recipe #f)
           identity))
         (task-files (fu-find-files (prepare-pattern "do_[^\\.]+" #t)
                                    depth: 0
                                    dir: recipe-dir))
         (tasks-start-end
          (map (lambda (task-file)
                 (let-values (((start end) (task-start/end task-file)))
                   (list start end (pathname-strip-directory task-file))))
               task-files))
         (sorted-tasks
          (sort tasks-start-end
                (lambda (a b)
                  (< (car a) (car b))))))
    (for-each (lambda (task)
                (printf "[~a] ~a (~a seconds)\n"
                        (seconds->string (car task))
                        (caddr task)
                        (format-seconds (- (cadr task) (car task)))))
              sorted-tasks)))


(define (bs-duration recipe task bs-dir)
  (let* ((task-filename (if (string-prefix? "do_" task)
                            task
                            (string-append "do_" task)))
         (recipe-dir
          (maybe-prompt-files
           (fu-find-files (prepare-pattern recipe #f)
                          match-full-path?: #f
                          depth: 0
                          dir: bs-dir)
           (prepare-pattern recipe #f)
           identity))
         (task-file (make-pathname recipe-dir task-filename)))
    (print (bs-parse-duration task-file))))

(define (bs-rank-task-speed criteria n-items tasks bs-dir)
  (let* ((op
          (case criteria
            ((slowest-tasks) >)
            ((fastest-tasks) <)
            (else (die! "Invalid criteria: ~a" criteria))))
         (durations
          (let loop ((files (fu-find-files (prepare-pattern
                                            (if tasks
                                                (sprintf ".*/(~a)"
                                                         (string-intersperse tasks "|"))
                                                ".*/do_[^\\.]+")
                                            #t)
                                           depth: 1
                                           match-full-path?: #t
                                           dir: bs-dir)))
            (if (null? files)
                '()
                (let* ((file (car files))
                       (duration (bs-parse-duration file)))
                  (if duration
                      (cons (cons file duration)
                            (loop (cdr files)))
                      (loop (cdr files)))))))
         (sorted-durations
          (sort durations
                (lambda (a b)
                  (op (cdr a) (cdr b))))))
    (let loop ((items (safe-take sorted-durations n-items)))
      (unless (null? items)
        (let* ((item (car items))
               (file (car item))
               (duration (cdr item))
               (task (pathname-strip-directory file))
               (recipe (pathname-strip-directory (pathname-directory file))))
          (when (or (not tasks)
                    (member task tasks))
            (printf "~a\t~a\t~a\n" duration recipe task))
          (loop (cdr items)))))))

(define (bs-rank-task-frequency n-items tasks bs-dir)
  (let* ((all-tasks
          (map pathname-strip-directory
               (fu-find-files (prepare-pattern
                               (if tasks
                                   (sprintf ".*/(~a)"
                                            (string-intersperse tasks "|"))
                                   ".*/do_[^\\.]+")
                               #t)
                              depth: 1
                              match-full-path?: #t
                              dir: bs-dir)))
         (uniq-tasks (delete-duplicates all-tasks equal?))
         (rank (let loop ((tasks uniq-tasks))
                 (if (null? tasks)
                     '()
                     (let ((task (car tasks)))
                       (cons (cons task (count (lambda (t)
                                                 (string=? t task))
                                               all-tasks))
                             (loop (cdr tasks))))))))
    (for-each (lambda (task/count)
                (printf "~a\t~a\n" (cdr task/count) (car task/count)))
              (sort (safe-take rank n-items)
                    (lambda (a b)
                      (> (cdr a) (cdr b)))))))

(define (bs-rank criteria n-items tasks bs-dir)
  (case criteria
    ((task-frequency)
     (bs-rank-task-frequency n-items tasks bs-dir))
    ((slowest-tasks fastest-tasks)
     (bs-rank-task-speed criteria n-items tasks bs-dir))
    (else (die! "buildstats rank: invalid criteria: ~a" criteria))))

(define (handle-bs args bs-dir* bs-base?)
  (let ((cmd (string->symbol (car args)))
        (bs-args (cdr args))
        (bs-dir (if bs-base?
                    (maybe-prompt-files
                     (filter directory? (glob (make-pathname bs-dir* "*")))
                     (prepare-pattern "[0-9]+" #f)
                     identity
                     quiet?: #t)
                    bs-dir*)))
    (when (null? bs-args)
      (die! bs-usage))
    (let ((recipe (car bs-args)))
      (case cmd
        ((tasks) (bs-tasks recipe bs-dir))
        (else
         (case cmd
           ((duration)
            (when (null? (cdr bs-args))
              (die! bs-usage))
            (let ((task (cadr bs-args)))
              (bs-duration recipe task bs-dir)))
           ((rank)
            (let* ((parsed-args
                    (parse-command-line bs-args
                                        `((-n . n)
                                          (-t . tags))))
                   (criteria (get-opt '-- parsed-args))
                   (n-items-raw (get-opt '-n parsed-args))
                   (n-items (and n-items-raw
                                 (string->number n-items-raw)))
                   (tasks (get-opt '-t parsed-args 'multiple)))
              (when (and n-items
                         (or (not (integer? n-items))
                             (< n-items 1)))
                (die! "<n> must be an integer greater than 0."))
              (if (null? (cdr criteria))
                  (bs-rank (string->symbol (car criteria))
                           n-items
                           (if (null? tasks) #f tasks)
                           bs-dir)
                  (die! bs-usage))))
           (else (die! bs-usage))))))))

(define bh-usage
  "buildhistory <subcommand> <recipe|package>
  Short command: bh.  Query the buildhistory directory.

  The BUILDHISTORY_DIR environment variable can be used to indicate
  the path to the buildhistory directory.  If it is not set, oe will use
  BUILDHISTORY_DIR from the BitBake environment.

  Subcommands:

  pkgs <recipe pattern>
    List the packages generated by recipes matching <recipe pattern>.

  latest <pkg pattern>
    Show the content of the `latest' file for packages matching
    <pkg pattern>.

  pkg-view <pkg pattern>
    Short command: pv.  Equivalent to `oe pv <pkg>', but using information
    from buildhistory instead.

  what-depends <recipe>
    Show recipes that depend on recipe <recipe>.

  what-rdepends <pkg>
    Show packages that have runtime dependency on <pkg>.

  what-rprovides <provider>
    Show packages that provide <provider> in run time.

  rank <criteria> [-n <n>]
    Rank recipes/packages according to <criteria>.
      <criteria>: One of depends, rdepends, what-depends, what-rdepends.
      -n <n>:     Number of items to show. (if omitted, all items will be
                  displayed).
")

(define bs-usage
  "buildstats <subcommand> <recipe> <task>
  Short command: bs.  Query the buildstats directory.

  The BUILDSTATS_DIR environment variable can be used to indicate the
  path to the buildstatis directory.  If it is not set, oe will use
  BUILDSTATS_BASE from the BitBake environment and will prompt you for
  the specific buildstats directory in case more than one exists.

  Subcommands:

  duration <recipe> <task>
    Show the duration of task <task> for recipe <recipe>.

  tasks <recipe>
    Show tasks executed for recipe <recipe>.

  rank <criteria> [-n <n>] [-t <task>]
    Rank tasks according to <criteria>.
      <criteria>: possible values: task-frequency, slowest-tasks, fastest-tasks
      -n <n>:     show <n> top items (if omitted, all items will be displayed).
      -t <task>:  specify task filters.  Can be provided multiple times.
")

(define oe-usage
  (string-append
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

task-log [-f] [-r] <recipe> [<task>]
  Short command: tl.  Show log file for <task> executed for <recipe>.  If
  <task> is not provided, all log files (log.do_<task>) will be displayed
  as options for selection.
  -f: format command lines.  Will break compiler command line arguments into
      individual lines, hopefully improving readability.
  -r: replace variable values.  Will replace some values by their
      corresponding variable, hopefully improving readability.

task-script <recipe> [<task>]
  Short command: ts.  Show run.do_<task> scripts for <task> executed for
  <recipe>.  If <task> is not provided, all script files will be displayed
  as options for selection.

grep [<grep options>] <pattern>
  Short command: g. Search for <pattern> in the source directories.

grep-view [<grep options>] <pattern>
  Short command: gv.  Search for <pattern> in the source directories and
  call the viewer on the selected option.

grep-edit [<grep options>] <pattern>
  Short command: ge.  Search for <pattern> in the source directories and
  call the editor on the selected option.

variable-find <pattern> [<recipe>]
  Short command: vf.  Find variables matching <pattern>.
"
   "\n"
   bh-usage
   "\n"
   bs-usage))


(define-command 'oe
  "oe <options>
   Run 'oe help' for the full help message."
  (lambda args
    (when (null? args)
      (die! oe-usage))

    (when (member (car args) '("help" "h"))
      (print oe-usage)
      (exit))

    (let* ((builddir (get-environment-variable "BUILDDIR"))
           (cmd (string->symbol (car args)))
           (oe-args (cdr args))
           (builddir-required?
            (not (memq cmd '(buildhistory bh buildstats bs)))))

      (cond ((and (not builddir) builddir-required?)
             (die! "The build environment is not set.  Aborting."))
            ((or builddir builddir-required?)
             (set! *build-dir* builddir)
             (set! *local-conf-file*
                   (make-pathname (list *build-dir* "conf") "local.conf"))
             (set! *fu-oe-data-dir* (make-pathname (list builddir ".fu-oe")
                                                   *cache-version*))
             (set! *cached-oe-data-file* (make-pathname *fu-oe-data-dir*
                                                        "cached-variables.scm"))
             (set! *documentation-cache-file*
                   (make-pathname *fu-oe-data-dir* "cached-documentation.scm"))

             (create-directory *fu-oe-data-dir* 'recursively)
             (maybe-load-fu-oe-config-file)
             (maybe-store-basic-oe-data!)))

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

        ((task-log task-script tl ts)
         (let ((task-script-cmd? (memq cmd '(task-script ts))))
           (if (null? oe-args)
               (die! "Missing recipe.  Aborting.")
               (let* ((non-options (remove (lambda (arg)
                                             (string-prefix? "-" arg))
                                           oe-args))
                      (recipe (string->symbol (car non-options)))
                      (task (and (not (null? (cdr non-options)))
                                 (cadr non-options)))
                      (break-compiler-command-lines?
                       (and (not task-script-cmd?)
                            (member "-f" oe-args)))
                      (replace-variables? (and (not task-script-cmd?)
                                               (member "-r" oe-args))))
                 (populate-bitbake-data! recipe)
                 (find-recipe-task-files task-script-cmd?
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

        ((buildhistory bh)
         (let ((bh-dir (get-environment-variable "BUILDHISTORY_DIR")))
           (when (and builddir (not bh-dir))
             (populate-bitbake-data-from-cache!))
           (when (null? oe-args)
             (die! bh-usage))
           (when (and (not builddir) (not bh-dir))
             (die! "BUILDDIR and/or BUILDHISTORY_DIR must be set."))
           (handle-bh oe-args (or bh-dir (get-var 'BUILDHISTORY_DIR)))))

        ((buildstats bs)
         (let ((bs-dir (get-environment-variable "BUILDSTATS_DIR")))
           (when (and builddir (not bs-dir))
             (populate-bitbake-data-from-cache!))
           (when (null? oe-args)
             (die! bs-usage))
           (when (and (not builddir) (not bs-dir))
             (die! "BUILDDIR and/or BUILDSTATS_DIR must be set."))
           (let ((bs-base (and builddir
                               (get-var 'BUILDSTATS_BASE))))
             (handle-bs oe-args (or bs-dir bs-base) (not bs-dir)))))

        (else (die! "Unknown command: ~a" cmd))))))
