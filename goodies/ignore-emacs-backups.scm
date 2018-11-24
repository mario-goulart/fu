(cond-expand
 (chicken-4
  (void))
 (chicken-5
  (import srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))

(constraints
 (conjoin (constraints)
          (lambda (path)
            (not (or (string-prefix? "#" path)
                     (string-suffix? "~" path))))))
