(constraints
 (conjoin (constraints)
          (lambda (path)
            (not (or (string-prefix? "#" path)
                     (string-suffix? "~" path))))))
