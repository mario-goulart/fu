(constraints
 (conjoin (constraints)
          (lambda (path)
            (not (cond ((substring-index "./.git/" path)
                        => (lambda (pos)
                             (zero? pos)))
                       (else #f))))))
