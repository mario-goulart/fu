(cond-expand
 (chicken-4
  (void))
 (chicken-5
  (import (chicken string)))
 (else
  (error "Unsupported CHICKEN version.")))

(constraints
 (conjoin (constraints)
          (lambda (path)
            (not (cond ((substring-index "./.git/" path)
                        => (lambda (pos)
                             (zero? pos)))
                       (else #f))))))
