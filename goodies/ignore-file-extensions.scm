(cond-expand
 (chicken-4
  (void))
 (chicken-5
  (import (chicken pathname)))
 (else
  (error "Unsupported CHICKEN version.")))

(define (ignore-file-extensions ignored-extensions)
  (constraints
   (conjoin (constraints)
            (lambda (path)
              (let ((ext (pathname-extension path)))
                (not (member ext ignored-extensions)))))))
