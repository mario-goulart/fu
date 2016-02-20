(define (ignore-file-extensions ignored-extensions)
  (constraints
   (conjoin (constraints)
            (lambda (path)
              (let ((ext (pathname-extension path)))
                (not (member ext ignored-extensions)))))))
