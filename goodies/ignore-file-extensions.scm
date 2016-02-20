(define (ignore-file-extensions ignored-extensions)
  (constraints
   (conjoin (constraints)
            (lambda (path)
              (let ((ext (pathname-extension path)))
                (member ext ignored-extensions))))))
