(define (%find-files dir pred action id limit follow dot loc)
  (##sys#check-string dir loc)
  (let* ((depth 0)
         (lproc
          (cond ((not limit) (lambda _ #t))
                ((fixnum? limit) (lambda _ (fx< depth limit)))
                (else limit) ) )
         (pproc
          (if (procedure? pred)
              pred
              (let ((pred (irregex pred))) ; force compilation
                (lambda (x) (irregex-match pred x))))))
    (let loop ((dir dir)
               (fs (directory dir dot))
               (r id))
      (if (null? fs)
          r
          (let* ((filename (##sys#slot fs 0))
                 (f (make-pathname dir filename))
                 (rest (##sys#slot fs 1)))
            (cond ((and (directory? f)
                        ;; Don't error out at unreadable directories
                        (file-read-access? f))
                   (cond ((member filename '("." "..")) (loop dir rest r))
                         ((and (symbolic-link? f) (not follow))
                          (loop dir rest (if (pproc f) (action f r) r)))
                         ((lproc f)
                          (loop dir
                                rest
                                (fluid-let ((depth (fx+ depth 1)))
                                  (loop f
                                        (directory f dot)
                                        (if (pproc f) (action f r) r)))))
                         (else (loop dir rest (if (pproc f) (action f r) r)))))
                  ((pproc f) (loop dir rest (action f r)))
                  (else (loop dir rest r))))))))

(define (find-files dir #!key (test (lambda _ #t))
                              (action (lambda (x y) (cons x y)))
                              (seed '())
                              (limit #f)
                              (dotfiles #f)
                              (follow-symlinks #f))
  (%find-files dir test action seed limit follow-symlinks dotfiles 'find-files))
