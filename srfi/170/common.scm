;; please see copyright notice in ./COPYING

;; Common code that's included by both 170.sld and test.sld

;; deletes flles and directories and does not raise an exception if
;; the fname doesn't exist.  Unlike the scsh version, will raise an
;; exception if an object can't be deleted.

(define (delete-filesystem-object fname)
  (if (file-exists? fname)
      (if (file-info-directory? (file-info fname #f))
          (if (not (delete-directory fname))
              (errno-error (errno) delete-filesystem-object fname))
          (if (not (delete-file fname))
              (errno-error (errno) delete-filesystem-object fname)))))

;;> The fundamental directory iterator.  Applies \var{kons} to
;;> each filename in directory \var{dir} and the result of the
;;> previous application, beginning with \var{knil}.  With
;;> \var{kons} as \scheme{cons} and \var{knil} as \scheme{'()},
;;> equivalent to \scheme{directory-files}.

(define (directory-fold dir kons knil . o)
  (let-optionals o ((dot-files? #f))
    (let ((do (open-directory dir dot-files?)))
      (let lp ((res knil))
        (let ((file (read-directory do)))
          (if (not (eof-object? file))
              (lp (kons file res))
              (begin (close-directory do) res)))))))

;; Needs to be in common for testing since we can't create or modify actual accounts

(define (parse-gecos gecos user-name)
  (let ((the-gecos-list (regexp-split "," gecos)))
    (if (> (string-length user-name) 0)
        (let ((capitalized-user-name (string-copy user-name))) ;; we are NOT being functional
          (string-set! capitalized-user-name 0 (char-upcase (string-ref capitalized-user-name 0)))
          (set-car! the-gecos-list (regexp-replace-all "@" (car the-gecos-list) capitalized-user-name))))
    the-gecos-list))
