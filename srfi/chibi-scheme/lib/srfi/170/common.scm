;; please see copyright notice in ./COPYING

;; Common code that's included by both 170.sld and test.sld

(define-record-type SRFI-170-Error
    (make-srfi-170-error message procedure-name data)
    srfi-170-error?
  (message srfi-170-error:message)
  (procedure-name srfi-170-error:procedure-name)
  (data srfi-170-error:data))

(define (srfi-170-error message procedure-name . data)
  (raise (make-srfi-170-error
           (string-append (symbol->string procedure-name) ": " message)
           procedure-name
           data)))


(define (retry-if-EINTR the-lambda)
  (let loop ((ret (the-lambda)))
    (if ret
        ret
        (if (equal? errno/EINTR (errno))
            (loop (the-lambda))
            ret))))


;; deletes flles and directories and does not raise an exception if
;; the fname doesn't exist.  Unlike the scsh version, will raise an
;; exception if an object can't be deleted.

;; ~~~ all these errors are obscure because this is not an SRFI defined procedure

(define (delete-filesystem-object fname)
  (if (not (string? fname))
        (srfi-170-error "fname must be a string" 'delete-filesystem-object fname))
  (if (file-exists? fname)
      (if (file-info-directory? (file-info fname #f))
          (if (not (delete-directory fname))
              (errno-error (errno) 'delete-filesystem-object 'rmdir fname))
          (if (not (delete-file fname))
              (errno-error (errno) 'delete-filesystem-object 'unlink fname)))))

;; Needs to be in common for testing since we can't create or modify actual accounts

(define (parse-gecos gecos user-name)
  (let ((the-gecos-list (regexp-split "," gecos)))
    (if (> (string-length user-name) 0)
        (let ((capitalized-user-name (string-copy user-name))) ;; we are NOT being functional
          (string-set! capitalized-user-name 0 (char-upcase (string-ref capitalized-user-name 0)))
          (set-car! the-gecos-list (regexp-replace-all "@" (car the-gecos-list) capitalized-user-name))))
    the-gecos-list))
