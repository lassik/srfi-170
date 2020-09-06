;; please see copyright notice in ./COPYING

(define-record-type Foreign-Interface-Error
    (make-foreign-interface-error-record error-set code scheme-procedure foreign-interface message data)
    foreign-error?
  (error-set foreign-error:error-set)
  (code foreign-error:code)
  (scheme-procedure foreign-error:scheme-procedure)
  (foreign-interface foreign-error:foreign-interface)
  (message foreign-error:message)
  (data foreign-error:data))


(define (make-malformed-foreign-error-alist original-argument the-message)
  (alist-cons 'message
              the-message
              (alist-cons 'data
                          (alist-cons 'arguments original-argument '())
                          '((error-set . error)
                            (scheme-procedure . make-foreign-error)))))

(define (alist-or-#f the-key the-alist)
  (let ((the-sub-alist (assq the-key the-alist)))
    (if the-sub-alist
        (cdr the-sub-alist)
        the-sub-alist)))

(define (make-foreign-error the-alist)
  (cond ((not (pair? the-alist))
         (make-foreign-error (make-malformed-foreign-error-alist the-alist "Malformed call to make-foreign-error, not a list; see data for details")))
        ((not (pair? (car the-alist)))
         (make-foreign-error (make-malformed-foreign-error-alist the-alist "Malformed call to make-foreign-error, not an alist; see data for details")))
        ((not (symbol? (caar the-alist)))
         (make-foreign-error (make-malformed-foreign-error-alist the-alist "Malformed call to make-foreign-error, first key must be a symbol; see data for details")))
        (else (let ((the-error-set (assq 'error-set the-alist)))
                (if (not the-error-set)
                    (make-foreign-error (make-malformed-foreign-error-alist the-alist "Malformed call to make-foreign-error, missing error-set; see data for details"))
                    (make-foreign-interface-error-record (cdr the-error-set)
                                                         (alist-or-#f 'code the-alist)
                                                         (alist-or-#f 'scheme-procedure the-alist)
                                                         (alist-or-#f 'foreign-interface the-alist)
                                                         (alist-or-#f 'message the-alist)
                                                         (alist-or-#f 'data the-alist)))))))

(define (raise-foreign-error the-alist . o)
  (let-optionals o ((continuable #f))
    (if continuable
        (raise-continuable (make-foreign-error the-alist))
        (raise (make-foreign-error the-alist)))))
