;; please see copyright notice in 198/COPYING

;; Must be a library so test.sld has the same type of record to check
;; against.

(define-library (srfi 170 posix-error)
  (export

   posix-error?

   posix-error-error-set
   posix-error-number
   posix-error-name
   posix-error-scheme-procedure
   posix-error-posix-interface
   posix-error-message
   posix-error-data

   make-posix-error
   raise-posix-error
   )

  (import
   (scheme base)

   (only (srfi 1) alist-cons)
   )

  (include "posix-error.scm")
  )
