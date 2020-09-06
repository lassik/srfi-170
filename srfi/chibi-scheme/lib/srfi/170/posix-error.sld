;; please see copyright notice in 198/COPYING

(define-library (srfi 198)
  (export

   foreign-error?

   foreign-error:error-set foreign-error:code
   foreign-error:scheme-procedure
   foreign-error:foreign-interface
   foreign-error:message foreign-error:data

   make-foreign-error
   raise-foreign-error
   )

  (import
   (scheme base)

   (chibi optional) ;; Snow package for optional args

   (only (srfi 1) alist-cons)
   )

  (include "198/198.scm")
  )
