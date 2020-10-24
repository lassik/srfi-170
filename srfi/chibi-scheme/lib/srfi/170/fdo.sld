;; please see copyright notice in 198/COPYING

;; File Descriptor Objects

;; Must be a library so test.sld has the same type of record to check
;; against, wants to be a seperate library so other libraries like
;; SRFI 205 can use FDO.

(define-library (srfi 170 fdo)
  (export fdo?
          make-fdo
          fdo:fd
          )

  (import
   (scheme base)

   )

  (include "fdo.scm")
  )
