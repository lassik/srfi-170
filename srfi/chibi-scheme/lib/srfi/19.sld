;; please see copyright notice in ./COPYING

;; Just the minimum needed for SRFI 170; will be replaced in due
;; course with an improved SRFI 174.

(define-library (srfi 19)
  (import
   (scheme base)
   )
  (export
   time-monotonic time-utc
   make-time time? time-type time-second time-nanosecond)

  (include-shared "170/170") ;; to pick up definitions of time-monotonic and time-utc

  (begin

    (define-record-type Time
        (make-time type seconds nanoseconds)
        time?
      (type time-type)
      (seconds time-second)
      (nanoseconds time-nanosecond))



    ))
