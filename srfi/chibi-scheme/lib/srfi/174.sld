(define-library (srfi 174)
  (import
   (scheme base)
   (only (srfi 128) make-comparator))
  (export
   make-timespec timespec? timespec-seconds timespec-nanoseconds)
  (begin

    (define-record-type timespec
        (make-timespec seconds nanoseconds)
        timespec?
      (seconds timespec-seconds)
      (nanoseconds timespec-nanoseconds))



    ))
