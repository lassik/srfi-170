(define-library (srfi 174)
  (import
   (scheme base)
   (only (srfi 128) make-comparator))
  (export
   timespec timespec? timespec-seconds timespec-nanoseconds)
  (begin

    (define-record-type Timespec
        (timespec seconds nanoseconds)
        timespec?
      (seconds timespec-seconds)
      (nanoseconds timespec-nanoseconds))



    ))
