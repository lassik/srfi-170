(c-declare "#include <sys/stat.h>")

(c-declare "#include <errno.h>")
(c-declare "#include <fcntl.h>")
(c-declare "#include <string.h>")
(c-declare "#include <unistd.h>")

(define %errno (c-lambda () int "___return(errno);"))
(define %strerror (c-lambda (int) char-string "strerror"))

(define-record-type <foreign-status>
  (%make-foreign-status plist)
  foreign-status?
  (plist %foreign-status-plist))

(define (make-foreign-status . plist)
  (%make-foreign-status plist))

(define (make-errno-error c-function errno-value)
  (make-foreign-status
   'foreign-interface  c-function
   'errno              errno-value
   'message            (%strerror errno-value)))

(define sane-length-limit 65536)

(define (null-terminate! bytes)
  (let ((n (u8vector-length bytes)))
    (let loop ((i 0))
      (if (and (< i n) (not (= 0 (u8vector-ref bytes i))))
          (loop (+ i 1))
          (begin (u8vector-shrink! bytes i)
                 bytes)))))

(define (raise-unexpected-retval c-function)
  (error "Unexpected return value" c-function))

(define (handle/errno-integral c-function retval)
  (cond ((>= retval 0) retval)
        ((= retval -1) (raise (make-errno-error c-function (%errno))))
        (else          (raise-unexpected-retval c-function))))

;; Return value is of the given signed integer type. Non-negative
;; means success, -1 means error, other negative values shouldn't
;; happen. Upon -1 return, errno has the error code.
(define-macro (call/errno-integral rettype c-function . args)
  (define (resolve-typedef type)
    (case type
      ((mode_t) 'unsigned-long)
      (else type)))
  (let loop ((values '()) (types '()) (args args))
    (if (null? args)
        (let ((values (reverse values))
              (types  (reverse types)))
          `(handle/errno-integral
            ((c-lambda ,types ,(resolve-typedef rettype) ,c-function)
             ,@values)))
        (let ((value (car args))
              (type (resolve-typedef (cadr args)))
              (args (cddr args)))
          (loop (cons value values)
                (cons type  types)
                args)))))

(define-macro (call/classic c-function . args)
  `(if (= 0 (call/errno-integral int ,c-function ,@args))
       #f
       (raise-unexpected-retval ,c-function)))

(define (open-file fname flags #!optional permission-bits)
  (call/classic "open"
                fname            nonnull-char-string
                flags            int
                permission-bits  mode_t))

(define (close-fdes fd)
  (call/classic "close"
                fd int))

(define (create-directory fname #!optional permission-bits)
  (call/classic "mkdir"
                fname            nonnull-char-string
                permission-bits  mode_t))

;; - POSIX says readlink() may or may not null-terminate the result.
;; - A truncated result doesn't result in a -1 return value.
(define (read-symlink fname)
  (let loop ((limit 64))
    (if (>= limit sane-length-limit)
        (error "Too long")
        (let* ((bytes  (make-bytevector limit 0))
               (length (handle/errno-integral
                        "readlink"
                        ((c-lambda (nonnull-char-string scheme-object size_t)
                             int
                           "___U8 *bytes = ___CAST(___U8 *, ___BODY(___arg2));
                            ___return(readlink(___arg1, bytes, ___arg3));")
                         fname bytes limit))))
          (if (< length (- limit 1))
              (utf8->string (null-terminate! bytes))
              (loop (* limit 2)))))))
