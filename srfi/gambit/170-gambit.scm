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

(define (make-errno-error errno-value c-function)
  (make-foreign-status
   'foreign-interface  c-function
   'errno              errno-value
   'message            (%strerror errno-value)))

;; Return value is of type int, 0 means success, -1 means error, other
;; values shouldn't happen. Upon -1 return, errno has the error code.
(define-macro (call/errno-int c-function . args)
  (define (resolve-typedef type)
    (case type
      ((mode_t) 'unsigned-long)
      (else type)))
  (let loop ((values '()) (types '()) (args args))
    (if (null? args)
        (let ((values (reverse values))
              (types  (reverse types))
              (rv     (gensym 'rv)))
          `(let ((,rv ((c-lambda ,types int ,c-function) ,@values)))
             (case ,rv
               ((0) #f)
               ((-1) (raise (make-errno-error (%errno) ,c-function)))
               (else (error "Unexpected return value" ,c-function ,rv)))))
        (let ((value (car args))
              (type (resolve-typedef (cadr args)))
              (args (cddr args)))
          (loop (cons value values)
                (cons type  types)
                args)))))

(define (open-file fname flags #!optional permission-bits)
  (call/errno-int "open"
                  fname            nonnull-char-string
                  flags            int
                  permission-bits  mode_t))

(define (close-fdes fd)
  (call/errno-int "close"
                  fd int))

(define (create-directory fname #!optional permission-bits)
  (call/errno-int "mkdir"
                  fname            nonnull-char-string
                  permission-bits  mode_t))
