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

(define (raise-unexpected-retval c-function)
  (error "Unexpected return value" c-function))

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
              (types  (reverse types))
              (retval (gensym 'retval)))
          `(let ((,retval ((c-lambda ,types ,(resolve-typedef rettype)
                             ,c-function)
                           ,@values)))
             (cond ((>= ,retval 0)
                    ,retval)
                   ((= ,retval -1)
                    (raise (make-errno-error ,c-function (%errno))))
                   (else
                    (raise-unexpected-retval ,c-function)))))
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
