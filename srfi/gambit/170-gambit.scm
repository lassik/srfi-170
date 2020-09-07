(c-declare "#include <sys/stat.h>")

(c-declare "#include <errno.h>")
(c-declare "#include <fcntl.h>")
(c-declare "#include <string.h>")
(c-declare "#include <unistd.h>")

(define %errno (c-lambda () int "___return(errno);"))
(define %strerror (c-lambda (int) char-string "strerror"))

(define group/unchanged #f)
(define owner/unchanged #f)

(define time/now       'time/now)
(define time/unchanged 'time/unchanged)

(define AT_FDCWD
  ((c-lambda () int   "___return(AT_FDCWD);")))

(define gid_t-1
  ((c-lambda () unsigned-long "___return((unsigned long)(gid_t)-1);")))

(define uid_t-1
  ((c-lambda () unsigned-long "___return((unsigned long)(uid_t)-1);")))

(define UTIME_NOW
  ((c-lambda () long "___return((long)UTIME_NOW);")))

(define UTIME_OMIT
  ((c-lambda () long "___return((long)UTIME_OMIT);")))

(define-record-type <foreign-status>
  (%make-foreign-status plist)
  foreign-status?
  (plist %foreign-status-plist))

(define-record-type <time-type>
  (make-time type nanosecond second)
  srfi-19-time?
  (type       time-type)
  (nanosecond time-nanosecond)
  (second     time-second))

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
      ((time_t) 'unsigned-long)
      ((mode_t) 'unsigned-long)
      ((uid_t)  'unsigned-long)
      ((gid_t)  'unsigned-long)
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

(define (rename-file old-fname new-fname)
  (call/classic "rename"
                old-fname nonnull-char-string
                new-fname nonnull-char-string))

(define (delete-directory fname)
  (call/classic "rmdir"
                fname nonnull-char-string))

(define (set-file-mode fname mode-bits)
  (call/classic "chmod"
                fname      nonnull-char-string
                mode-bits  mode_t))

(define (set-file-owner fname uid gid)
  (let ((uid (if (eqv? uid owner/unchanged) uid_t-1 uid))
        (gid (if (eqv? gid group/unchanged) gid_t-1 gid)))
    (call/classic "chown"
                  fname  nonnull-char-string
                  uid    uid_t
                  gid    gid_t)))

(define (set-file-times fname
                        #!optional
                        access-time-object
                        modify-time-object)
  (define (get-sec time)
    (cond ((equal? time time/now)       0)
          ((equal? time time/unchanged) 0)
          (else                         (time-second time))))
  (define (get-nsec time)
    (cond ((equal? time time/now)       UTIME_NOW)
          ((equal? time time/unchanged) UTIME_OMIT)
          (else                         (time-nanosecond time))))
  (handle/errno-integral
   "utimensat"
   ((c-lambda (nonnull-char-string
               long
               long
               long
               long)
        int
      "struct timespec tv[2];
       tv[0].tv_sec = ___arg2;
       tv[1].tv_sec = ___arg3;
       tv[0].tv_nsec = ___arg4;
       tv[1].tv_nsec = ___arg5;
       ___return(utimensat(AT_FDCWD, ___arg1, tv, 0));")
    fname
    (get-sec access-time-object)
    (get-sec modify-time-object)
    (get-nsec access-time-object)
    (get-nsec modify-time-object))))

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
