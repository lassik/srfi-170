;; please see copyright notice in 170/COPYING

;; Interim implementation as of June 2020.

(define-library (srfi 198)
  (export

   ;; 3.1  Errors

#| useful for debuging

   errno/E2BIG errno/EACCES errno/EADDRINUSE errno/EADDRNOTAVAIL
   errno/EAFNOSUPPORT errno/EAGAIN errno/EALREADY errno/EBADF
   errno/EBADMSG errno/EBUSY errno/ECANCELED errno/ECHILD
   errno/ECONNABORTED errno/ECONNREFUSED errno/ECONNRESET
   errno/EDEADLK errno/EDESTADDRREQ errno/EDOM errno/EDQUOT
   errno/EEXIST errno/EFAULT errno/EFBIG errno/EHOSTUNREACH
   errno/EIDRM errno/EILSEQ errno/EINPROGRESS errno/EINTR errno/EINVAL
   errno/EIO errno/EISCONN errno/EISDIR errno/ELOOP errno/EMFILE
   errno/EMLINK errno/EMSGSIZE errno/ENAMETOOLONG errno/ENETDOWN
   errno/ENETRESET errno/ENETUNREACH errno/ENFILE errno/ENOBUFS
   errno/ENODEV errno/ENOENT errno/ENOEXEC errno/ENOLCK errno/ENOMEM
   errno/ENOMSG errno/ENOPROTOOPT errno/ENOSPC errno/ENOSYS
   errno/ENOTCONN errno/ENOTDIR errno/ENOTEMPTY errno/ENOTRECOVERABLE
   errno/ENOTSOCK errno/ENOTSUP errno/ENOTTY errno/ENXIO
   errno/EOPNOTSUPP errno/EOVERFLOW errno/EOWNERDEAD errno/EPERM
   errno/EPIPE errno/EPROTO errno/EPROTONOSUPPORT errno/EPROTOTYPE
   errno/ERANGE errno/EROFS errno/ESPIPE errno/ESRCH errno/ESTALE
   errno/ETIMEDOUT errno/ETXTBSY errno/EWOULDBLOCK errno/EXDEV
|#

   make-syscall-error syscall-error?
   syscall-error:errno syscall-error:message
   syscall-error:procedure-name syscall-error:data
   errno-error
   )

  (cond-expand ((not bsd)
    (export

#|
     ;; 3.1  Errors

     errno/EMULTIHOP errno/ENOLINK
     ;; STREAMS:
     errno/ENODATA errno/ENOSTR errno/ENOSR errno/ETIME
|#
    )))

  (cond-expand
   (chibi
    (import
     (scheme base)

     (only (srfi 69) make-hash-table hash-table-set! hash-table-ref)
     )

    (include-shared "170/170")
    (include-shared "170/aux")))

  (begin

    (define errno-map (make-hash-table))

    (map (lambda (errno-number errno-string) (hash-table-set! errno-map errno-number errno-string))
         (list
          errno/E2BIG errno/EACCES errno/EADDRINUSE errno/EADDRNOTAVAIL
          errno/EAFNOSUPPORT errno/EAGAIN errno/EALREADY errno/EBADF
          errno/EBADMSG errno/EBUSY errno/ECANCELED errno/ECHILD
          errno/ECONNABORTED errno/ECONNREFUSED errno/ECONNRESET
          errno/EDEADLK errno/EDESTADDRREQ errno/EDOM errno/EDQUOT
          errno/EEXIST errno/EFAULT errno/EFBIG errno/EHOSTUNREACH
          errno/EIDRM errno/EILSEQ errno/EINPROGRESS errno/EINTR errno/EINVAL
          errno/EIO errno/EISCONN errno/EISDIR errno/ELOOP errno/EMFILE
          errno/EMLINK errno/EMSGSIZE errno/ENAMETOOLONG errno/ENETDOWN
          errno/ENETRESET errno/ENETUNREACH errno/ENFILE errno/ENOBUFS
          errno/ENODEV errno/ENOENT errno/ENOEXEC errno/ENOLCK errno/ENOMEM
          errno/ENOMSG errno/ENOPROTOOPT errno/ENOSPC errno/ENOSYS
          errno/ENOTCONN errno/ENOTDIR errno/ENOTEMPTY errno/ENOTRECOVERABLE
          errno/ENOTSOCK errno/ENOTSUP errno/ENOTTY errno/ENXIO
          errno/EOPNOTSUPP errno/EOVERFLOW errno/EOWNERDEAD errno/EPERM
          errno/EPIPE errno/EPROTO errno/EPROTONOSUPPORT errno/EPROTOTYPE
          errno/ERANGE errno/EROFS errno/ESPIPE errno/ESRCH errno/ESTALE
          errno/ETIMEDOUT errno/ETXTBSY errno/EWOULDBLOCK errno/EXDEV

          (cond-expand ((not bsd)
                        errno/EMULTIHOP errno/ENOLINK
                        ;; STREAMS:
                        errno/ENODATA errno/ENOSTR errno/ENOSR errno/ETIME)))

         (list
          "errno/E2BIG" "errno/EACCES" "errno/EADDRINUSE" "errno/EADDRNOTAVAIL"
          "errno/EAFNOSUPPORT" "errno/EAGAIN" "errno/EALREADY" "errno/EBADF"
          "errno/EBADMSG" "errno/EBUSY" "errno/ECANCELED" "errno/ECHILD"
          "errno/ECONNABORTED" "errno/ECONNREFUSED" "errno/ECONNRESET"
          "errno/EDEADLK" "errno/EDESTADDRREQ" "errno/EDOM" "errno/EDQUOT"
          "errno/EEXIST" "errno/EFAULT" "errno/EFBIG" "errno/EHOSTUNREACH"
          "errno/EIDRM" "errno/EILSEQ" "errno/EINPROGRESS" "errno/EINTR" "errno/EINVAL"
          "errno/EIO" "errno/EISCONN" "errno/EISDIR" "errno/ELOOP" "errno/EMFILE"
          "errno/EMLINK" "errno/EMSGSIZE" "errno/ENAMETOOLONG" "errno/ENETDOWN"
          "errno/ENETRESET" "errno/ENETUNREACH" "errno/ENFILE" "errno/ENOBUFS"
          "errno/ENODEV" "errno/ENOENT" "errno/ENOEXEC" "errno/ENOLCK" "errno/ENOMEM"
          "errno/ENOMSG" "errno/ENOPROTOOPT" "errno/ENOSPC" "errno/ENOSYS"
          "errno/ENOTCONN" "errno/ENOTDIR" "errno/ENOTEMPTY" "errno/ENOTRECOVERABLE"
          "errno/ENOTSOCK" "errno/ENOTSUP" "errno/ENOTTY" "errno/ENXIO"
          "errno/EOPNOTSUPP" "errno/EOVERFLOW" "errno/EOWNERDEAD" "errno/EPERM"
          "errno/EPIPE" "errno/EPROTO" "errno/EPROTONOSUPPORT" "errno/EPROTOTYPE"
          "errno/ERANGE" "errno/EROFS" "errno/ESPIPE" "errno/ESRCH" "errno/ESTALE"
          "errno/ETIMEDOUT" "errno/ETXTBSY" "errno/EWOULDBLOCK" "errno/EXDEV"

          (cond-expand ((not bsd)
                        "errno/EMULTIHOP" "errno/ENOLINK"
                        ;; STREAMS:
                        "errno/ENODATA" "errno/ENOSTR" "errno/ENOSR" "errno/ETIME"))
          ))

    (define-record-type syscall-error
      (make-syscall-error errno message procedure-name data)
      syscall-error?
      (errno syscall-error:errno)
      (message syscall-error:message)
      (procedure-name syscall-error:procedure-name)
      (data syscall-error:data))

    (define (errno-error errno procedure-name system-call-name . data)
      (raise (make-syscall-error
              errno
              (string-append (symbol->string procedure-name) ": called " system-call-name ": " (hash-table-ref errno-map errno) ": " (integer->error-string errno))
              procedure-name
              data))))
  )
