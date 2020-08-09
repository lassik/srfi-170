;; please see copyright notice in 170/COPYING

(define-library (srfi 170)
  (export

#|
   ;; 3.1  Errors

   ;; useful for debuging

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


   ;; 3.2  I/O

   open-file
   open/read open/write open/read+write
   open/append open/create open/exclusive open/nofollow open/truncate
   fdes->textual-input-port fdes->binary-input-port
   fdes->textual-output-port fdes->binary-output-port
   port-fdes
   close-fdes


   ;; 3.3  File system

   create-directory create-fifo create-hard-link create-symlink
   read-symlink
   rename-file
   delete-directory
   set-file-mode set-file-owner set-file-group
   set-file-timespecs timespec/now timespec/omit
   truncate-file

   file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   ;; file-info:blksize file-info:blocks are below, not in Windows
   file-info:atime file-info:mtime file-info:ctime

   file-info-directory? file-info-fifo? file-info-symlink?
   file-info-regular? file-info-socket? file-info-device?
   directory-files
   make-directory-files-generator
   open-directory read-directory close-directory

   real-path

   temp-file-prefix
   create-temp-file
   ;; call-with-temporary-filename is left as an exercise for the reader


   ;; 3.5  Process state

   umask set-umask!
   current-directory set-current-directory!
   pid
   nice

   user-uid user-gid
   user-effective-uid user-effective-gid
   user-supplementary-gids


   ;; 3.6  User and group database access

   user-info user-info?
   user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
   user-info:full-name user-info:parsed-full-name

   group-info group-info?
   group-info:name group-info:gid


   ;; 3.10  Time

   posix-time monotonic-time


   ;; 3.11  Environment variables

   set-environment-variable!
   delete-environment-variable!


   ;; 3.12  Terminal device control

   terminal?

   )
  
  (cond-expand ((not windows)
    (export

     ;; 3.3  File system

     file-info:blksize file-info:blocks)))

  (cond-expand ((not bsd)
    (export

#|
     ;; 3.1  Errors

     ;; useful for debuging

     errno/EMULTIHOP errno/ENOLINK
     ;; STREAMS:
     errno/ENODATA errno/ENOSTR errno/ENOSR errno/ETIME
|#
    )))


  (cond-expand
   (chibi
    (import
     (scheme base)
     (scheme case-lambda)
     (only (scheme process-context) get-environment-variable)

     (chibi)
     (chibi optional) ;; Snow package for optional args
     (only (chibi filesystem) file-exists? delete-file open open/write open/create)

     (only (srfi 1) alist-cons take)
     (only (srfi 8) receive) ;; the only export, but let us maintain form
     (only (srfi 27) random-integer)
     (only (srfi 69) make-hash-table hash-table-set! hash-table-ref)
     (only (srfi 98) get-environment-variables)
     (only (srfi 115) regexp-replace-all regexp-split)
     (srfi 151) ;; bitwise operators
     (rename (only (srfi 174) timespec timespec? timespec-seconds timespec-nanoseconds)
             (timespec make-timespec))
     (only (srfi 198) raise-foreign-error)
     )

    (include-shared "170/170")
    (include-shared "170/aux")))

  (include "170/common.scm")
  (include "170/170.scm")
  )
