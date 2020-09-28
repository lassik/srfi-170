;; please see copyright notice in ./COPYING

;; Note this should be wrapping Chibi's fileno object, but there are enough complexities to that it'll probably have to wait until Alex Shinn can spare the attention.

(define-record-type File-Descriptor-Object
  (make-fdo fd)
    fdo?
  (fd fdo:fd))
