#lang racket/base
;; Private canonical storage primitives shared by supplemental journals/claims.
(require json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         ffi/unsafe
         ffi/unsafe/port
         "../../util/json/checksum.rkt"
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         "supplemental-evidence-authority.rkt")
(provide freeze
         json-hash
         bytes-for
         seal
         exact-keys?
         local-lock
         root!
         private-directory!
         check-file!
         present?
         atomic-write!
         read-sealed
         sync-directory!)
(define (refuse message)
  (error 'supplemental-evidence-journal "~a" message))
(define max-bytes (* 1024 1024))
(define (freeze v [depth 0])
  (when (> depth 64)
    (refuse "JSON nesting exceeds 64 levels"))
  (cond
    [(hash? v)
     (unless (and (immutable? v)
                  (andmap (lambda (k) (and (symbol? k) (symbol-interned? k))) (hash-keys v)))
       (refuse "facts and base binding must be immutable JSON hashes"))
     (for/hasheq ([(k value) (in-hash v)])
       (values k (freeze value (add1 depth))))]
    [(list? v) (map (lambda (x) (freeze x (add1 depth))) v)]
    [(string? v) (string->immutable-string v)]
    [(or (exact-integer? v) (boolean? v) (eq? v 'null)) v]
    [else (refuse "unsupported canonical JSON data")]))
(define (json-hash v)
  (unless (hash? v)
    (refuse "expected immutable JSON hash"))
  (freeze v))
(define (bytes-for record)
  (define b (string->bytes/utf-8 (canonical-evidence-json record)))
  (when (> (bytes-length b) max-bytes)
    (refuse "journal exceeds 1 MiB"))
  b)
(define (seal record)
  (define body (hash-remove record 'sha256))
  (hash-set body
            'sha256
            (string->immutable-string (sha256-string (bytes->string/utf-8 (bytes-for body))))))
(define (exact-keys? h keys)
  (and (hash? h) (= (hash-count h) (length keys)) (andmap (lambda (k) (hash-has-key? h k)) keys)))
(define local-locks (make-hash))
(define local-locks-lock (make-semaphore 1))
(define (local-lock path)
  (call-with-semaphore local-locks-lock
                       (lambda () (hash-ref! local-locks path (lambda () (make-semaphore 1))))))
(define-runtime-path source-root "../..")
(define (within? path base)
  (define p (explode-path (simplify-path path #f)))
  (define b (explode-path (simplify-path base #f)))
  (and (<= (length b) (length p)) (equal? b (take p (length b)))))
(define (check-chain! path)
  (when (link-exists? path)
    (refuse "symlink directory component"))
  (unless (eq? (file-or-directory-type path #f) 'directory)
    (refuse "non-directory component"))
  (define-values (base _name _dir?) (split-path path))
  (when (and (path? base) (not (equal? base path)))
    (check-chain! base)))
(define (private-directory! path)
  (check-chain! path)
  (unless (= #o700 (bitwise-and #o7777 (file-or-directory-permissions path 'bits)))
    (refuse "private directory must have mode 0700")))
(define (present? path)
  (or (link-exists? path) (file-or-directory-type path #f)))
(define (root! [allow-absent? #f])
  (define raw (current-supplemental-authority-root))
  (unless (and (path-string? raw) (complete-path? raw) (not (member 'up (explode-path raw))))
    (refuse "absolute authority root required; no project context root"))
  (define text
    (path->string (if (path? raw)
                      raw
                      (string->path raw))))
  (define path
    (string->path (if (regexp-match? #px"^/+$" text)
                      "/"
                      (regexp-replace #px"/+$" text ""))))
  (define root (simplify-path path #f))
  (when (or (within? root (current-directory)) (within? root source-root))
    (refuse "authority root is inside project/control source"))
  (cond
    [(and allow-absent? (not (present? path)))
     (let absent! ([missing path])
       (define-values (parent name _dir?) (split-path missing))
       (cond
         [(not (present? parent)) (absent! (simplify-path parent #f))]
         [else
          (check-chain! parent)
          (when (member name (directory-list parent))
            (refuse "authority root absence is uncertain"))]))
     #f]
    [else
     (private-directory! path)
     root]))
(define (check-file! path)
  (when (link-exists? path)
    (refuse "symlink journal file"))
  (unless (eq? (file-or-directory-type path #f) 'file)
    (refuse "journal is not a regular file"))
  (define stat (file-or-directory-stat path #t))
  (unless (= #o100000 (bitwise-and #o170000 (hash-ref stat 'mode)))
    (refuse "journal is not a regular file"))
  (unless (= 1 (hash-ref stat 'hardlink-count))
    (refuse "hardlinked journal file"))
  (unless (= #o600 (bitwise-and #o7777 (file-or-directory-permissions path 'bits)))
    (refuse "journal file must have mode 0600"))
  (when (> (file-size path) max-bytes)
    (refuse "oversized journal file")))
(define libc (and (eq? (system-type 'os) 'unix) (ffi-lib #f)))
(define c-fsync (and libc (get-ffi-obj "fsync" libc (_fun _int -> _int))))
(define c-open (and libc (get-ffi-obj "open" libc (_fun _path _int -> _int))))
(define c-close (and libc (get-ffi-obj "close" libc (_fun _int -> _int))))
(define (sync-port! port)
  (unless c-fsync
    (refuse "durable journal unsupported on this platform"))
  (flush-output port)
  (unless (zero? (c-fsync (unsafe-port->file-descriptor port)))
    (refuse "file fsync failed")))
(define (sync-directory! dir)
  (unless c-open
    (refuse "durable journal unsupported on this platform"))
  (define fd (c-open dir 0))
  (when (< fd 0)
    (refuse "cannot open journal directory for fsync"))
  (dynamic-wind void
                (lambda ()
                  (unless (zero? (c-fsync fd))
                    (refuse "directory fsync failed")))
                (lambda () (c-close fd))))
(define (atomic-write! path record)
  (when (present? path)
    (check-file! path))
  (define dir (path-only path))
  (define temp (make-temporary-file ".supplemental-~a.tmp" #f dir))
  (dynamic-wind void
                (lambda ()
                  (file-or-directory-permissions temp #o600)
                  (call-with-output-file temp
                                         (lambda (out)
                                           (write-bytes (bytes-for record) out)
                                           (sync-port! out))
                                         #:exists 'truncate)
                  (rename-file-or-directory temp path #t)
                  (sync-directory! dir))
                (lambda ()
                  (when (file-exists? temp)
                    (delete-file temp)))))
(define (read-sealed path)
  (check-file! path)
  (define b (file->bytes path))
  (define in (open-input-bytes b))
  (define record (freeze (read-json in)))
  (unless (and (eof-object? (peek-byte in))
               (hash? record)
               (hash-has-key? record 'sha256)
               (equal? record (seal record))
               (bytes=? b (bytes-for record)))
    (refuse "noncanonical or altered journal"))
  (check-file! path)
  record)
