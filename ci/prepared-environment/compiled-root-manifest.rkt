#lang racket/base

;; q/ci/prepared-environment/compiled-root-manifest.rkt — v1.00.30 W3
;;
;; Manifest schema and PURE validation for the trusted compiled root
;; prototype. This module never runs code from the artifact: validation
;; inspects manifest data and payload bytes only, before any compiled
;; module can be loaded (PLAN §Safe compiled-root architecture: "Verify
;; BEFORE loading any compiled module").
;;
;; Identity dimensions bound by the manifest (same-run, same-head reuse
;; only; W3 never enables cross-PR reuse):
;;   - schema + creation stamp
;;   - trusted producer provenance (label + trust flag + build command)
;;   - exact Racket executable digest, VM version
;;   - platform/ABI (OS, arch, shared-object suffix)
;;   - locked dependency set (lockfile digest, optional in W3 fixtures)
;;   - every compilation input source: rel path, digest, size
;;   - every compiled payload member: rel path, digest, size, mtime
;;   - canonical whole-payload digest (sorted rel paths + framed bytes)
;;
;; Dirty, untracked or generated compilation inputs are rejected by the
;; producer in compiled-root.rkt (git status check). Validation here
;; additionally fails closed on any path escaping the root (traversal),
;; any symlink inside the payload, any digest mismatch, any missing
;; field, and any producer not in the consumer's trusted set.

(require racket/date
         racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         "manifest.rkt")

(provide manifest-schema
         sha256-bytes
         sha256-file
         canonical-payload-digest
         payload-file-digest
         make-compiled-root-manifest
         validate-compiled-root-manifest
         manifest-source-paths
         manifest-payload-members
         manifest-reason-ok
         (struct-out exn:fail:compiled-root-manifest))

(define manifest-schema "q-compiled-root-manifest-1")
(define manifest-reason-ok #t)

;; ---------------------------------------------------------------- digests

;; SHA-256 via the pure-Racket FIPS 180-4 implementation owned by the
;; v1.00.11 manifest contract (single authoritative copy in this
;; directory; no duplicated constant tables, no openssl dependency).
(define (sha256-bytes b)
  (define tmp (make-temporary-file "qcr-digest~a"))
  (dynamic-wind (lambda () (with-output-to-file tmp #:exists 'replace (lambda () (write-bytes b))))
                (lambda () (sha256-file tmp))
                (lambda () (delete-file tmp))))

;; Digest of one payload member: canonical "rel\0len:bytes" framing so a
;; renamed file cannot silently substitute for another.
(define (payload-file-digest rel-bytes content)
  (sha256-bytes (bytes-append rel-bytes
                              (bytes 0)
                              (string->bytes/utf-8 (format "~a:" (bytes-length content)))
                              content)))

;; Whole-payload digest: sorted rel paths (bytes) + framed contents.
(define (canonical-payload-digest members)
  ;; members: (list (cons rel-bytes content-bytes))
  (define sorted (sort members bytes<? #:key car))
  (sha256-bytes (apply bytes-append
                       (for/list ([m (in-list sorted)])
                         (bytes-append (car m)
                                       (bytes 0)
                                       (string->bytes/utf-8 (format "~a:" (bytes-length (cdr m))))
                                       (cdr m))))))

;; ------------------------------------------------------------- safe paths

;; A manifest path must be a relative, normalized POSIX-style string: no
;; absolute form, no ".." component, no empty component, no backslashes.
(define (safe-rel-path-string? s)
  (and (string? s)
       (non-empty-string? s)
       (not (string-prefix? s "/"))
       (not (string-contains? s "\\"))
       (let ([parts (string-split s "/" #:trim? #f)])
         (and (pair? parts)
              (andmap (lambda (p)
                        (and (non-empty-string? p) (not (string=? p ".")) (not (string=? p ".."))))
                      parts)))))

(define (path-stays-inside-root? root rel-string)
  ;; Traversal/symlink-escape gate: canonicalized root/rel must remain
  ;; inside the canonicalized root; escaping symlinks fail this check.
  (define target (build-path root (string->path rel-string)))
  (cond
    [(not (file-exists? target)) #f]
    [else
     (define root-c (path->directory-path (simplify-path (path->complete-path root))))
     (define target-c (simplify-path (path->complete-path target)))
     (define rel (explode-path (find-relative-path root-c target-c)))
     (and (pair? rel) (not (ormap (lambda (p) (equal? p 'up)) rel)))]))

;; ------------------------------------------------------------ shape checks

(struct exn:fail:compiled-root-manifest exn:fail ())

(define (raise-manifest! fmt . args)
  (raise (exn:fail:compiled-root-manifest (apply format fmt args) (current-continuation-marks))))

;; system-type fields arrive as strings, symbols or bytes depending on
;; key and platform; normalize to string once, in one place.
(define (->string v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [(bytes? v) (bytes->string/utf-8 v)]
    [else (error '->string "unexpected system-type value: ~s" v)]))

(define (req-hash h key where)
  (define v (hash-ref h key #f))
  (unless (hash? v)
    (raise-manifest! "~a: missing or non-hash field ~a" where key))
  v)

(define (req-string h key where)
  (define v (hash-ref h key #f))
  (unless (string? v)
    (raise-manifest! "~a: missing or non-string field ~a" where key))
  v)

(define (req-hex h key where)
  (define v (req-string h key where))
  (unless (and (>= (string-length v) 64)
               (andmap (lambda (c) (or (char-numeric? c) (and (char>=? c #\a) (char<=? c #\f))))
                       (string->list (string-downcase v))))
    (raise-manifest! "~a: field ~a is not a hex digest" where key))
  (string-downcase v))

(define (check-source-entry e where)
  (unless (hash? e)
    (raise-manifest! "~a: source entry is not a hash" where))
  (unless (safe-rel-path-string? (hash-ref e 'path #f))
    (raise-manifest! "~a: unsafe source path ~s" where (hash-ref e 'path #f)))
  (unless (safe-rel-path-string? (hash-ref e 'zo #f))
    (raise-manifest! "~a: unsafe zo path ~s" where (hash-ref e 'zo #f)))
  (req-hex e 'digest where)
  (req-hex e 'zo-digest where)
  (unless (exact-positive-integer? (hash-ref e 'bytes #f))
    (raise-manifest! "~a: entry ~s lacks byte size" where (hash-ref e 'path)))
  (unless (exact-positive-integer? (hash-ref e 'zo-bytes #f))
    (raise-manifest! "~a: entry ~s lacks zo byte size" where (hash-ref e 'path)))
  (unless (exact-nonnegative-integer? (hash-ref e 'zo-mtime-ms #f))
    (raise-manifest! "~a: entry ~s lacks zo mtime" where (hash-ref e 'path))))

;; -------------------------------------------------------------- producer

;; make-compiled-root-manifest
;;   #:sources (list (list src-rel-string zo-rel-string))
;;   #:root-dir directory containing payload members at the rel zo paths
;;   (staging root at manifest build time). Compose the manifest hash,
;;   computing every digest from the actual bytes on disk. Raises on any
;;   missing member.
(define (make-compiled-root-manifest
         #:sources sources
         #:root-dir root-dir
         #:racket-executable executable-path
         #:producer-label label
         #:producer-trusted? [trusted? #t]
         #:producer-command [command "racket ci/prepared-environment/compiled-root.rkt produce"]
         #:lockfile [lockfile #f])
  (unless (path-string? root-dir)
    (raise-manifest! "root-dir must be a path or string"))
  (define entries
    (for/list ([s (in-list sources)])
      (match-define (list src-rel zo-rel) s)
      (unless (and (safe-rel-path-string? src-rel) (safe-rel-path-string? zo-rel))
        (raise-manifest! "unsafe manifest pair ~s ~s" src-rel zo-rel))
      (define src-abs (build-path root-dir src-rel))
      (define zo-abs (build-path root-dir zo-rel))
      (unless (file-exists? src-abs)
        (raise-manifest! "source missing: ~a" src-rel))
      (unless (file-exists? zo-abs)
        (raise-manifest! "payload member missing: ~a" zo-rel))
      (define src-b (file->bytes src-abs))
      (define zo-b (file->bytes zo-abs))
      (hasheq 'path
              src-rel
              'digest
              (sha256-bytes src-b)
              'bytes
              (bytes-length src-b)
              'zo
              zo-rel
              'zo-digest
              (sha256-bytes zo-b)
              'zo-bytes
              (bytes-length zo-b)
              'zo-mtime-ms
              (file-or-directory-modify-seconds zo-abs))))
  (define payload-digest
    (canonical-payload-digest (for/list ([e (in-list entries)])
                                (cons (string->bytes/utf-8 (hash-ref e 'zo))
                                      (file->bytes (build-path root-dir (hash-ref e 'zo)))))))
  (define lock-ht
    (if lockfile
        (hasheq 'lockfile
                (if (path? lockfile)
                    (path->string lockfile)
                    lockfile)
                'lockfile-digest
                (sha256-file (if (path? lockfile)
                                 lockfile
                                 (string->path lockfile))))
        (hasheq 'lockfile #f 'lockfile-digest #f)))
  (hasheq 'schema
          manifest-schema
          'created
          (parameterize ([date-display-format 'iso-8601])
            (date->string (current-date) #t))
          'producer
          (hasheq 'label label 'trusted trusted? 'command command)
          'racket
          (hasheq 'version
                  (version)
                  'executable-digest
                  (sha256-file (path->complete-path executable-path))
                  'executable-path
                  (path->string executable-path))
          'platform
          (hasheq 'os
                  (->string (system-type))
                  'arch
                  (->string (system-type 'arch))
                  'so-suffix
                  (->string (system-type 'so-suffix)))
          'packages
          lock-ht
          'sources
          entries
          'payload
          (hasheq 'algorithm "sha256" 'digest payload-digest 'files (length entries))))

;; ------------------------------------------------------------- validation

;; validate-compiled-root-manifest
;;   Pure manifest-first validation. Returns the manifest unchanged when
;;   every identity dimension matches; raises
;;   exn:fail:compiled-root-manifest with a precise reason otherwise.
;;   NO compiled code runs inside.
(define (validate-compiled-root-manifest manifest
                                         #:root-dir root-dir
                                         #:racket-executable executable-path
                                         #:trusted-producer-labels trusted-labels
                                         #:expect-lockfile-digest [expect-lock-digest #f])
  (define where "validate")
  (with-handlers ([exn:fail:compiled-root-manifest? raise]
                  [exn:fail? (lambda (e) (raise-manifest! "~a" (exn-message e)))])
    (unless (hash? manifest)
      (raise-manifest! "manifest is not a hash"))
    (unless (equal? (hash-ref manifest 'schema #f) manifest-schema)
      (raise-manifest! "unknown manifest schema ~s (expected ~s)"
                       (hash-ref manifest 'schema #f)
                       manifest-schema))
    (define producer (req-hash manifest 'producer where))
    (define producer-label (req-string producer 'label "producer"))
    (unless (member producer-label trusted-labels)
      (raise-manifest! "untrusted producer ~s (trusted: ~a)" producer-label trusted-labels))
    (unless (eq? (hash-ref producer 'trusted #f) #t)
      (raise-manifest! "producer ~s is not flagged trusted" producer-label))
    (define racket-ht (req-hash manifest 'racket where))
    (define expect-exec-digest (sha256-file (path->complete-path executable-path)))
    (unless (string=? (req-hex racket-ht 'executable-digest "racket") expect-exec-digest)
      (raise-manifest! "racket executable digest mismatch: manifest ~s consumer ~s"
                       (hash-ref racket-ht 'executable-digest)
                       expect-exec-digest))
    (unless (equal? (req-string racket-ht 'version "racket") (version))
      (raise-manifest! "racket version mismatch: manifest ~s consumer ~s"
                       (hash-ref racket-ht 'version)
                       (version)))
    (define platform (req-hash manifest 'platform where))
    (unless (and (string=? (req-string platform 'os "platform") (->string (system-type)))
                 (string=? (req-string platform 'arch "platform") (->string (system-type 'arch)))
                 (string=? (req-string platform 'so-suffix "platform")
                           (->string (system-type 'so-suffix))))
      (raise-manifest! "platform/ABI mismatch: manifest ~s consumer ~s/~a/~a"
                       (hash->list platform)
                       (system-type)
                       (system-type 'arch)
                       (system-type 'so-suffix)))
    (when expect-lock-digest
      (define packages (req-hash manifest 'packages where))
      (define m-lock (hash-ref packages 'lockfile-digest #f))
      (unless (and (string? m-lock)
                   (string=? (string-downcase m-lock) (string-downcase expect-lock-digest)))
        (raise-manifest! "locked dependency-set digest mismatch: manifest ~s expected ~s"
                         m-lock
                         expect-lock-digest)))
    (define sources (hash-ref manifest 'sources #f))
    (unless (and (list? sources) (pair? sources))
      (raise-manifest! "manifest has no sources"))
    (for ([e (in-list sources)])
      (check-source-entry e "sources"))
    (define payload (req-hash manifest 'payload where))
    (unless (equal? (req-string payload 'algorithm "payload") "sha256")
      (raise-manifest! "unsupported payload algorithm ~s" (hash-ref payload 'algorithm)))
    (for ([e (in-list sources)])
      (define zo-rel (hash-ref e 'zo))
      (define zo-abs (build-path root-dir zo-rel))
      (unless (path-stays-inside-root? root-dir zo-rel)
        (raise-manifest! "payload path escapes root, is a symlink, or is missing: ~s" zo-rel))
      (define zo-b (file->bytes zo-abs))
      (unless (equal? (sha256-bytes zo-b) (hash-ref e 'zo-digest))
        (raise-manifest! "payload corruption detected: ~s" zo-rel))
      (unless (equal? (bytes-length zo-b) (hash-ref e 'zo-bytes))
        (raise-manifest! "payload size mismatch: ~s" zo-rel)))
    (define computed
      (canonical-payload-digest (for/list ([e (in-list sources)])
                                  (cons (string->bytes/utf-8 (hash-ref e 'zo))
                                        (file->bytes (build-path root-dir (hash-ref e 'zo)))))))
    (unless (string=? computed (req-hex payload 'digest "payload"))
      (raise-manifest! "whole-payload digest mismatch: manifest ~s computed ~s"
                       (hash-ref payload 'digest)
                       computed))
    manifest))

(define (manifest-source-paths manifest)
  (for/list ([e (in-list (hash-ref manifest 'sources))])
    (hash-ref e 'path)))

(define (manifest-payload-members manifest)
  (for/list ([e (in-list (hash-ref manifest 'sources))])
    (list (hash-ref e 'path) (hash-ref e 'zo) (hash-ref e 'zo-digest) (hash-ref e 'zo-mtime-ms))))
