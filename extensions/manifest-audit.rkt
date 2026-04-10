#lang racket/base

;; extensions/manifest-audit.rkt -- package-level checksum verification and auditing
;;
;; Provides verify-package-checksum and audit-package which require
;; manifest knowledge. Lives in extensions/ (not util/) because it
;; depends on the manifest struct.

(require racket/contract
         racket/port
         racket/string
         racket/file
         racket/path
         racket/list
         "manifest.rkt"
         "../util/checksum.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide
 (contract-out
  [verify-package-checksum (-> path-string? boolean?)]
  [audit-package           (-> path-string? (listof string?))]))

;; ============================================================
;; Package checksum verification
;; ============================================================

(define (verify-package-checksum pkg-dir)
  (define qpm-path (build-path pkg-dir "qpm.json"))
  (define m (read-qpm-manifest qpm-path))
  (cond
    [(not m) #f]
    [(not (qpm-manifest-checksum m)) #t]
    [else
     (string=? (qpm-manifest-checksum m)
               (compute-manifest-checksum pkg-dir))]))

;; ============================================================
;; Package audit
;; ============================================================

(define (audit-package pkg-dir)
  (define qpm-path (build-path pkg-dir "qpm.json"))
  (define m (read-qpm-manifest qpm-path))
  (define issues '())

  (define (add-issue! msg)
    (set! issues (append issues (list msg))))

  (cond
    [(not m)
     (list (format "cannot read manifest: ~a" qpm-path))]
    [else
     (define manifest-files (qpm-manifest-files m))

     ;; Check for missing files (in manifest but not on disk)
     (for ([f (in-list manifest-files)])
       (define full-path (build-path pkg-dir f))
       (unless (file-exists? full-path)
         (add-issue! (format "missing file: ~a" f))))

     ;; Check for extra files (on disk but not in manifest and not qpm.json)
     (define disk-files
       (parameterize ([current-directory pkg-dir])
         (for/list ([f (in-directory ".")])
           (path->string
            (find-relative-path (simple-form-path pkg-dir)
                                (simple-form-path f))))))
     (for ([f (in-list disk-files)])
       (unless (or (member f manifest-files)
                   (string=? f "qpm.json")
                   (string-prefix? f "compiled"))
         (add-issue! (format "extra file: ~a" f))))

     ;; Check checksum if present
     (when (qpm-manifest-checksum m)
       (define computed (compute-manifest-checksum pkg-dir))
       (unless (string=? (qpm-manifest-checksum m) computed)
         (add-issue! (format "checksum mismatch: expected ~a, got ~a"
                             (qpm-manifest-checksum m) computed))))

     issues]))
