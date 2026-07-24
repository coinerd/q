#lang racket/base

;; Atomic, permission-preserving replacement guarded by the identity, contents,
;; and portable metadata observed by a preceding read.

(require racket/file)

(provide atomic-replace-file-if-unchanged)

(define (target-unchanged? path
                           expected-identity
                           expected-content
                           expected-permission-bits
                           expected-modify-seconds
                           expected-size)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    ;; Check identity and metadata on both sides of the content read so a
    ;; replacement or chmod during validation cannot satisfy only one part of
    ;; the guard. These are the portable metadata fields Racket exposes here.
    (define expected-bytes
      (if (bytes? expected-content)
          expected-content
          (string->bytes/utf-8 expected-content)))
    (define identity-before (file-or-directory-identity path))
    (define permissions-before (file-or-directory-permissions path 'bits))
    (define modify-seconds-before (file-or-directory-modify-seconds path))
    (define size-before (file-size path))
    (and (= identity-before expected-identity)
         (= permissions-before expected-permission-bits)
         (= modify-seconds-before expected-modify-seconds)
         (= size-before expected-size)
         (bytes=? (file->bytes path) expected-bytes)
         (= (file-or-directory-identity path) identity-before)
         (= (file-or-directory-permissions path 'bits) permissions-before)
         (= (file-or-directory-modify-seconds path) modify-seconds-before)
         (= (file-size path) size-before))))

;; Return #t after replacement, or #f if an optimistic guard detects that the
;; target changed. The temporary file is created beside the target so rename is
;; a same-filesystem atomic replacement.
;;
;; Portable Racket provides atomic rename, but no conditional rename/CAS that
;; replaces a directory entry only when it still names an expected identity.
;; Linux renameat2 adds NOREPLACE/EXCHANGE modes, not an expected-inode compare;
;; project file locks are advisory and cannot constrain non-cooperating writers.
;; Consequently such a writer can still change the target in the few
;; instructions between the final guard and rename. The repeated guard is
;; deliberately adjacent to rename and rejects every change visible through
;; that boundary; it does not claim external-writer exclusion after it.
(define (atomic-replace-file-if-unchanged path
                                          expected-identity
                                          expected-content
                                          expected-permission-bits
                                          expected-modify-seconds
                                          expected-size
                                          new-content
                                          #:before-guard [before-guard void]
                                          #:before-final-guard [before-final-guard void])
  (define-values (parent _name _directory?) (split-path path))
  (define temp-path #f)
  (define (unchanged?)
    (target-unchanged? path
                       expected-identity
                       expected-content
                       expected-permission-bits
                       expected-modify-seconds
                       expected-size))
  (dynamic-wind void
                (lambda ()
                  (set! temp-path
                        (make-temporary-file ".q-edit-~a.tmp"
                                             #:base-dir (if (path? parent)
                                                            parent
                                                            (current-directory))))
                  (call-with-output-file temp-path
                                         #:exists 'truncate
                                         (lambda (out)
                                           (display new-content out)
                                           (flush-output out)))
                  (file-or-directory-permissions temp-path expected-permission-bits)
                  (before-guard path)
                  (and (unchanged?)
                       (begin
                         ;; Deterministic final-boundary test seam. Production's
                         ;; no-op leaves only this final check before rename.
                         (before-final-guard path)
                         (and (unchanged?)
                              (begin
                                (rename-file-or-directory temp-path path #t)
                                #t)))))
                (lambda ()
                  (when (and temp-path (file-exists? temp-path))
                    (delete-file temp-path)))))
