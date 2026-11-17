#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
;; Superseded fingerprint-mechanics cases (binding, drift, symlinks) moved to
;; tests/test-testing-audit.rkt with the generated admission record; this file
;; keeps the production-record integrity pin.
(require rackunit
         file/sha1
         racket/file
         racket/runtime-path
         "../scripts/testing/quick-audit.rkt")
(define-runtime-path root "..")
(define-runtime-path production-record "../tests/.quick-admission.rktd")
(module+ test
  (test-case "the committed record binds every owner's dependency closure"
    (check-true (file-exists? production-record))
    (define record (call-with-input-file production-record read))
    (define owners (cdr (assoc 'owners record)))
    (check-equal? (length owners) (length (admitted-owners)))
    ;; The recorded closure is exactly what live discovery produces, and the
    ;; recorded hashes match the shipped bytes (the reviewed identity).
    (for ([entry (in-list owners)])
      (define owner-rel (car entry))
      (define data (cdr entry))
      (check-equal? (cdr (assoc 'closure data)) (closure-of root owner-rel)
                    (format "~a closure drift" owner-rel))
      (for ([pair (in-list (cdr (assoc 'hashes data)))])
        (check-equal? (cdr pair)
                      (call-with-input-file (build-path root (car pair)) sha1)
                      (format "~a hash drift: ~a" owner-rel (car pair)))))))
