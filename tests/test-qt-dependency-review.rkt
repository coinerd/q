#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path
         "../scripts/testing/quick-audit.rkt")
(define-runtime-path root "..")
(module+ test
  (test-case "review binds dependencies and rejects missing or linked paths"
    (define dir (make-temporary-file "qt-deps-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (make-directory (build-path dir "tests"))
       (make-directory (build-path dir "util"))
       (define entry (car audited))
       (define test-path (build-path dir (car entry)))
       (define dep-path (build-path dir "util/exn.rkt"))
       (copy-file (build-path root (car entry)) test-path)
       (check-false (audit-ok? dir entry) "missing dependency")
       (copy-file (build-path root "util/exn.rkt") dep-path)
       (check-true (audit-ok? dir entry))
       (call-with-output-file dep-path #:exists 'append (lambda (out) (display "\n; drift\n" out)))
       (check-false (audit-ok? dir entry) "changed dependency")
       (delete-file dep-path)
       (define file-linked?
         (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
           (make-file-or-directory-link (build-path root "util/exn.rkt") dep-path)
           #t))
       (if file-linked?
           (begin
             (check-false (audit-ok? dir entry) "linked dependency")
             (delete-file dep-path))
           (displayln "SKIP linked dependency: link creation unavailable"))
       (delete-directory (build-path dir "util"))
       (define dir-linked?
         (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
           (make-file-or-directory-link (build-path root "util") (build-path dir "util"))
           #t))
       (if dir-linked?
           (check-false (audit-ok? dir entry) "linked parent directory")
           (displayln "SKIP linked parent: link creation unavailable"))
       (check-false (audit-ok? dir (cons "../escape.rkt" (cdr entry))) "unreviewed escape"))
     (lambda () (delete-directory/files dir)))))
