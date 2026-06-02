#lang racket

;; BOUNDARY: architecture

;; Verifies v0.81.1 protocol-types migration guardrails:
;; source modules should use canonical protocol sub-modules directly, while tests
;; and the backward-compatibility facade may still exercise protocol-types.rkt.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/string
         (only-in "../util/event-classes.rkt" make-stream-text-event stream-text-event?))

(define project-root
  (simplify-path (build-path (or (current-load-relative-directory) (current-directory)) "..")))

(define (path-string path)
  (path->string (simplify-path path)))

(define (source-racket-file? path)
  (define s (path-string path))
  (and (regexp-match? #rx"[.]rkt$" s)
       (not (string-contains? s "/compiled/"))
       (not (string-contains? s "/tests/"))
       (not (regexp-match? #rx"/util/protocol-types[.]rkt$" s))))

(define (source-protocol-facade-reference? path)
  (regexp-match? #rx"\\\"[^\\\"]*protocol-types[.]rkt\\\"" (file->string path)))

(define protocol-migration-tests
  (test-suite "protocol-types source migration"

    (test-case "non-test source files do not require protocol-types facade"
      (define offenders
        (for/list ([path (in-list (find-files source-racket-file? project-root))]
                   #:when (source-protocol-facade-reference? path))
          (find-relative-path project-root path)))
      (check-equal? offenders '()))

    (test-case "event-classes imports canonical modules without circular facade dependency"
      (define evt (make-stream-text-event "turn-1" (hasheq 'text "hello")))
      (check-true (stream-text-event? evt)))))

(module+ main
  (run-tests protocol-migration-tests))

(module+ test
  (run-tests protocol-migration-tests))
