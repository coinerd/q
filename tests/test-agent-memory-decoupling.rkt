#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;;; test-agent-memory-decoupling.rkt — Verify Agent Core has no
;;; dependency on Runtime Memory.

(require rackunit
         racket/file
         racket/path
         racket/runtime-path)

(define-runtime-path here ".")
(define q-dir (simplify-path (build-path here "..")))

(test-case "agent/ contains no imports of runtime/memory"
  (define agent-dir (build-path q-dir "agent"))
  (define (rkt-files-in dir)
    (for/list ([f (in-directory dir)]
               #:when (regexp-match? #rx"\\.rkt$" (path->string f))
               #:when (not (regexp-match? #rx"compiled/" (path->string f))))
      f))
  (define agent-files (rkt-files-in agent-dir))
  (define violations
    (for/list ([f (in-list agent-files)])
      (define src (file->string f))
      (define rel (find-relative-path q-dir f))
      (cond
        [(regexp-match? #rx"runtime/memory" src) (format "~a: imports runtime/memory" rel)]
        [(regexp-match? #rx"maybe-auto-extract-after-response" src)
         (format "~a: references maybe-auto-extract-after-response" rel)]
        [(regexp-match? #rx"maybe-reflect-session-memories" src)
         (format "~a: references maybe-reflect-session-memories" rel)]
        [else #f])))
  (define actual (filter values violations))
  (check-equal? actual '() (format "agent/ still has runtime/memory dependencies: ~a" actual)))

(test-case "turn-orchestrator.rkt calls maybe-auto-extract-after-response!"
  (define src (file->string (build-path q-dir "runtime" "turn-orchestrator.rkt")))
  (check-true (string-contains? src "maybe-auto-extract-after-response!")
              "turn-orchestrator should own post-turn extraction"))

(test-case "turn-orchestrator.rkt calls maybe-reflect-session-memories!"
  (define src (file->string (build-path q-dir "runtime" "turn-orchestrator.rkt")))
  (check-true (string-contains? src "maybe-reflect-session-memories!")
              "turn-orchestrator should own post-turn reflection"))
