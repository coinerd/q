#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;;; test-step-executor-relocation.rkt — Verify that step execution
;;; has been relocated from agent/iteration/ to runtime/iteration/.

(require rackunit
         racket/file)

;; Resolve q/ root: parent of tests/ directory
(define q-dir
  (simplify-path
   (string->path (or (getenv "Q_DIR")
                     (if (and (directory-exists? "..") (file-exists? "../main.rkt")) ".." ".")))))

(define (repo-path . parts)
  (apply build-path q-dir parts))

;; ============================================================
;; Tests: Module relocation
;; ============================================================

(test-case "step-executor.rkt exists in runtime/iteration/"
  (check-true (file-exists? (repo-path "runtime" "iteration" "step-executor.rkt"))
              "runtime/iteration/step-executor.rkt must exist after relocation"))

(test-case "step-interpreter.rkt no longer exists in agent/iteration/"
  (check-false (file-exists? (repo-path "agent" "iteration" "step-interpreter.rkt"))
               "agent/iteration/step-interpreter.rkt must be removed after relocation"))

(test-case "runtime/iteration/step-executor.rkt provides interpret-step"
  (define src (file->string (repo-path "runtime" "iteration" "step-executor.rkt")))
  (check-true (string-contains? src "interpret-step")
              "step-executor.rkt must provide interpret-step"))

(test-case "runtime/iteration/step-executor.rkt provides execute-pending-tool-calls"
  (define src (file->string (repo-path "runtime" "iteration" "step-executor.rkt")))
  (check-true (string-contains? src "execute-pending-tool-calls")
              "step-executor.rkt must provide execute-pending-tool-calls"))

;; ============================================================
;; Tests: Architecture — agent/iteration/ no longer imports
;; heavy Runtime implementation modules
;; ============================================================

(define (rkt-files-in-dir dir)
  (define full-dir (repo-path dir))
  (if (directory-exists? full-dir)
      (for/list ([f (in-directory full-dir)]
                 #:when (and (file-exists? f) (regexp-match? #rx"\\.rkt$" (path->string f))))
        f)
      '()))

(define (file-contains? path pattern)
  (and (file-exists? path) (string-contains? (file->string path) pattern)))

(test-case "agent/iteration/ does not import runtime/tool-coordinator"
  ;; v0.99.86: extract-tool-calls-from-messages moved to util/tool/tool-extract.rkt
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "tool-coordinator"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports tool-coordinator: ~a" violations)))

(test-case "agent/iteration/ does not import runtime/session-store"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "session-store"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports session-store: ~a" violations)))

(test-case "agent/iteration/ does not import runtime/runtime-helpers"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "runtime-helpers"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports runtime-helpers: ~a" violations)))

(test-case "agent/iteration/ does not import runtime/iteration/step-executor"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "step-executor"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports step-executor: ~a" violations)))

(test-case "agent/iteration/ does not import runtime/context/context-policy"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "context-policy"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports context-policy: ~a" violations)))

(test-case "agent/iteration/ does not import runtime/compaction/"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "compaction"))
      (path->string f)))
  (check-equal? violations '() (format "agent/iteration/ still imports compaction: ~a" violations)))

(test-case "agent/iteration/ does not import rollback-actions"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "rollback-actions"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports rollback-actions: ~a" violations)))

(test-case "agent/iteration/ does not import effect-executor"
  (define files (rkt-files-in-dir "agent/iteration"))
  (define violations
    (for/list ([f files]
               #:when (file-contains? f "effect-executor"))
      (path->string f)))
  (check-equal? violations
                '()
                (format "agent/iteration/ still imports effect-executor: ~a" violations)))
