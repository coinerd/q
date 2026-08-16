#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-planning-exec-context.rkt — D7 regression tests
;;
;; Campaign 81f9be4b W3 failed 5 attempts. D7 root cause: get-base-dir
;; (extensions/gsd/tool-handlers.rkt) calls ctx-cwd — which is
;; extension-ctx-working-directory, a contracted accessor requiring
;; extension-ctx? — on the tool scheduler's exec-context struct.
;; Interactive sessions masked the bug because current-pinned-dir is set
;; after /plan; campaign executors run reset-all-gsd-state! (pinned-dir #f)
;; so execution falls through to the broken (ctx-cwd exec-ctx) call and
;; raises an instant contract violation, burning wave iterations.
;;
;; These tests prove planning-read / planning-write work under BOTH
;; context types with pinned-dir unset — the executor condition.

(require rackunit
         racket/file
         racket/string
         "../extensions/gsd-planning.rkt"
         "../extensions/gsd/tool-handlers.rkt"
         "../extensions/gsd/core.rkt"
         (only-in "../extensions/gsd/session-state.rkt" set-pinned-dir!)
         "../extensions/tool-api.rkt"
         (only-in "../extensions/context.rkt" make-extension-ctx ctx-cwd)
         (only-in "../tools/tool.rkt" tool-result-is-error?))

;; Tool result text helper (mirrors test-gsd-planning.rkt)
(define (read-result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(define (with-temp-dir proc)
  (define dir (make-temporary-directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda ()
                  (when (directory-exists? dir)
                    (for ([f (directory-list dir)])
                      (when (file-exists? (build-path dir f))
                        (delete-file (build-path dir f))))
                    (when (directory-exists? (build-path dir ".planning"))
                      (for ([f (directory-list (build-path dir ".planning"))])
                        (when (file-exists? (build-path dir ".planning" f))
                          (delete-file (build-path dir ".planning" f))))
                      (delete-directory (build-path dir ".planning")))
                    (delete-directory dir)))))

(define (write-planning-artifact! dir name content)
  (define artifact-dir (build-path dir ".planning"))
  (make-directory* artifact-dir)
  (call-with-output-file (build-path artifact-dir name) (lambda (out) (display content out))))

(define (clear-pinned-dir!)
  ;; Simulate the executor condition: reset-all-gsd-state! sets pinned-dir #f.
  (set-pinned-dir! #f))

;; ---------------------------------------------------------------------
;; D7: planning-read under exec-context (executor condition)
;; ---------------------------------------------------------------------

(test-case "D7: planning-read works under a real exec-context with pinned-dir #f"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "STATE.md" "# State\nW3 in progress")
                   (clear-pinned-dir!)
                   (define ctx (make-exec-context #:working-directory dir))
                   (define result (handle-planning-read (hasheq 'artifact "STATE") ctx))
                   (check-false (tool-result-is-error? result))
                   (check-true (string-contains? (read-result-text result) "W3 in progress")))))

(test-case "D7: planning-write works under a real exec-context with pinned-dir #f"
  (with-temp-dir
   (lambda (dir)
     (clear-pinned-dir!)
     (define ctx (make-exec-context #:working-directory dir))
     (define result
       (handle-planning-write (hasheq 'artifact "STATE" 'content "# State\nUpdated via executor")
                              ctx))
     (check-false (tool-result-is-error? result))
     (check-true (file-exists? (build-path dir ".planning" "STATE.md")))
     (check-true (string-contains? (call-with-input-file (build-path dir ".planning" "STATE.md")
                                                         (lambda (in) (port->string in)))
                                   "Updated via executor")))))

;; ---------------------------------------------------------------------
;; D7: planning-read under extension-ctx (interactive condition, pinned-dir #f)
;; ---------------------------------------------------------------------

(test-case "D7: planning-read works under an extension-ctx with pinned-dir #f"
  (with-temp-dir (lambda (dir)
                   (write-planning-artifact! dir "STATE.md" "# State\nInteractive read")
                   (clear-pinned-dir!)
                   (define ctx
                     (make-extension-ctx #:session-id "test-session"
                                         #:session-dir dir
                                         #:event-bus #f
                                         #:extension-registry #f
                                         #:working-directory dir))
                   (define result (handle-planning-read (hasheq 'artifact "STATE") ctx))
                   (check-false (tool-result-is-error? result))
                   (check-true (string-contains? (read-result-text result) "Interactive read")))))

;; ---------------------------------------------------------------------
;; D7: base_dir arg still wins over context (precedence preserved)
;; ---------------------------------------------------------------------

(test-case "D7: explicit base_dir arg takes precedence over exec-context"
  (with-temp-dir
   (lambda (dir)
     (define other-dir (make-temporary-directory))
     (dynamic-wind void
                   (lambda ()
                     (write-planning-artifact! other-dir "STATE.md" "# State\nExplicit base_dir")
                     (clear-pinned-dir!)
                     (define ctx (make-exec-context #:working-directory dir))
                     (define result
                       (handle-planning-read (hasheq 'artifact "STATE" 'base_dir other-dir) ctx))
                     (check-false (tool-result-is-error? result))
                     (check-true (string-contains? (read-result-text result) "Explicit base_dir")))
                   (lambda ()
                     (when (directory-exists? other-dir)
                       (for ([f (directory-list other-dir)])
                         (when (file-exists? (build-path other-dir f))
                           (delete-file (build-path other-dir f))))
                       (when (directory-exists? (build-path other-dir ".planning"))
                         (for ([f (directory-list (build-path other-dir ".planning"))])
                           (when (file-exists? (build-path other-dir ".planning" f))
                             (delete-file (build-path other-dir ".planning" f))))
                         (delete-directory (build-path other-dir ".planning")))
                       (delete-directory other-dir)))))))
