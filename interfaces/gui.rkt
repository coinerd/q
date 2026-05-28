#lang racket/base

;; q/interfaces/gui.rkt — Public facade for GUI mode
;;
;; Re-exports from gui/main.rkt. This is the stable interface consumed
;; by main.rkt and external SDK users.

(require racket/contract
         racket/runtime-path)

(provide (contract-out [run-gui-with-runtime (-> any/c any/c void?)]))

;; Lazy require to avoid loading gui-easy at module level.
;; This allows the system to work without gui-easy installed
;; (e.g., headless CI, TUI-only mode).
;;
;; NOTE: We use define-runtime-path (not a bare string in dynamic-require)
;; because dynamic-require resolves relative string paths against the
;; enclosing module's resolved path at RUNTIME, which may differ from
;; the source tree layout when compiled. define-runtime-path records
;; the correct relative path at compile time.

(define-runtime-path gui-main-module "../gui/main.rkt")

(define (run-gui-with-runtime rt-config cfg)
  (define run-gui
    (with-handlers ([exn:fail? (λ (e)
                                 (eprintf "GUI mode unavailable: ~a\n" (exn-message e))
                                 (eprintf "Install gui-easy-lib: raco pkg install gui-easy-lib\n")
                                 (exit 1))])
      (dynamic-require gui-main-module 'run-gui-with-runtime)))
  (run-gui rt-config cfg))
