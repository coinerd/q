#lang racket/base

;; wiring/extension-setup.rkt — Extension loading and hook setup for all modes
;; STABILITY: internal
;;
;; Extracted from wiring/run-modes.rkt (v0.29.15 W2) to reduce fan-in.
;; Creates extension registry, loads extensions from global + project dirs,
;; and queries extensions for commands and shortcuts.

(require racket/contract
         racket/file
         racket/string
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" load-extension!)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload)
         (only-in "../tui/palette.rkt"
                  commands-from-hashes
                  merge-extension-commands
                  make-command-registry)
         (only-in "../tui/keymap.rkt" shortcut-specs->keymap keymap-merge))

(provide (contract-out
          [make-wired-extension-registry (-> any/c path-string? (values any/c any/c any/c))]
          [load-extensions-from-dir! (->* (any/c path-string?) (#:event-bus any/c) void?)]))

;; ============================================================
;; make-wired-extension-registry
;; ============================================================

;;; make-wired-extension-registry : event-bus? path? -> (values extension-registry? list? list?)
;;;
;;; Creates an extension registry, loads extensions from global (~/.q/extensions/)
;;; and project-local (./.q/extensions/) directories, then queries extensions for
;;; commands and shortcuts. Returns the populated registry plus extension-provided
;;; commands and shortcuts.
(define (make-wired-extension-registry bus project-dir)
  (define ext-reg (make-extension-registry))
  (define q-home (build-path (find-system-path 'home-dir) ".q"))
  (load-extensions-from-dir! ext-reg (build-path q-home "extensions") #:event-bus bus)
  (load-extensions-from-dir! ext-reg (build-path project-dir ".q" "extensions") #:event-bus bus)

  ;; Query extensions for commands and shortcuts
  (define ext-cmds
    (let ()
      (define result (dispatch-hooks 'register-commands (hasheq) ext-reg))
      (if (and result (eq? (hook-result-action result) 'amend))
          (commands-from-hashes (hash-ref (hook-result-payload result) 'commands '()))
          '())))
  (define ext-shortcuts
    (let ()
      (define result (dispatch-hooks 'register-shortcuts (hasheq) ext-reg))
      (if (and result (eq? (hook-result-action result) 'amend))
          (hash-ref (hook-result-payload result) 'shortcuts '())
          '())))

  (values ext-reg ext-cmds ext-shortcuts))

;; ============================================================
;; load-extensions-from-dir! helper
;; ============================================================

;; Load all .rkt extension files from a directory into the registry.
;; Silently skips if directory doesn't exist or individual loads fail.

(define (load-extensions-from-dir! ext-reg dir #:event-bus [event-bus #f])
  (when (directory-exists? dir)
    ;; v0.21.5 (F1): Sort by filename for deterministic load order across platforms.
    (define files
      (sort (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
                    (directory-list dir #:build? #t))
            (λ (a b) (string<? (path->string a) (path->string b)))))
    (for ([f (in-list files)])
      (with-handlers ([exn:fail? (λ (e)
                                   (fprintf (current-error-port)
                                            "Warning: failed to load extension ~a: ~a\n"
                                            f
                                            (exn-message e)))])
        (load-extension! ext-reg f #:event-bus event-bus)))))
