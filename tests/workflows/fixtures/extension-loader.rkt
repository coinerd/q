#lang racket/base

;; tests/workflows/fixtures/extension-loader.rkt — extension loading fixture
;;
;; Convenience functions for loading extensions in workflow tests.
;; Provides a simple API to create a registry and load one or more extensions.

(require "../../../agent/event-bus.rkt"
         "../../../extensions/api.rkt"
         (only-in "../../../extensions/loader.rkt" load-extension!)
         (only-in "../../../wiring/run-modes.rkt" load-extensions-from-dir!))

(provide make-loaded-registry
         load-extension-from-path
         load-extensions-from-dir-path)

;; make-loaded-registry : (listof path-string?) [#:event-bus any/c] -> extension-registry?
;;
;; Create an extension registry and load all given extension paths into it.
;; Silently skips paths that don't exist.
(define (make-loaded-registry paths #:event-bus [bus #f])
  (define reg (make-extension-registry))
  (for ([p (in-list paths)])
    (when (file-exists? p)
      (load-extension! reg p #:event-bus bus)))
  reg)

;; load-extension-from-path : extension-registry? path-string? [#:event-bus any/c] -> boolean?
;;
;; Load a single extension from a file path. Returns #t on success.
(define (load-extension-from-path reg path #:event-bus [bus #f])
  (and (file-exists? path) (load-extension! reg path #:event-bus bus)))

;; load-extensions-from-dir-path : extension-registry? path-string? [#:event-bus any/c] -> void?
;;
;; Load all extensions from a directory into the registry.
(define (load-extensions-from-dir-path reg dir #:event-bus [bus #f])
  (load-extensions-from-dir! reg dir #:event-bus bus))
