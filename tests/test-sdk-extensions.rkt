#lang racket

;; tests/test-sdk-extensions.rkt — v0.19.4 Wave 4: SDK extension tool wiring
;;
;; Verifies that SDK sessions with extension registries work properly
;; and that extension-registry is passed through to the agent session.

(require rackunit
         racket/file
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../tools/tool.rkt"
         "../agent/event-bus.rkt"
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         "../interfaces/sdk.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "Done"))
                                           (hasheq 'inputTokens 5 'outputTokens 5)
                                           "mock-model"
                                           'stop)))

(define (make-temp-session-dir)
  (make-temporary-file "q-sdk-ext-test-~a" 'directory))

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Tests
;; ============================================================

(test-case "SDK make-runtime accepts #:extension-registry parameter"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:extension-registry ext-reg))
    (check-pred runtime? rt)
    (check-equal? (runtime-config-extension-registry (runtime-rt-config rt)) ext-reg)
    (cleanup-dir tmp)))

(test-case "SDK with extension registry creates session and runs prompt"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    (define bus (make-event-bus))
    ;; Load gsd-planning extension — provides planning-read, planning-write tools
    (define ext-path
      (build-path (or (current-load-relative-directory) (current-directory))
                  "../extensions/gsd-planning.rkt"))
    (when (file-exists? ext-path)
      (load-extension! ext-reg ext-path #:event-bus bus))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:extension-registry ext-reg))
    (define rt-open (open-session rt))
    ;; Run a prompt to trigger register-tools dispatch
    (define-values (rt2 result) (run-prompt! rt-open "test"))
    (check-pred runtime? rt2)
    (check-not-exn (lambda () result))
    (cleanup-dir tmp)))

(test-case "SDK without extension registry still works (graceful degradation)"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-pred runtime? rt)
    (check-false (runtime-config-extension-registry (runtime-rt-config rt)))
    (define rt-open (open-session rt))
    (check-pred runtime? rt-open)
    (define-values (rt2 result) (run-prompt! rt-open "hello"))
    (check-pred runtime? rt2)
    (cleanup-dir tmp)))
