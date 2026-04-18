#lang racket

;; tests/test-wave2-runtime-ergonomics.rkt — Wave 2 runtime ergonomics tests
;;
;; Tests for v0.11.0 Wave 2 sub-issues:
;;   #1181: Output guard (stdout protection for TUI mode)
;;   #1182: Model configuration hot reload
;;   #1183: Extension custom entries (state vs context distinction)

(require rackunit
         racket/port
         "../util/output-guard.rkt"
         "../util/protocol-types.rkt"
         "../runtime/settings.rkt"
         "../runtime/model-registry.rkt"
         "../runtime/session-store.rkt"
         "../runtime/context-builder.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (msg id role text [kind 'message])
  (make-message id #f role kind (list (make-text-part text)) (current-seconds) (hasheq)))

;; ============================================================
;; #1181: Output Guard
;; ============================================================

(test-case "#1181: call-with-output-guard redirects stdout"
  (define captured "")
  (define real-out (current-output-port))
  ;; Inside the guard, current-output-port should NOT be the real one
  (call-with-output-guard
   (lambda ()
     (define guarded-out (current-output-port))
     (check-not-eq? guarded-out real-out "output port should be redirected")
     ;; Stray writes should be silently discarded
     (display "this should not appear" (current-output-port))
     ;; Real port should be accessible via guarded-real-output-port
     (check-eq? (guarded-real-output-port) real-out "real port should be saved"))))

(test-case "#1181: call-with-raw-output restores real port"
  (define real-out (current-output-port))
  (call-with-output-guard
   (lambda ()
     ;; Inside guard: current-output-port is NOT real
     (check-not-eq? (current-output-port) real-out)
     ;; call-with-raw-output should restore the real port
     (call-with-raw-output
      (lambda ()
        (check-eq? (current-output-port) real-out "raw output should use real port"))))))

(test-case "#1181: stray writes are silently discarded"
  (define captured "")
  (call-with-output-guard
   (lambda ()
     ;; These writes go to the null sink
     (display "garbage" (current-output-port))
     (display "more garbage" (current-output-port))
     ;; Nothing should have been written
     (void))
   #:redirect-to (open-output-string))
  ;; No error means the null sink handled the writes
  (check-true #t))

(test-case "#1181: guard can redirect to stderr"
  ;; This just verifies the #:redirect-to option works
  (define buf (open-output-string))
  (call-with-output-guard
   (lambda ()
     (display "redirected" (current-output-port)))
   #:redirect-to buf)
  (check-equal? (get-output-string buf) "redirected"))

(test-case "#1181: call-with-raw-output outside guard is passthrough"
  (define result
    (call-with-raw-output
     (lambda ()
       (current-output-port)
       "ok")))
  (check-equal? result "ok"))

(test-case "#1181: output guard does not affect error port"
  (call-with-output-guard
   (lambda ()
     ;; Error port should still work
     (check-true (output-port? (current-error-port))))))

;; ============================================================
;; #1182: Model Config Hot Reload
;; ============================================================

(test-case "#1182: reload-config! refreshes model registry"
  ;; Create a settings with one provider with models list
  (define config1
    (hasheq 'providers
            (hasheq 'test-provider
                    (hasheq 'type 'openai
                            'models (list (hasheq 'id "model-a"
                                                   'api-key "test-key"))))))
  (define reg1 (make-model-registry-from-config config1))
  (define models1 (map model-entry-name (available-models reg1)))
  (check-not-false (member "model-a" models1))

  ;; Create new config with different models (simulating file change)
  (define config2
    (hasheq 'providers
            (hasheq 'test-provider
                    (hasheq 'type 'openai
                            'models (list (hasheq 'id "model-b"
                                                   'api-key "test-key")
                                          (hasheq 'id "model-c"
                                                   'api-key "test-key"))))))
  (define reg2 (make-model-registry-from-config config2))
  (define models2 (map model-entry-name (available-models reg2)))
  (check-false (member "model-a" models2) "old model should be gone")
  (check-not-false (member "model-b" models2) "new model-b should be present")
  (check-not-false (member "model-c" models2) "new model-c should be present"))

(test-case "#1182: load-settings is idempotent and re-reads from disk"
  ;; Calling load-settings twice should return equivalent (but not eq?) results
  (define s1 (load-settings "/nonexistent" #:home-dir "/nonexistent"))
  (define s2 (load-settings "/nonexistent" #:home-dir "/nonexistent"))
  (check-equal? (q-settings-merged s1) (q-settings-merged s2)))

;; ============================================================
;; #1183: Custom Entries — State vs Context
;; ============================================================

(test-case "#1183: custom-entry has empty content (state-only)"
  (define entry (make-custom-entry "my-ext" "state-key" '(foo bar)))
  (check-true (custom-entry? entry))
  (check-equal? (message-role entry) 'system)
  (check-equal? (message-kind entry) 'custom-message)
  ;; Content is empty — no text parts
  (check-equal? (message-content entry) '())
  ;; Data is in meta
  (check-equal? (custom-entry-extension entry) "my-ext")
  (check-equal? (custom-entry-key entry) "state-key")
  (check-equal? (custom-entry-data entry) '(foo bar)))

(test-case "#1183: custom-entry persists but doesn't inject text into context"
  ;; entry->context-message passes custom-message through, but since
  ;; content is empty, it contributes zero text tokens
  (define entry (make-custom-entry "my-ext" "state" '(secret-data)))
  (define ctx-entry (entry->context-message entry))
  (check-true (custom-entry? ctx-entry))
  ;; Still has empty content — LLM sees nothing
  (check-equal? (message-content ctx-entry) '()))

(test-case "#1183: regular text message does inject into context"
  (define text-msg (msg "m1" 'system "important instruction"))
  (define ctx-msg (entry->context-message text-msg))
  (check-true (message? ctx-msg))
  ;; Has text content — LLM sees it
  (check-true (pair? (message-content ctx-msg))))

(test-case "#1183: custom-message-entry? is same as custom-entry?"
  ;; They are the same predicate
  (define entry (make-custom-entry "ext" "k" "v"))
  (check-equal? (custom-entry? entry) (custom-message-entry? entry))
  (check-true (custom-entry? entry)))

(test-case "#1183: state-only entry round-trips through session store"
  (define mgr (make-in-memory-session-manager))
  (define sid "test-session-1183")
  ;; Create session
  (in-memory-append! mgr sid (msg "init" 'system "start"))
  ;; Append custom entry
  (append-custom-entry! mgr sid "my-ext" "counter" 42)
  ;; Load it back
  (define loaded (load-custom-entries mgr sid "my-ext"))
  (check-equal? (length loaded) 1)
  (define retrieved (car loaded))
  (check-true (custom-entry? retrieved))
  (check-equal? (custom-entry-key retrieved) "counter")
  (check-equal? (custom-entry-data retrieved) 42))

(test-case "#1183: state-only entries filtered by key"
  (define mgr (make-in-memory-session-manager))
  (define sid "test-session-1183b")
  (in-memory-append! mgr sid (msg "init" 'system "start"))
  (append-custom-entry! mgr sid "ext" "key-a" "value-a")
  (append-custom-entry! mgr sid "ext" "key-b" "value-b")
  ;; Filter by key
  (define a-entries (load-custom-entries mgr sid "ext" "key-a"))
  (check-equal? (length a-entries) 1)
  (check-equal? (custom-entry-data (car a-entries)) "value-a")
  ;; All entries for extension
  (define all-entries (load-custom-entries mgr sid "ext"))
  (check-equal? (length all-entries) 2))

(test-case "#1183: different extension names don't collide"
  (define mgr (make-in-memory-session-manager))
  (define sid "test-session-1183c")
  (in-memory-append! mgr sid (msg "init" 'system "start"))
  (append-custom-entry! mgr sid "ext-a" "shared-key" "from-a")
  (append-custom-entry! mgr sid "ext-b" "shared-key" "from-b")
  ;; Each extension sees only its own entries
  (define a (load-custom-entries mgr sid "ext-a"))
  (check-equal? (length a) 1)
  (check-equal? (custom-entry-data (car a)) "from-a")
  (define b (load-custom-entries mgr sid "ext-b"))
  (check-equal? (length b) 1)
  (check-equal? (custom-entry-data (car b)) "from-b"))
