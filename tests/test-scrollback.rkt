#lang racket

(require rackunit
         "../tui/scrollback.rkt"
         "../tui/state.rkt")

;; ============================================================
;; transcript-entry serialization
;; ============================================================

(test-case "transcript-entry->jsexpr serializes fields"
  (define entry (transcript-entry 'assistant "hello" 12345 (hash 'model "gpt")))
  (define j (transcript-entry->jsexpr entry))
  (check-equal? (hash-ref j 'kind) "assistant")
  (check-equal? (hash-ref j 'text) "hello")
  (check-equal? (hash-ref j 'timestamp) 12345))

(test-case "jsexpr->transcript-entry deserializes fields"
  (define h (hash 'kind "user" 'text "hi" 'timestamp 99 'meta (hash)))
  (define entry (jsexpr->transcript-entry h))
  (check-pred transcript-entry? entry)
  (check-eq? (transcript-entry-kind entry) 'user)
  (check-equal? (transcript-entry-text entry) "hi")
  (check-equal? (transcript-entry-timestamp entry) 99))

(test-case "transcript-entry round-trip preserves data"
  (define original (transcript-entry 'system "test" 42 (hash 'key "val")))
  (define restored (jsexpr->transcript-entry (transcript-entry->jsexpr original)))
  (check-equal? (transcript-entry-text restored) "test")
  (check-eq? (transcript-entry-kind restored) 'system)
  (check-equal? (transcript-entry-timestamp restored) 42))

;; ============================================================
;; save-scrollback / load-scrollback
;; ============================================================

(test-case "save-scrollback and load-scrollback round-trip"
  (define tmp (make-temporary-file "q-test-scrollback-~a.jsonl"))
  (define entries
    (list (transcript-entry 'assistant "hello" 1 (hash))
          (transcript-entry 'user "world" 2 (hash))))
  (save-scrollback entries tmp)
  (define loaded (load-scrollback tmp))
  (check-equal? (length loaded) 2)
  (check-equal? (transcript-entry-text (car loaded)) "hello")
  (check-equal? (transcript-entry-text (cadr loaded)) "world")
  (delete-file tmp))

(test-case "load-scrollback returns empty for missing file"
  (define result (load-scrollback "/nonexistent/path.jsonl"))
  (check-equal? result '()))

(test-case "save-scrollback trims to max entries"
  ;; Create more than 500 entries
  (define tmp (make-temporary-file "q-test-scrollback-~a.jsonl"))
  (define entries
    (for/list ([i (in-range 600)])
      (transcript-entry 'system (format "entry-~a" i) i (hash))))
  (save-scrollback entries tmp)
  (define loaded (load-scrollback tmp))
  ;; Should be trimmed to 500 (scrollback-max-entries)
  (check-true (<= (length loaded) 500))
  ;; First entry should be entry-100 (entries 100-599 kept)
  (check-equal? (transcript-entry-text (car loaded)) "entry-100")
  (delete-file tmp))

(test-case "save-scrollback handles empty list"
  (define tmp (make-temporary-file "q-test-scrollback-~a.jsonl"))
  (save-scrollback '() tmp)
  (define loaded (load-scrollback tmp))
  (check-equal? loaded '())
  (delete-file tmp))
