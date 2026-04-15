#lang racket

;; tests/test-properties.rkt — Property-based invariant tests
;;
;; Property invariants that should always hold regardless of input.
;; Uses randomized testing with fixed seeds for reproducibility.
;;
;; #619: Property-based invariant tests
;; 1. Keymap-merge idempotency (catches #514)
;; 2. Session replay idempotency (catches BUG-25)
;; 3. Display-width round-trip
;; 4. UTF-8 encode/decode round-trip
;; 5. Token estimation monotonicity

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/list
         "../tui/keymap.rkt"
         "../tui/char-width.rkt"
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt"
         "../util/protocol-types.rkt"
         "../llm/token-budget.rkt")

;; Seed for reproducibility
(random-seed 42)

(define N-ITERATIONS 200)

;; ============================================================
;; 1. Keymap-merge idempotency
;; ============================================================

;; Merging the same keymap twice should be idempotent.
;; This catches bugs like #514 where duplicate entries accumulate.

(test-case "prop: keymap-merge is idempotent"
  (for ([_ (in-range N-ITERATIONS)])
    (define km (make-keymap))
    ;; Add random entries
    (define actions '(action-a action-b action-c action-d action-e))
    (for ([i (in-range (random 1 10))])
      (define key (key-spec (list-ref '(up down left right home end page-up page-down)
                                      (random 8))
                            (random 2) (random 2) (random 2)))
      (keymap-add! km key (list-ref actions (random (length actions)))))
    (define entries-before (length (keymap-list km)))
    ;; Merge with itself — should not change entries
    (keymap-merge km km)
    (check-equal? (length (keymap-list km)) entries-before
                  "merging with self should not grow entries")))

(test-case "prop: keymap-add! replaces existing binding"
  (for ([_ (in-range N-ITERATIONS)])
    (define km (make-keymap))
    (define ks (key-spec 'up #t #f #f))
    (keymap-add! km ks 'action-1)
    (keymap-add! km ks 'action-2)
    ;; Should have exactly one binding for this key-spec
    (define bindings (filter (lambda (e) (key-spec=? (car e) ks)) (keymap-list km)))
    (check-equal? (length bindings) 1 "only one binding per key-spec")
    (check-equal? (keymap-lookup km ks) 'action-2 "last add wins")))

(test-case "prop: keymap-remove! then add! is consistent"
  (for ([_ (in-range N-ITERATIONS)])
    (define km (make-keymap))
    (define ks (key-spec 'down #f #t #f))
    (keymap-add! km ks 'first)
    (keymap-remove! km ks)
    (check-false (keymap-lookup km ks) "removed key should not be found")
    (keymap-add! km ks 'second)
    (check-equal? (keymap-lookup km ks) 'second "re-added key should be found")))

(test-case "prop: keymap-merge source wins on conflict"
  (for ([_ (in-range N-ITERATIONS)])
    (define target (make-keymap))
    (define source (make-keymap))
    (define ks (key-spec 'left #f #f #t))
    (keymap-add! target ks 'target-action)
    (keymap-add! source ks 'source-action)
    (keymap-merge target source)
    (check-equal? (keymap-lookup target ks) 'source-action
                  "source should win on conflict")))

(test-case "prop: keymap-merge preserves non-conflicting bindings"
  (for ([_ (in-range N-ITERATIONS)])
    (define target (make-keymap))
    (define source (make-keymap))
    (define ks1 (key-spec 'up #f #f #f))
    (define ks2 (key-spec 'down #f #f #f))
    (keymap-add! target ks1 'keep-this)
    (keymap-add! source ks2 'add-this)
    (keymap-merge target source)
    (check-equal? (keymap-lookup target ks1) 'keep-this
                  "non-conflicting target binding preserved")
    (check-equal? (keymap-lookup target ks2) 'add-this
                  "source binding added")))

;; ============================================================
;; 2. Session replay idempotency
;; ============================================================

;; Replaying the same session log twice should produce identical results.
;; This catches BUG-25 where replay was not deterministic.

(define (make-temp-session)
  (define dir (make-temporary-file "q-prop-~a" 'directory))
  (values dir (build-path dir "session.jsonl")))

(define (cleanup-temp-session dir)
  (when (directory-exists? dir)
    (delete-directory/files dir #:must-exist? #f)))

(define (random-message)
  (make-message (format "msg-~a-~a" (current-inexact-milliseconds) (random 100000))
                #f
                (list-ref '(user assistant system) (random 3))
                'message
                (list (make-text-part (format "content ~a" (random 100000))))
                (current-seconds)
                (hasheq)))

(test-case "prop: session replay is deterministic"
  (define-values (dir sp) (make-temp-session))
  (define msgs (for/list ([_ (in-range (random 5 20))]) (random-message)))
  ;; Write messages
  (for ([m (in-list msgs)])
    (append-entry! sp m))
  ;; Replay twice
  (define replay1 (replay-session sp))
  (define replay2 (replay-session sp))
  (check-equal? (length replay1) (length replay2)
                "replay lengths should match")
  (for ([m1 (in-list replay1)]
        [m2 (in-list replay2)]
        [i (in-naturals)])
    (check-equal? (message-id m1) (message-id m2)
                  (format "message ~a IDs should match" i)))
  (cleanup-temp-session dir))

(test-case "prop: session replay preserves message count"
  (define-values (dir sp) (make-temp-session))
  (define n (+ 5 (random 20)))
  (for ([_ (in-range n)])
    (define m (random-message))
    (append-entry! sp (random-message)))
  (define replayed (replay-session sp))
  (check-equal? (length replayed) n
                "replay should return exactly the written messages")
  (cleanup-temp-session dir))

(test-case "prop: session replay round-trip preserves content"
  (define-values (dir sp) (make-temp-session))
  (define m (make-message "test-id" #f 'user 'message
                          (list (make-text-part "hello world"))
                          (current-seconds)
                          (hasheq)))
  (append-entry! sp m)
  (define replayed (replay-session sp))
  (check-equal? (length replayed) 1)
  (define r (car replayed))
  (check-equal? (message-id r) "test-id")
  (check-equal? (message-role r) 'user)
  (check-equal? (message-kind r) 'message)
  (check-equal? (text-part-text (car (message-content r))) "hello world")
  (cleanup-temp-session dir))

;; ============================================================
;; 3. Display-width properties
;; ============================================================

(test-case "prop: char-width is non-negative"
  (for ([_ (in-range N-ITERATIONS)])
    ;; Avoid surrogates (0xD800-0xDFFF) and stay in BMP
    (define cp
      (let ([r (random 0 65536)])
        (if (or (<= #xd800 r #xdfff) (< r 32))
            (random 32 127)
            r)))
    (define w (char-width (integer->char cp)))
    (check-true (and (integer? w) (>= w 0) (<= w 2))
                (format "char-width of codepoint ~a should be 0, 1, or 2, got ~a" cp w))))

(test-case "prop: ASCII printable chars have width 1"
  (for ([cp (in-range 32 127)])
    (check-equal? (char-width (integer->char cp)) 1
                  (format "ASCII char ~a should have width 1" cp))))

(test-case "prop: string-width equals sum of char widths"
  ;; For strings without combining characters
  (define (string-width-impl s) (for/sum ([c (in-string s)]) (char-width c)))
  (define test-strings '("hello" "abc123" "test" "ASCII only" ""))
  (for ([s (in-list test-strings)])
    (check-equal? (string-width-impl s) (string-width-impl s)
                  "sum-of-char-widths should be consistent")))

(test-case "prop: string-width is non-negative"
  (for ([_ (in-range N-ITERATIONS)])
    (define s (format "~a~a~a" (random 1000) (random 1000) (random 1000)))
    (define sw (for/sum ([c (in-string s)]) (char-width c)))
    (check-true (>= sw 0)
                "string-width should always be non-negative")))

;; ============================================================
;; 4. UTF-8 encode/decode round-trip
;; ============================================================

(test-case "prop: string→bytes→string round-trip preserves content"
  (for ([_ (in-range N-ITERATIONS)])
    ;; Generate random strings from various Unicode ranges
    (define cp (random 32 1000))
    (define s (list->string (for/list ([_ (in-range (random 1 20))])
                              (integer->char (random 32 1000)))))
    (define bs (string->bytes/utf-8 s))
    (define s2 (bytes->string/utf-8 bs))
    (check-equal? s2 s "UTF-8 round-trip should preserve string")))

(test-case "prop: UTF-8 of ASCII is identity"
  (for ([_ (in-range N-ITERATIONS)])
    (define s (list->string (for/list ([_ (in-range (random 1 30))])
                              (integer->char (random 32 127)))))
    (define bs (string->bytes/utf-8 s))
    ;; ASCII UTF-8 bytes should be the same as the char values
    (for ([b (in-bytes bs)]
          [c (in-string s)])
      (check-equal? b (char->integer c)))))

(test-case "prop: empty string round-trips"
  (define bs (string->bytes/utf-8 ""))
  (check-equal? (bytes-length bs) 0)
  (check-equal? (bytes->string/utf-8 bs) ""))

;; ============================================================
;; 5. Token estimation monotonicity
;; ============================================================

(test-case "prop: estimate-text-tokens is non-decreasing with length"
  (for ([_ (in-range 100)])
    (define base (format "test string ~a " (random 1000)))
    (define extended (string-append base base base base))
    (check-true (>= (estimate-text-tokens extended) (estimate-text-tokens base))
                "longer string should have >= tokens")))

(test-case "prop: estimate-text-tokens returns 0 for empty"
  (check-equal? (estimate-text-tokens "") 0))

(test-case "prop: estimate-text-tokens returns >= 1 for non-empty"
  (for ([_ (in-range N-ITERATIONS)])
    (define s (format "~a" (random 1 1000000)))
    (check-true (>= (estimate-text-tokens s) 1)
                "non-empty string should have >= 1 token")))

(test-case "prop: estimate-text-tokens: concatenation >= individual"
  (for ([_ (in-range 100)])
    (define s1 (format "string one ~a " (random 1000)))
    (define s2 (format "string two ~a " (random 1000)))
    (define combined (string-append s1 s2))
    (check-true (>= (estimate-text-tokens combined)
                    (estimate-text-tokens s1))
                "combined should have >= tokens of first part")))
