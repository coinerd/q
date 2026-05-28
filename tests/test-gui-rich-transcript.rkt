#lang racket

;; q/tests/test-gui-rich-transcript.rkt — Tests for rich-transcript-view pure helpers

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/components/rich-transcript-view.rkt")

(define-test-suite test-gui-rich-transcript
                   ;; ── role->label ──
                   (test-case "role->label maps known roles"
                     (check-equal? (role->label "user") "You")
                     (check-equal? (role->label "assistant") "Assistant")
                     (check-equal? (role->label "system") "System")
                     (check-equal? (role->label "tool") "Tool"))
                   (test-case "role->label handles unknown role"
                     (check-equal? (role->label "custom") "Custom"))
                   (test-case "role->label handles #f"
                     (check-equal? (role->label #f) "Unknown"))
                   ;; ── role->color ──
                   (test-case "role->color returns theme colors"
                     (define t (default-theme))
                     (check-equal? (role->color "user" t) (theme-ref t 'accent))
                     (check-equal? (role->color "assistant" t) (theme-ref t 'foreground))
                     (check-equal? (role->color "system" t) (theme-ref t 'muted))
                     (check-equal? (role->color "tool" t) (theme-ref t 'warning)))
                   ;; ── hex->color-object ──
                   (test-case "hex->color-object parses hex with #"
                     (define c (hex->color-object "#1e1e2e"))
                     (check-equal? (hash-ref c 'r) 30)
                     (check-equal? (hash-ref c 'g) 30)
                     (check-equal? (hash-ref c 'b) 46))
                   (test-case "hex->color-object handles no hash prefix"
                     (define c (hex->color-object "ff0000"))
                     (check-equal? (hash-ref c 'r) 255)
                     (check-equal? (hash-ref c 'g) 0)
                     (check-equal? (hash-ref c 'b) 0))
                   (test-case "hex->color-object handles #f"
                     (define c (hex->color-object #f))
                     (check-equal? (hash-ref c 'r) 0))
                   ;; ── delta factories ──
                   (test-case "make-role-label-delta produces descriptor"
                     (define d (make-role-label-delta "You" "#89b4fa"))
                     (check-equal? (hash-ref d 'type) 'role-label)
                     (check-true (hash-ref d 'bold))
                     (check-equal? (hash-ref d 'label) "You")
                     (check-equal? (hash-ref d 'family) 'modern))
                   (test-case "make-content-delta produces descriptor"
                     (define d (make-content-delta "#cdd6f4"))
                     (check-equal? (hash-ref d 'type) 'content)
                     (check-false (hash-ref d 'bold)))
                   ;; ── render-message-descriptor ──
                   (test-case "render-message-descriptor produces segments"
                     (define t (default-theme))
                     (define msg (hash 'role "user" 'text "Hello"))
                     (define desc (render-message-descriptor msg t))
                     (check-equal? (hash-ref desc 'role) "user")
                     (check-equal? (hash-ref desc 'text) "Hello")
                     (define segs (hash-ref desc 'segments))
                     (check-equal? (length segs) 2)
                     (check-equal? (hash-ref (car segs) 'type) 'role-label)
                     (check-equal? (hash-ref (car segs) 'text) "You: ")
                     (check-equal? (hash-ref (cadr segs) 'type) 'content)
                     (check-equal? (hash-ref (cadr segs) 'text) "Hello\n\n"))
                   (test-case "render-message-descriptor uses theme colors"
                     (define t (default-theme))
                     (define msg (hash 'role "assistant" 'text "World"))
                     (define desc (render-message-descriptor msg t))
                     (define segs (hash-ref desc 'segments))
                     (define role-style (hash-ref (car segs) 'style))
                     (check-equal? (hash-ref role-style 'color) (theme-ref t 'foreground)))
                   ;; ── messages->render-plan ──
                   (test-case "messages->render-plan produces list"
                     (define t (default-theme))
                     (define msgs
                       (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
                     (define plan (messages->render-plan msgs t))
                     (check-equal? (length plan) 2)
                     (check-equal? (hash-ref (car plan) 'role) "user")
                     (check-equal? (hash-ref (cadr plan) 'role) "assistant")))

;; ── Diff-based update tests ──

(define-test-suite
 test-transcript-diff
 (test-case "compute-transcript-diff: no change"
   (define msgs (list (hash 'role "user" 'text "Hi")))
   (check-equal? (compute-transcript-diff msgs msgs) '()))
 (test-case "compute-transcript-diff: append one message"
   (define old (list (hash 'role "user" 'text "Hi")))
   (define new (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   (define diff (compute-transcript-diff old new))
   (check-equal? (length diff) 1)
   (check-equal? (hash-ref (car diff) 'op) 'append)
   (check-equal? (hash-ref (hash-ref (car diff) 'msg) 'role) "assistant"))
 (test-case "compute-transcript-diff: append multiple messages"
   (define old '())
   (define new (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   (define diff (compute-transcript-diff old new))
   (check-equal? (length diff) 2)
   (check-equal? (hash-ref (car diff) 'op) 'append)
   (check-equal? (hash-ref (cadr diff) 'op) 'append))
 (test-case "compute-transcript-diff: update last message (streaming)"
   (define old (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel")))
   (define new (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   (define diff (compute-transcript-diff old new))
   (check-equal? (length diff) 1)
   (check-equal? (hash-ref (car diff) 'op) 'update-last)
   (check-equal? (hash-ref (hash-ref (car diff) 'msg) 'text) "Hello"))
 (test-case "compute-transcript-diff: reset (clear)"
   (define old (list (hash 'role "user" 'text "Hi")))
   (define new '())
   (define diff (compute-transcript-diff old new))
   (check-equal? (length diff) 1)
   (check-equal? (hash-ref (car diff) 'op) 'reset)
   (check-equal? (hash-ref (car diff) 'msgs) '()))
 (test-case "update-last-message replaces text"
   (define msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel")))
   (define updated (update-last-message msgs "Hello"))
   (check-equal? (length updated) 2)
   (check-equal? (hash-ref (cadr updated) 'text) "Hello")
   ;; First message unchanged
   (check-equal? (hash-ref (car updated) 'text) "Hi"))
 (test-case "update-last-message on empty list returns empty"
   (check-equal? (update-last-message '() "text") '()))
 (test-case "apply-diff-to-plan: append"
   (define t (default-theme))
   (define plan (messages->render-plan (list (hash 'role "user" 'text "Hi")) t))
   (define diff (list (hash 'op 'append 'msg (hash 'role "assistant" 'text "Hello"))))
   (define new-plan (apply-diff-to-plan plan diff t))
   (check-equal? (length new-plan) 2)
   (check-equal? (hash-ref (cadr new-plan) 'role) "assistant"))
 (test-case "apply-diff-to-plan: update-last"
   (define t (default-theme))
   (define plan
     (messages->render-plan (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel"))
                            t))
   (define diff (list (hash 'op 'update-last 'msg (hash 'role "assistant" 'text "Hello"))))
   (define new-plan (apply-diff-to-plan plan diff t))
   (check-equal? (length new-plan) 2)
   (check-equal? (hash-ref (cadr new-plan) 'text) "Hello"))
 (test-case "apply-diff-to-plan: reset"
   (define t (default-theme))
   (define plan (messages->render-plan (list (hash 'role "user" 'text "Hi")) t))
   (define diff (list (hash 'op 'reset 'msgs '())))
   (define new-plan (apply-diff-to-plan plan diff t))
   (check-equal? new-plan '()))
 (test-case "apply-diff-to-plan: no-ops returns same plan"
   (define t (default-theme))
   (define plan (messages->render-plan (list (hash 'role "user" 'text "Hi")) t))
   (define new-plan (apply-diff-to-plan plan '() t))
   (check-equal? new-plan plan)))

(run-tests test-gui-rich-transcript)
(run-tests test-transcript-diff)


;; ── Auto-scroll state tests ──

(define-test-suite test-scroll-state

  (test-case "make-scroll-state defaults to auto-scroll enabled"
    (define ss (make-scroll-state))
    (check-true (scroll-state-auto-scroll? ss))
    (check-false (scroll-state-user-scrolled-up? ss)))

  (test-case "make-scroll-state can be created with auto disabled"
    (define ss (make-scroll-state #f))
    (check-false (scroll-state-auto-scroll? ss)))

  (test-case "scroll-state-on-scroll near bottom enables auto-scroll"
    (define ss (make-scroll-state #f))
    (define updated (scroll-state-on-scroll ss 0.97))
    (check-true (scroll-state-auto-scroll? updated))
    (check-false (scroll-state-user-scrolled-up? updated)))

  (test-case "scroll-state-on-scroll mid-way disables auto-scroll"
    (define ss (make-scroll-state))
    (define updated (scroll-state-on-scroll ss 0.5))
    (check-false (scroll-state-auto-scroll? updated))
    (check-true (scroll-state-user-scrolled-up? updated)))

  (test-case "scroll-state-on-scroll at top disables auto-scroll"
    (define ss (make-scroll-state))
    (define updated (scroll-state-on-scroll ss 0.0))
    (check-false (scroll-state-auto-scroll? updated)))

  (test-case "scroll-state-on-submit re-enables auto-scroll"
    (define ss (hash 'auto-scroll #f 'scroll-ratio 0.3 'user-scrolled-up #t))
    (define updated (scroll-state-on-submit ss))
    (check-true (scroll-state-auto-scroll? updated))
    (check-false (scroll-state-user-scrolled-up? updated)))

  (test-case "scroll-state-on-scroll boundary at 0.95"
    (define ss (make-scroll-state #f))
    (define at-boundary (scroll-state-on-scroll ss 0.95))
    (check-true (scroll-state-auto-scroll? at-boundary))
    (define below-boundary (scroll-state-on-scroll ss 0.94))
    (check-false (scroll-state-auto-scroll? below-boundary))))

(run-tests test-scroll-state)


;; ── Code block detection tests ──

(define-test-suite test-code-block-detection

  (test-case "contains-code-blocks? detects triple backticks"
    (check-true (contains-code-blocks? "some ```code``` here"))
    (check-true (contains-code-blocks? "```racket\n(define x 1)```"))
    (check-false (contains-code-blocks? "no code blocks here"))
    (check-false (contains-code-blocks? ""))
    (check-false (contains-code-blocks? 42)))

  (test-case "parse-code-blocks plain text returns single text segment"
    (define result (parse-code-blocks "hello world"))
    (check-equal? (length result) 1)
    (check-equal? (hash-ref (car result) 'type) 'text)
    (check-equal? (hash-ref (car result) 'text) "hello world"))

  (test-case "parse-code-blocks with inline code"
    (define result (parse-code-blocks "before ```code``` after"))
    (check >= (length result) 2)
    ;; Should have text + code-block segments
    (define types (map (lambda (s) (hash-ref s 'type)) result))
    (check-not-false (member 'text types))
    (check-not-false (member 'code-block types)))

  (test-case "parse-code-blocks with fenced code block and language"
    (define result (parse-code-blocks "```racket\n(define x 1)\n```"))
    (check >= (length result) 1)
    (define code-seg (findf (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) result))
    (check-not-false code-seg)
    (when code-seg
      (check-equal? (hash-ref code-seg 'lang) "racket")
      (check-not-false (string-contains? (hash-ref code-seg 'text "") "define"))))

  (test-case "parse-code-blocks with fenced code block no language"
    (define result (parse-code-blocks "```\nsome code\n```"))
    (check >= (length result) 1)
    (define code-seg (findf (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) result))
    (check-not-false code-seg))

  (test-case "parse-code-blocks mixed text and code"
    (define result (parse-code-blocks "Here is code:\n```python\nprint(1)\n```\nDone."))
    (define types (map (lambda (s) (hash-ref s 'type)) result))
    (check >= (length result) 2)
    (check-not-false (member 'text types))
    (check-not-false (member 'code-block types)))

  (test-case "parse-code-blocks handles non-string input"
    (define result (parse-code-blocks 42))
    (check-equal? (length result) 1)
    (check-equal? (hash-ref (car result) 'type) 'text)
    (check-equal? (hash-ref (car result) 'text) "42"))

  (test-case "render-message-with-code-blocks produces segments"
    (define msg (hash 'role "assistant" 'text "Try this:\n```racket\n(+ 1 2)\n```"))
    (define result (render-message-with-code-blocks msg (default-theme)))
    (check-not-false (hash-ref result 'segments #f))
    (define segs (hash-ref result 'segments))
    (check >= (length segs) 2)
    ;; First segment should be role label
    (check-not-false (string-contains? (hash-ref (car segs) 'text "") "Assistant"))
    ;; Should have a code-block segment
    (define code-segs (filter (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) segs))
    (check >= (length code-segs) 1)))

(run-tests test-code-block-detection)


;; ── Code block style helper tests ──

(define-test-suite test-code-block-style

  (test-case "code-block-style returns hash with expected keys"
    (define s (code-block-style (default-theme)))
    (check-not-false (hash-ref s 'background #f))
    (check-not-false (hash-ref s 'foreground #f))
    (check-not-false (hash-ref s 'font #f))
    (check-equal? (hash-ref s 'font) "monospace"))

  (test-case "code-block-header-style with language"
    (define h (code-block-header-style "racket"))
    (check-equal? (hash-ref h 'text) "racket")
    (check-not-false (hash-ref h 'style #f)))

  (test-case "code-block-header-style with #f lang"
    (define h (code-block-header-style #f))
    (check-equal? (hash-ref h 'text) ""))

  (test-case "render-message-descriptor uses code-block parsing"
    ;; Message with code blocks should produce multiple content segments
    (define msg (hash 'role "assistant" 'text "```racket\n(+ 1 2)\n```\nDone."))
    (define desc (render-message-descriptor msg (default-theme)))
    (define segs (hash-ref desc 'segments))
    (check >= (length segs) 3)  ;; role-label + code-block + "Done."
    (define code-segs (filter (lambda (s) (equal? (hash-ref s 'type #f) 'code-block)) segs))
    (check >= (length code-segs) 1)))

(run-tests test-code-block-style)


;; ── Multiline input helper tests ──

(define-test-suite test-multiline-input

  (test-case "input-key-should-submit? Enter without modifiers"
    (check-true (input-key-should-submit? 'return #f #f)))

  (test-case "input-key-should-submit? Shift+Enter does not submit"
    (check-false (input-key-should-submit? 'return #t #f)))

  (test-case "input-key-should-submit? Control+Enter does not submit"
    (check-false (input-key-should-submit? 'return #f #t)))

  (test-case "input-key-should-submit? non-return key does not submit"
    (check-false (input-key-should-submit? 'space #f #f)))

  (test-case "prepare-input-for-submit trims trailing whitespace"
    (check-equal? (prepare-input-for-submit "hello   ") "hello")
    (check-equal? (prepare-input-for-submit "hello") "hello"))

  (test-case "input-line-count single line"
    (check-equal? (input-line-count "hello") 1))

  (test-case "input-line-count multiple lines"
    (check-equal? (input-line-count "line1\nline2\nline3") 3))

  (test-case "input-looks-like-code? detects racket"
    (check-true (input-looks-like-code? "(define x 1)"))
    (check-true (input-looks-like-code? "(let ([x 1]) x)")))

  (test-case "input-looks-like-code? plain text"
    (check-false (input-looks-like-code? "Hello, how are you?"))))

(run-tests test-multiline-input)
