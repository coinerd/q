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
