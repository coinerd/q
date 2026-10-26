#lang racket

;; @speed fast  ;; @suite tui
;; @boundary unit

;; tests/test-tui-cursor-cell-contract.rkt — BUG-0100 / R100
;;
;; Contract suite for cursor/cell-buffer safety:
;;   1. cell-buffer-ref/set! validate BOTH axes independently against the
;;      ACTUAL buffer dimensions BEFORE flattening; violations raise a
;;      descriptive exn:fail:cell-buffer-bounds (coordinates + dimensions),
;;      never alias ((W,0) != (0,1)) and never silently clamp.
;;   2. The logical end-of-line insertion sentinel (composer col == width)
;;      is projected to a real visible cell (end-of-line placement) shared
;;      by the painting/cursor paths; logical offsets are preserved and the
;;      composer boundary expectation (width5/col5) is NOT flipped.
;;   3. The render loop stamps cursor caches with buffer generation +
;;      physical dims per frame; resize invalidates them; zero-size or
;;      inconsistent geometry defers cursor/frame work explicitly; blink
;;      save/paint/restore is exact and refuses stale geometry.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/cell-buffer.rkt"
                  make-cell-buffer
                  cell-buffer-ref
                  cell-buffer-set!
                  cell-char
                  cell-fg
                  cell-bg
                  cell-buffer-putstring!
                  cell-buffer-snapshot
                  cell-buffer-in-bounds?
                  cell-buffer?
                  cell-buffer-cols
                  cell-buffer-rows
                  exn:fail:cell-buffer-bounds?
                  exn:fail:cell-buffer-bounds-op
                  exn:fail:cell-buffer-bounds-col
                  exn:fail:cell-buffer-bounds-row
                  exn:fail:cell-buffer-bounds-cols
                  exn:fail:cell-buffer-bounds-rows
                  logical-cursor-eol-sentinel?
                  project-logical-cursor)
         (only-in "../tui/render-loop/frame-vdom.rkt" render-frame-vdom!)
         (only-in "../tui/tui-render-loop.rkt"
                  render-frame!
                  render-cursor-blink-frame!
                  apply-cursor-blink-timer!
                  reset-idle-render-state!
                  current-blink-interval-ms
                  tui-buffer-generation-box
                  bump-tui-buffer-generation!
                  invalidate-cursor-caches!
                  cursor-cache-valid-for?
                  last-cursor-buffer-gen-box
                  last-cursor-buffer-cols-box
                  last-cursor-buffer-rows-box)
         (only-in "../tui/context.rkt"
                  make-tui-ctx
                  tui-ctx-ubuf-box
                  tui-ctx-input-state-box
                  tui-ctx-needs-redraw-box)
         (only-in "../tui/terminal.rkt" current-screen-size-provider tui-screen-size-cache-reset!)
         (only-in "../tui/input.rkt"
                  initial-input-state
                  input-insert-string
                  input-cursor-left
                  input-state)
         (only-in "../tui/layout.rkt" compute-layout composer-text-area-width)
         (only-in "../tui/state.rkt" initial-ui-state)
         (only-in "../ui-core/composer-layout.rkt"
                  compute-composer-layout
                  composer-layout-cursor-col
                  composer-layout-row-count
                  composer-layout-width)
         (only-in "../ui-core/ui-diagnostics.rkt"
                  current-ui-diagnostics
                  ui-diagnostics-reset!
                  ui-diagnostics-summary))

;; ─────────────────────────────────────────────────────────────────
;; Helpers
;; ─────────────────────────────────────────────────────────────────

(define unit-width string-length)

;; Both-axis invalid position matrix: read AND write must each raise the
;; descriptive bounds contract for every listed position.
(define (check-both-axes-raise buf positions)
  (for ([pos (in-list positions)])
    (define col (car pos))
    (define row (cadr pos))
    (check-exn exn:fail:cell-buffer-bounds?
               (lambda () (cell-buffer-ref buf col row))
               (format "read (~a,~a) must raise" col row))
    (check-exn exn:fail:cell-buffer-bounds?
               (lambda () (cell-buffer-set! buf col row #:char #\x))
               (format "write (~a,~a) must raise" col row))))

;; Monotonic per-category counter — immune to rate-limit suppression.
(define (diag-count category)
  (hash-ref (ui-diagnostics-summary (current-ui-diagnostics)) category 0))

;; Monotone fake clock so blink toggles always fire.
(define fake-now (box 0.0))
(define (toggle-blink!)
  (set-box! fake-now (+ (unbox fake-now) 10000.0))
  (parameterize ([current-blink-interval-ms 0])
    (apply-cursor-blink-timer! (unbox fake-now))))

;; Run a full render-frame! headlessly against a scripted screen size.
(define (render-frame-headless! ctx cols rows)
  (parameterize ([current-screen-size-provider (lambda () (values cols rows))])
    (tui-screen-size-cache-reset!)
    (parameterize ([current-output-port (open-output-string)])
      (render-frame! ctx))))

(define (blink-headless! ctx)
  (parameterize ([current-output-port (open-output-string)])
    (render-cursor-blink-frame! ctx)))

(define (fresh-render-ctx! cols rows #:input [inp (initial-input-state)])
  (define ctx (make-tui-ctx))
  (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer cols rows))
  (set-box! (tui-ctx-input-state-box ctx) inp)
  (reset-idle-render-state!)
  (render-frame-headless! ctx cols rows)
  ctx)

;; ─────────────────────────────────────────────────────────────────
;; 1. Both-axis position contract (read AND write, no aliasing)
;; ─────────────────────────────────────────────────────────────────
(test-case "read/write validate both axes independently (5x3 buffer)"
  (define buf (make-cell-buffer 5 3))
  ;; col violations: negative, ==cols, >cols — on several rows
  (check-both-axes-raise buf '((-1 0) (5 0) (6 0) (-1 2) (5 2) (6 2)))
  ;; row violations: negative, ==rows, >rows — on several cols
  (check-both-axes-raise buf '((0 -1) (0 3) (0 4) (4 -1) (4 3) (4 4)))
  ;; combined violations and far-out positions
  (check-both-axes-raise buf '((-1 -1) (5 3) (6 4) (100 100)))
  ;; valid corners stay accessible
  (check-not-exn (lambda () (cell-buffer-ref buf 0 0)))
  (check-not-exn (lambda () (cell-buffer-ref buf 4 2)))
  (check-not-exn (lambda () (cell-buffer-set! buf 4 2 #:char #\o))))

(test-case "violations carry coordinates and dimensions in the error"
  (define buf (make-cell-buffer 5 3))
  (define e
    (with-handlers ([exn:fail:cell-buffer-bounds? values])
      (cell-buffer-set! buf 5 0 #:char #\X)
      #f))
  (check-true (exn:fail:cell-buffer-bounds? e))
  (check-true (exn:fail:contract? e) "descends from exn:fail:contract for generic handlers")
  (check-equal? (exn:fail:cell-buffer-bounds-op e) 'cell-buffer-set!)
  (check-equal? (exn:fail:cell-buffer-bounds-col e) 5)
  (check-equal? (exn:fail:cell-buffer-bounds-row e) 0)
  (check-equal? (exn:fail:cell-buffer-bounds-cols e) 5)
  (check-equal? (exn:fail:cell-buffer-bounds-rows e) 3)
  (check-true (string-contains? (exn-message e) "col=5"))
  (check-true (string-contains? (exn-message e) "row=0"))
  (check-true (string-contains? (exn-message e) "cols=5, rows=3"))
  (check-true (string-contains? (exn-message e) "aliasing"))
  ;; row violations name the row axis
  (define e2 (with-handlers ([exn:fail:cell-buffer-bounds? values]) (cell-buffer-ref buf 0 3) #f))
  (check-true (string-contains? (exn-message e2) "row must be in [0,3)")))

(test-case "flattened-index collisions never alias: (5,0) is not (0,1)"
  (define buf (make-cell-buffer 5 3))
  (check-exn exn:fail:cell-buffer-bounds? (lambda () (cell-buffer-set! buf 5 0 #:char #\X)))
  ;; flat index 5 belongs to (0,1): it must be untouched
  (check-equal? (cell-char (cell-buffer-ref buf 0 1)) #\space)
  (check-exn exn:fail:cell-buffer-bounds? (lambda () (cell-buffer-ref buf 5 0)))
  ;; write through the VALID route and confirm (0,1) is independently writable
  (cell-buffer-set! buf 0 1 #:char #\Y)
  (check-equal? (cell-char (cell-buffer-ref buf 0 1)) #\Y))

(test-case "negative positions never alias: (-1,1) is not (4,0)"
  (define buf (make-cell-buffer 5 3))
  (cell-buffer-set! buf 4 0 #:char #\A)
  (check-exn exn:fail:cell-buffer-bounds? (lambda () (cell-buffer-set! buf -1 1 #:char #\Z)))
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\A)
  (check-equal? (cell-char (cell-buffer-ref buf 0 1)) #\space))

(test-case "span painting clips; negative dims rejected; zero-size allowed"
  (define buf (make-cell-buffer 5 3))
  ;; Span painting is CLIPPING by contract: a long/offset string paints the
  ;; part inside the buffer and never raises.
  (check-not-exn (lambda () (cell-buffer-putstring! buf 0 99 "overflow row")))
  (check-not-exn (lambda () (cell-buffer-putstring! buf 3 0 "abcdef")))
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\b)
  ;; make-cell-buffer rejects negative dims but allows zero-size surfaces
  (check-exn exn:fail:contract? (lambda () (make-cell-buffer -1 3)))
  (check-exn exn:fail:contract? (lambda () (make-cell-buffer 3 -1)))
  (define zero (make-cell-buffer 0 3))
  (check-equal? (cell-buffer-cols zero) 0)
  (check-true (cell-buffer? (cell-buffer-snapshot zero))))

;; ─────────────────────────────────────────────────────────────────
;; 2. Logical EOL insertion sentinel → physical projection
;; ─────────────────────────────────────────────────────────────────
(test-case "sentinel predicate"
  (check-true (logical-cursor-eol-sentinel? 5 5))
  (check-false (logical-cursor-eol-sentinel? 4 5))
  (check-false (logical-cursor-eol-sentinel? 0 5)))

(test-case "projection places the sentinel on the last visible cell"
  ;; sentinel col==width → end-of-line placement (width-1), any row
  (define-values (c1 r1) (project-logical-cursor 5 0 5 5 3))
  (check-equal? (list c1 r1) (list 4 0))
  ;; FINAL row: (W, H-1) → (W-1, H-1) — the historical overflow position
  (define-values (c2 r2) (project-logical-cursor 5 2 5 5 3))
  (check-equal? (list c2 r2) (list 4 2))
  ;; non-sentinel columns pass through unchanged
  (define-values (c3 r3) (project-logical-cursor 2 1 5 5 3))
  (check-equal? (list c3 r3) (list 2 1)))

(test-case "projection refuses rather than clamps"
  ;; (0,H) offset: row == rows → no physical cell
  (define-values (c1 r1) (project-logical-cursor 0 3 5 5 3))
  (check-false c1)
  (check-false r1)
  ;; zero-size surfaces
  (define-values (c2 r2) (project-logical-cursor 0 0 1 0 3))
  (check-false c2)
  (define-values (c3 r3) (project-logical-cursor 0 0 1 3 0))
  (check-false c3)
  ;; layout/buffer geometry inconsistent: logical column has no real cell
  (define-values (c4 r4) (project-logical-cursor 4 0 5 3 3))
  (check-false c4)
  ;; sentinel whose end-of-line cell would fall outside the actual buffer
  (define-values (c5 r5) (project-logical-cursor 5 0 5 3 3))
  (check-false c5)
  ;; out-of-range logical rows
  (define-values (c6 r6) (project-logical-cursor 0 -1 5 5 3))
  (check-false c6))

;; ─────────────────────────────────────────────────────────────────
;; 3. Exact-width composer buffer: logical semantics preserved
;; ─────────────────────────────────────────────────────────────────
(test-case "composer keeps the logical col==width boundary (width5/col5 pinned)"
  (define lay (compute-composer-layout "abcde" 5 5 unit-width))
  (check-equal? (composer-layout-row-count lay) 1)
  (check-equal? (composer-layout-cursor-col lay) 5)
  (check-equal? (composer-layout-width lay) 5)
  ;; the projection absorbs the sentinel; the layout itself is unchanged
  (define-values (c r) (project-logical-cursor 5 0 (composer-layout-width lay) 5 24))
  (check-equal? (list c r) (list 4 0)))

(test-case "frame path paints the q> prompt and projects the exact-width sentinel"
  ;; 8-col input region: prompt "q> " at cols 0-2, text area cols 3-7
  ;; (composer width 5). Exact-width input "abcde" cursor 5 = the logical
  ;; sentinel col==width → projected to the row's last visible cell.
  (define ubuf (make-cell-buffer 8 24))
  (define layout (compute-layout 24 8))
  (define-values (c r _s _f)
    (render-frame-vdom! ubuf (initial-ui-state)
                        (input-insert-string (initial-input-state) "abcde") layout))
  (check-equal? (list c r) (list 7 22))
  ;; prompt painted at the first composer text row
  (check-equal? (cell-char (cell-buffer-ref ubuf 0 22)) #\q)
  (check-equal? (cell-char (cell-buffer-ref ubuf 1 22)) #\>)
  (check-equal? (cell-char (cell-buffer-ref ubuf 2 22)) #\space)
  ;; the projected cell is real and holds the last typed char
  (check-equal? (cell-char (cell-buffer-ref ubuf c r)) #\e)
  ;; the projected cell must be usable by the render loop's save path
  (check-true (cell-buffer-in-bounds? ubuf c r))
  ;; empty-input cursor sits just after the prompt
  (define-values (c0 r0 _s0 _f0)
    (render-frame-vdom! ubuf (initial-ui-state) (initial-input-state) layout))
  (check-equal? (list c0 r0) (list 3 22)))

(test-case "frame path: sentinel on the FINAL visible row (W-1, H-1)"
  ;; 8x3 buffer, 8-col exact-width composer → cursor lands on row 2
  (define ubuf (make-cell-buffer 8 3))
  (define inp (input-insert-string (initial-input-state) "abcde"))
  (define layout (compute-layout 3 8))
  (define-values (c r _s _f) (render-frame-vdom! ubuf (initial-ui-state) inp layout))
  (check-equal? (list c r) (list 7 2))
  (check-equal? (cell-char (cell-buffer-ref ubuf c r)) #\e))

(test-case "frame path: multi-row wrap keeps logical offsets, projects cursor"
  (define ubuf (make-cell-buffer 8 24))
  (define layout (compute-layout 24 8))
  ;; cursor at END of "abcdefg": row 1 ("fg"), text col 2 — a plain visible
  ;; cell at buffer col 5 (prompt offset 3)
  (define-values (c2 r2 _s2 _f2)
    (render-frame-vdom! ubuf (initial-ui-state)
                        (input-insert-string (initial-input-state) "abcdefg") layout))
  (check-equal? (list c2 r2) (list 5 23))
  (check-true (cell-buffer-in-bounds? ubuf c2 r2))
  (check-equal? (cell-char (cell-buffer-ref ubuf c2 r2)) #\space)
  (check-equal? (cell-char (cell-buffer-ref ubuf 3 23)) #\f)
  ;; cursor exactly at the SOFT-WRAP BOUNDARY (offset 5 of "abcdef"):
  ;; logical sentinel col 5 == width on row 0 → projected to row 0's last
  ;; cell (buffer col 7), whose content is 'e'
  (define inp5
    (struct-copy input-state (input-insert-string (initial-input-state) "abcdef") [cursor 5]))
  (define-values (c1 r1 _s1 _f1) (render-frame-vdom! ubuf (initial-ui-state) inp5 layout))
  (check-equal? (list c1 r1) (list 7 22))
  (check-equal? (cell-char (cell-buffer-ref ubuf c1 r1)) #\e))

;; ─────────────────────────────────────────────────────────────────
;; 4. Tiny surfaces: zero/one-cell geometry defers, never crashes
;; ─────────────────────────────────────────────────────────────────
(test-case "frame path: zero-size surfaces return explicit no-cursor"
  (ui-diagnostics-reset! (current-ui-diagnostics))
  (for ([dims (in-list '((0 24) (24 0) (0 0)))])
    (define ubuf (make-cell-buffer (car dims) (cadr dims)))
    (define inp (input-insert-string (initial-input-state) "abcde"))
    (define layout (compute-layout 24 5))
    (define-values (c r _s _f) (render-frame-vdom! ubuf (initial-ui-state) inp layout))
    (check-false c (format "cols=~a rows=~a must defer the cursor" (car dims) (cadr dims)))
    (check-false r)
    (check-true (> (diag-count 'renderer.cursor-deferred) 0))))

(test-case "frame path: 1x1 surface yields the single physical cell"
  (define ubuf (make-cell-buffer 1 1))
  (define layout (compute-layout 1 1))
  (define-values (c r _s _f)
    (render-frame-vdom! ubuf (initial-ui-state) (initial-input-state) layout))
  (check-equal? (list c r) (list 0 0))
  (check-not-exn (lambda () (cell-buffer-ref ubuf c r))))

(test-case "frame path: narrow surfaces hide the prompt; wrap uses full width"
  ;; width 5: prompt SHOWN (5 > 3), text area = 2 — "abcde" wraps to
  ;; "ab"/"cd"/"e"; end cursor (row 2, text col 1) → buffer col 3+1 = 4
  (define ubuf (make-cell-buffer 5 24))
  (define layout (compute-layout 24 5))
  (define-values (c r _s _f)
    (render-frame-vdom! ubuf (initial-ui-state)
                        (input-insert-string (initial-input-state) "abcde") layout))
  (check-equal? (list c r) (list 4 23))
  (check-equal? (cell-char (cell-buffer-ref ubuf 0 22)) #\q)
  (check-equal? (cell-char (cell-buffer-ref ubuf 3 23)) #\e)
  (check-true (cell-buffer-in-bounds? ubuf c r))
  ;; width 3: prompt HIDDEN (3 ≤ 3), text area = full width — sentinel
  ;; col 3 of "abc" projects to the last buffer column
  (define ubuf3 (make-cell-buffer 3 24))
  (define layout3 (compute-layout 24 3))
  (define-values (c3 r3 _s3 _f3)
    (render-frame-vdom! ubuf3 (initial-ui-state)
                        (input-insert-string (initial-input-state) "abc") layout3))
  (check-equal? (list c3 r3) (list 2 22))
  (check-equal? (cell-char (cell-buffer-ref ubuf3 0 22)) #\a "no prompt at width 3")
  (check-equal? (cell-char (cell-buffer-ref ubuf3 c3 r3)) #\c))

(test-case "composer-text-area-width matches the frame contract"
  (check-equal? (composer-text-area-width 8) 5)
  (check-equal? (composer-text-area-width 5) 2)
  (check-equal? (composer-text-area-width 3) 3)
  (check-equal? (composer-text-area-width 0) 1))

;; ─────────────────────────────────────────────────────────────────
;; 5. Render loop: geometry snapshot, blink exactness, invalidation
;; ─────────────────────────────────────────────────────────────────
(test-case "render-frame! stamps cursor caches with generation + physical dims"
  (invalidate-cursor-caches!)
  (define ctx (fresh-render-ctx! 10 3))
  (define ubuf (unbox (tui-ctx-ubuf-box ctx)))
  (check-equal? (unbox last-cursor-buffer-gen-box) (unbox tui-buffer-generation-box))
  (check-equal? (unbox last-cursor-buffer-cols-box) 10)
  (check-equal? (unbox last-cursor-buffer-rows-box) 3)
  ;; the guard accepts exactly in-bounds cells of the CURRENT geometry
  (check-true (cursor-cache-valid-for? ubuf 0 2))
  (check-false (cursor-cache-valid-for? ubuf 10 2))
  (check-false (cursor-cache-valid-for? ubuf 2 3))
  (check-false (cursor-cache-valid-for? ubuf -1 2)))

(test-case "blink save/restore is exact through the guarded path"
  ;; cursor sits ON a painted char: "hi" then cursor-left onto 'i'
  (define inp (input-cursor-left (input-insert-string (initial-input-state) "hi")))
  (define ctx (fresh-render-ctx! 10 3 #:input inp))
  (define ubuf (unbox (tui-ctx-ubuf-box ctx)))
  (define base (vector #\i 7 0 #f #f #f #f))
  ;; phase is ON after render-frame! → cursor cell (4,2) inverted
  ;; (prompt "q> " occupies cols 0-2; cursor sits on 'i' at col 3+1)
  (check-equal? (cell-char (cell-buffer-ref ubuf 3 2)) #\h)
  (check-equal? (cell-char (cell-buffer-ref ubuf 4 2)) #\i)
  (check-equal? (cell-fg (cell-buffer-ref ubuf 4 2)) 0)
  (check-equal? (cell-bg (cell-buffer-ref ubuf 4 2)) 7)
  ;; blink OFF → EXACT restore of the base cell
  (toggle-blink!)
  (check-true (blink-headless! ctx))
  (check-equal? (cell-buffer-ref ubuf 4 2) base "restore must reproduce the base cell exactly")
  ;; blink ON again → exact inversion
  (toggle-blink!)
  (check-true (blink-headless! ctx))
  (check-equal? (cell-buffer-ref ubuf 4 2) (vector #\i 0 7 #f #f #f #f))
  ;; and OFF once more — idempotent exactness
  (toggle-blink!)
  (blink-headless! ctx)
  (check-equal? (cell-buffer-ref ubuf 4 2) base))

(test-case "resize lifecycle: generation bump clears caches; blink defers then reconciles"
  (define ctx (fresh-render-ctx! 10 3))
  (define gen-before (unbox tui-buffer-generation-box))
  (parameterize ([current-screen-size-provider (lambda () (values 12 4))])
    (tui-screen-size-cache-reset!)
    ;; the resize path: new buffer + generation bump + cache invalidation
    (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer 12 4))
    (bump-tui-buffer-generation!)
    (invalidate-cursor-caches!))
  (check-equal? (unbox tui-buffer-generation-box) (add1 gen-before))
  (check-false (unbox last-cursor-buffer-gen-box) "cursor caches cleared on resize")
  ;; a blink now defers: no cached cursor → full-redraw request, no write
  (define new-ubuf (unbox (tui-ctx-ubuf-box ctx)))
  (define stale-cell (cell-buffer-ref new-ubuf 0 0))
  (toggle-blink!)
  (check-false (blink-headless! ctx))
  (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "deferred blink requests reconcile/redraw")
  (check-equal? (cell-buffer-ref new-ubuf 0 0) stale-cell "no cell was written from a stale cache")
  ;; the next full frame re-stamps against the NEW geometry and blink works
  (render-frame-headless! ctx 12 4)
  (check-equal? (unbox last-cursor-buffer-cols-box) 12)
  (check-equal? (unbox last-cursor-buffer-rows-box) 4)
  (toggle-blink!)
  (check-true (blink-headless! ctx)))

(test-case "stale physical dims alone invalidate the cache (bounded diagnostic, no write)"
  (ui-diagnostics-reset! (current-ui-diagnostics))
  (define ctx (fresh-render-ctx! 10 3))
  (define ubuf (unbox (tui-ctx-ubuf-box ctx)))
  (check-true (cursor-cache-valid-for? ubuf 0 2))
  ;; simulate physical-dims drift without a generation bump
  (set-box! last-cursor-buffer-rows-box 99)
  (check-false (cursor-cache-valid-for? ubuf 0 2))
  ;; a blink in this state refuses, is explicitly diagnosed, and never writes
  (define before (cell-buffer-ref ubuf 0 2))
  (define count-before (diag-count 'renderer.cursor-cache-invalidated))
  (toggle-blink!)
  (check-false (blink-headless! ctx))
  (check-true (> (diag-count 'renderer.cursor-cache-invalidated) count-before)
              "stale-cache deferral is explicitly diagnosed")
  (check-equal? (cell-buffer-ref ubuf 0 2) before))

(test-case "render-frame! defers on zero-size / negative screen geometry"
  (ui-diagnostics-reset! (current-ui-diagnostics))
  (invalidate-cursor-caches!)
  (define ctx (make-tui-ctx))
  (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer 10 3))
  (for ([dims (in-list '((0 0) (0 24) (24 0) (-1 24) (24 -1)))])
    (define before-frame (diag-count 'renderer.frame-deferred))
    (check-not-exn
     (lambda () (render-frame-headless! ctx (car dims) (cadr dims)))
     (format "deferred frame for ~ax~a must not raise" (car dims) (cadr dims)))
    (check-true (> (diag-count 'renderer.frame-deferred) before-frame)
                (format "deferral of ~ax~a is explicitly diagnosed" (car dims) (cadr dims)))
    (check-false (unbox last-cursor-buffer-gen-box) "no cursor cached from a deferred frame")))

(test-case "render-frame! reconciles stale buffer geometry before painting"
  (ui-diagnostics-reset! (current-ui-diagnostics))
  (define ctx (fresh-render-ctx! 10 3))
  ;; change the screen size without delivering a resize message first
  (render-frame-headless! ctx 12 4)
  (check-true (> (diag-count 'renderer.buffer-reconciled) 0)
              "geometry mismatch is explicitly diagnosed")
  (define ubuf (unbox (tui-ctx-ubuf-box ctx)))
  (check-equal? (cell-buffer-cols ubuf) 12)
  (check-equal? (cell-buffer-rows ubuf) 4)
  (check-equal? (unbox last-cursor-buffer-cols-box) 12)
  (check-equal? (unbox last-cursor-buffer-rows-box) 4))
