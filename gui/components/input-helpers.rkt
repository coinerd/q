#lang racket/base

;; q/gui/components/input-helpers.rkt — Pure multiline input helpers
;;
;; Headless-testable key handling and input validation for the GUI.
;;
;; W4 (v0.99.96): the GUI contract (Enter submits; Shift/Ctrl+Enter inserts
;; newline) is now routed through the SHARED frontend-neutral resolution in
;; ui-core/preferences.rkt and produces the same named intents the TUI uses
;; (ui-core/ui-intents.rkt).  Draft state is stored in the shared composer
;; model (ui-core/composer-model.rkt) so saved drafts, history intent, and
;; submit/newline semantics are identical across frontends.

(require racket/contract
         racket/list
         racket/string
         "markdown-parser.rkt"
         "../../ui-core/preferences.rkt"
         "../../ui-core/ui-intents.rkt"
         "../../ui-core/composer-model.rkt")

(provide (contract-out
          [input-key-should-submit? (-> any/c boolean? boolean? boolean?)]
          [prepare-input-for-submit (-> string? string?)]
          [input-line-count (-> string? exact-nonnegative-integer?)]
          [input-looks-like-code? (-> string? boolean?)]
          ;; ── W4: shared intent resolution ──────────────────────
          [gui-key->intent
           (->* (any/c)
                (#:shift? boolean?
                          #:control? boolean?
                          #:alt? boolean?
                          #:at-start? (or/c boolean? 'no)
                          #:at-end? (or/c boolean? 'no)
                          #:prefs preferences?)
                (or/c ui-intent? #f))]
          ;; ── W4: shared draft adapter over composer-model ──────
          [make-gui-draft (->* () ((and/c string? (length>= 0))) composer-state?)]
          [gui-draft-update (-> composer-state? string? composer-state?)]
          [gui-draft-text (-> composer-state? string?)]
          [gui-draft-insert-newline (-> composer-state? composer-state?)]
          [gui-draft-submit (-> composer-state? (values string? composer-state?))]
          [gui-draft-history
           (-> composer-state?
               exact-nonnegative-integer?
               (listof string?)
               (or/c 'up 'down)
               (values composer-state? exact-nonnegative-integer? (or/c string? #f)))]
          [make-history-list (->* () ((listof string?)) (listof string?))]
          [history-previous (-> (listof string?) exact-nonnegative-integer? (or/c string? #f))]
          [history-next (-> (listof string?) exact-nonnegative-integer? (or/c string? #f))]
          [history-index-back (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
          [history-index-forward
           (-> exact-nonnegative-integer? (listof string?) exact-nonnegative-integer?)]))

(define-syntax-rule (length>= n)
  any/c)

;; Should this key event trigger submit?
;; Enter without Shift/Control → submit
;; Shift+Enter or Control+Enter → insert newline
;;
;; W4: delegates to the shared preference resolution so GUI and TUI can
;; never disagree.  Kept as a boolean predicate for legacy callers/tests.
(define (input-key-should-submit? key-code shift? control?)
  (eq?
   'ui.composer.submit
   (resolve-key->intent key-code #:shift? shift? #:control? control? #:prefs (default-preferences))))

;; Process input text: trim trailing whitespace for submission
(define (prepare-input-for-submit text)
  (string-trim text #:left? #f))

;; Split input into lines for validation
(define (input-line-count text)
  (length (string-split text "\n")))

;; Check if input appears to be a code block (for auto-detection)
(define (input-looks-like-code? text)
  (or (contains-code-blocks? text)
      (ormap (lambda (pat) (string-contains? text pat))
             (list "(define " "(let " "(lambda " "(if " "(cond " "(for " "(when " "(set! "))))

;; ── W4: shared key → named intent ───────────────────────────
;; Normalizes a GUI key event into the SAME intent struct the TUI builds.
(define (gui-key->intent key-code
                         #:shift? [shift? #f]
                         #:control? [control? #f]
                         #:alt? [alt? #f]
                         #:at-start? [at-start? #f]
                         #:at-end? [at-end? #f]
                         #:prefs [prefs (default-preferences)])
  (define kind
    (resolve-key->intent key-code
                         #:shift? shift?
                         #:control? control?
                         #:alt? alt?
                         #:at-start? at-start?
                         #:at-end? at-end?
                         #:prefs prefs))
  (case kind
    [(ui.composer.submit) (make-composer-submit-intent)]
    [(ui.composer.insert-newline) (make-composer-newline-intent)]
    [(composer.history-up) (make-composer-history-intent 'up)]
    [(composer.history-down) (make-composer-history-intent 'down)]
    [(ui.transcript.toggle-detail) (make-toggle-detail-intent)]
    [else #f]))

;; ── W4: shared draft adapter over the composer model ────────
;; Persistent draft state lives in the SHARED composer model so the saved
;; draft text is byte-identical between frontends.

(define (make-gui-draft [initial ""])
  (composer-load-text (make-composer-state) initial))

(define (gui-draft-update st text)
  ;; Whole-field replacement (native on-change callback semantics):
  ;; load the text, then park the cursor at the end of the buffer.
  (composer-set-cursor (composer-load-text st text) (string-length (if (string? text) text ""))))

(define (gui-draft-text st)
  (composer-state-buffer st))

(define (gui-draft-insert-newline st)
  (composer-insert-string st "\n"))

;; Submit: returns the prepared text and a cleared draft.
(define (gui-draft-submit st)
  (values (prepare-input-for-submit (composer-state-buffer st)) (make-composer-state)))

;; History walk shared with the TUI: 'up loads the previous entry,
;; 'down the next one.  Returns the new state and the text to load
;; (or #f when the boundary is hit and the buffer should stay put).
(define (gui-draft-history st idx history direction)
  (define intent (if (eq? direction 'up) 'up 'down))
  (define entry
    (if (eq? direction 'up)
        (history-previous history idx)
        (history-next history idx)))
  (cond
    [(not entry) (values (composer-set-history-intent st intent) idx #f)]
    [else
     (define idx*
       (if (eq? direction 'up)
           (history-index-back idx)
           (history-index-forward idx history)))
     (values (composer-set-history-intent (composer-load-text (make-composer-state) entry) intent)
             idx*
             entry)]))

;; History list is oldest-first; index counts DOWN from (length history)
;; (= live buffer) to 0 (= oldest entry).
(define (make-history-list [seed '()])
  (if (list? seed)
      (reverse (remove-duplicates (reverse seed)))
      '()))

(define (history-previous history idx)
  (and (> idx 0) (< idx (add1 (length history))) (list-ref history (sub1 idx))))

(define (history-next history idx)
  (and (< idx (sub1 (length history))) (list-ref history (add1 idx))))

(define (history-index-back idx)
  (max 0 (sub1 idx)))

(define (history-index-forward idx history)
  (min (sub1 (length history)) (add1 idx)))

(module+ test
  (require rackunit)
  ;; Legacy contract preserved
  (check-true (input-key-should-submit? 'return #f #f))
  (check-false (input-key-should-submit? 'return #t #f))
  (check-false (input-key-should-submit? 'return #f #t))
  (check-false (input-key-should-submit? #\a #f #f))
  (check-equal? (prepare-input-for-submit "hello  \n") "hello")
  (check-equal? (input-line-count "a\nb\nc") 3)
  (check-true (input-looks-like-code? "(define x 1)"))
  (check-false (input-looks-like-code? "plain text"))

  ;; W4: intent resolution parity
  (check-equal? (ui-intent-kind (gui-key->intent 'return)) 'ui.composer.submit)
  (check-equal? (ui-intent-kind (gui-key->intent 'return #:shift? #t)) 'ui.composer.insert-newline)
  (check-equal? (ui-intent-kind (gui-key->intent 'return #:control? #t)) 'ui.composer.insert-newline)
  (check-equal? (ui-intent-kind (gui-key->intent 'up #:at-start? #t)) 'composer.history-up)
  (check-equal? (ui-intent-kind (gui-key->intent 'down #:at-end? #t)) 'composer.history-down)
  (check-equal? (ui-intent-kind (gui-key->intent #\o #:control? #t)) 'ui.transcript.toggle-detail)
  (check-false (gui-key->intent #\j))

  ;; W4: draft adapter
  (define d0 (make-gui-draft))
  (define d1 (gui-draft-update d0 "line one"))
  (define d2 (gui-draft-insert-newline d1))
  (define d3 (gui-draft-update d2 (string-append (gui-draft-text d2) "line two")))
  (check-equal? (gui-draft-text d3) "line one\nline two")
  (define-values (text cleared) (gui-draft-submit d3))
  (check-equal? text "line one\nline two")
  (check-equal? (gui-draft-text cleared) "")

  ;; W4: history walk
  (define hist (make-history-list (list "first" "second" "third")))
  (check-equal? hist (list "first" "second" "third"))
  (define hi0 (length hist))
  (define-values (h1 i1 t1) (gui-draft-history d0 hi0 hist 'up))
  (check-equal? t1 "third")
  (check-equal? (gui-draft-text h1) "third")
  (define-values (h2 i2 t2) (gui-draft-history h1 i1 hist 'up))
  (check-equal? t2 "second")
  (define-values (h3 i3 t3) (gui-draft-history h2 i2 hist 'down))
  (check-equal? t3 "third")
  ;; boundary: 'up past oldest returns #f and keeps buffer
  (define-values (hb ib tb) (gui-draft-history (make-composer-state) 0 hist 'up))
  (check-false tb)
  (define-values (hd id td) (gui-draft-history d0 (length '()) '() 'up))
  (check-false td)
  ;; semantic intent recorded, shared with TUI
  (check-equal? (composer-state-history-intent h1) 'up)
  (check-equal? (composer-state-history-intent h3) 'down))
