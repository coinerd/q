#lang racket/base

;; q/ui-core/ui-intents.rkt — named semantic UI intents
;;
;; Pure data symbols/structs shared by TUI and GUI frontends.
;; No terminal key parsing, no GUI widget code, no event bus.
;;
;; W4 (v0.99.96): composer intents added so BOTH frontends resolve the
;; same keyboard shortcuts to the same named semantic actions:
;;   ui.composer.submit         — submit the composer buffer
;;   ui.composer.insert-newline — insert a newline into the buffer
;;   composer.history-up/down   — walk the prompt history
;;   ui.transcript.toggle-detail— fold/unfold a reasoning artifact

(require racket/contract)

(provide (struct-out toggle-detail-intent)
         (struct-out composer-submit-intent)
         (struct-out composer-newline-intent)
         (struct-out composer-history-intent)
         (contract-out
          [ui-intent? (-> any/c boolean?)]
          [make-toggle-detail-intent (->* () ((or/c string? symbol? #f)) ui-intent?)]
          [make-composer-submit-intent (->* () (string?) ui-intent?)]
          [make-composer-newline-intent (-> ui-intent?)]
          [make-composer-history-intent (-> (or/c 'up 'down) ui-intent?)]
          [ui-intent-target (-> ui-intent? (or/c string? symbol? #f))]
          [ui-intent-kind (-> ui-intent? symbol?)]
          [ui-intent-text (-> ui-intent? (or/c string? #f))]
          [composer-intent? (-> any/c boolean?)]))

;; A toggle-detail intent targets a specific artifact (by id), or #f for auto-resolve.
(struct toggle-detail-intent (target) #:transparent)

;; Submit the composer.  `text` is the (already prepared) buffer snapshot;
;; empty string means "read the live buffer" for frontends that own the text.
(struct composer-submit-intent (text) #:transparent)

;; Insert a newline at the composer cursor.
(struct composer-newline-intent () #:transparent)

;; Walk prompt history: 'up = previous entry, 'down = next entry.
(struct composer-history-intent (direction) #:transparent)

(define (ui-intent? v)
  (or (toggle-detail-intent? v)
      (composer-submit-intent? v)
      (composer-newline-intent? v)
      (composer-history-intent? v)))

(define (composer-intent? v)
  (or (composer-submit-intent? v)
      (composer-newline-intent? v)
      (composer-history-intent? v)))

(define (make-toggle-detail-intent [target #f])
  (toggle-detail-intent target))

(define (make-composer-submit-intent [text ""])
  (composer-submit-intent (if (string? text) text "")))

(define (make-composer-newline-intent)
  (composer-newline-intent))

(define (make-composer-history-intent direction)
  (composer-history-intent (if (eq? direction 'down) 'down 'up)))

(define (ui-intent-target i)
  (cond
    [(toggle-detail-intent? i) (toggle-detail-intent-target i)]
    [else #f]))

(define (ui-intent-kind i)
  (cond
    [(toggle-detail-intent? i) 'ui.transcript.toggle-detail]
    [(composer-submit-intent? i) 'ui.composer.submit]
    [(composer-newline-intent? i) 'ui.composer.insert-newline]
    [(composer-history-intent? i)
     (case (composer-history-intent-direction i)
       [(down) 'composer.history-down]
       [else 'composer.history-up])]
    [else 'unknown]))

(define (ui-intent-text i)
  (cond
    [(composer-submit-intent? i) (composer-submit-intent-text i)]
    [else #f]))

(module+ test
  (require rackunit)
  (check-equal? (ui-intent-kind (make-toggle-detail-intent)) 'ui.transcript.toggle-detail)
  (check-equal? (ui-intent-kind (make-composer-submit-intent "hi")) 'ui.composer.submit)
  (check-equal? (ui-intent-text (make-composer-submit-intent "hi")) "hi")
  (check-equal? (ui-intent-kind (make-composer-newline-intent)) 'ui.composer.insert-newline)
  (check-equal? (ui-intent-kind (make-composer-history-intent 'up)) 'composer.history-up)
  (check-equal? (ui-intent-kind (make-composer-history-intent 'down)) 'composer.history-down)
  (check-true (composer-intent? (make-composer-history-intent 'up)))
  (check-false (composer-intent? (make-toggle-detail-intent)))
  (check-true (ui-intent? (make-composer-newline-intent))))
