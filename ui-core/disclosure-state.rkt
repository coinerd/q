#lang racket/base

;; q/ui-core/disclosure-state.rkt — disclosure/collapsed presentation state
;;
;; Pure module. No terminal key parsing, no mouse widgets, no event bus.
;; Computes per-artifact collapsed/expanded state and preview line descriptors.

(require racket/contract
         racket/string
         racket/list
         racket/set)

(provide (struct-out disclosure-state)
         active-streaming-artifact-id
         (contract-out [make-empty-disclosure-state (-> disclosure-state?)]
                       [disclosure-toggle (-> disclosure-state? any/c disclosure-state?)]
                       [disclosure-set (-> disclosure-state? any/c boolean? disclosure-state?)]
                       [disclosure-expanded? (-> disclosure-state? any/c boolean?)]
                       [resolve-toggle-target
                        (->* (disclosure-state?) (any/c any/c (listof any/c)) any/c)]
                       [make-collapsed-preview
                        (-> string? exact-nonnegative-integer? exact-nonnegative-integer? string?)]
                       [first-non-empty-line (-> string? (or/c string? #f))]
                       [neutral-detail-label (-> string?)]))

;; Disclosure state: set of artifact IDs currently expanded (default collapsed).
(struct disclosure-state (expanded-set) #:transparent)

(define (make-empty-disclosure-state)
  (disclosure-state (set)))

(define (disclosure-toggle state id)
  (define s (disclosure-state-expanded-set state))
  (disclosure-state (if (set-member? s id)
                        (set-remove s id)
                        (set-add s id))))

(define (disclosure-set state id expanded?)
  (define s (disclosure-state-expanded-set state))
  (disclosure-state (if expanded?
                        (set-add s id)
                        (set-remove s id))))

(define (disclosure-expanded? state id)
  (set-member? (disclosure-state-expanded-set state) id))

;; Default target selection order:
;; 1. Explicit focused artifact id, but only when it resolves to one of the
;;    canonical artifact ids supplied by the caller. Component focus ids must
;;    never leak into disclosure state.
;; 2. Currently active/streaming canonical reasoning artifact id.
;; 3. Most recently completed canonical reasoning artifact id from the
;;    candidate list (oldest-first).
;; Returns #f if no candidate matches.
;;
;; Kept as a compatibility export for older extensions. Core renderers no
;; longer use this synthetic value; live reasoning uses its canonical id.
(define active-streaming-artifact-id 'streaming-thinking)

(define (canonical-disclosure-id? id)
  (and (string? id) (not (string=? id ""))))

(define (resolve-toggle-target state [focused-id #f] [active-reasoning-id #f] [candidate-ids '()])
  (define candidates (filter canonical-disclosure-id? candidate-ids))
  (define active (and (canonical-disclosure-id? active-reasoning-id) active-reasoning-id))
  (or (and (canonical-disclosure-id? focused-id)
           (or (equal? focused-id active) (member focused-id candidates))
           focused-id)
      active
      (and (pair? candidates) (last candidates))
      #f))

;; Build a collapsed preview line for a full body.
;; preview-lines: number of lines to show (e.g., 3).
;; total-lines: total line count of the full body.
;; Returns a string like "Thinking · 42 lines · Ctrl+O to expand" or a snippet.
(define (make-collapsed-preview body preview-lines total-lines)
  (define total (max 0 total-lines))
  (define show (min total preview-lines))
  (define hidden (- total show))
  (define first-line (first-non-empty-line body))
  (define preview-line
    (and first-line
         (let ([trimmed (string-trim first-line)])
           (if (> (string-length trimmed) 120)
               (string-append (substring trimmed 0 117) "...")
               trimmed))))
  (cond
    [(and preview-line (> hidden 0))
     (format "~a · ~a lines · Show ~a more · Ctrl+O to expand" preview-line total hidden)]
    [preview-line
     (format "~a · ~a line~a · Show reasoning · Ctrl+O to expand"
             preview-line
             total
             (if (= total 1) "" "s"))]
    [else (format "Thinking · ~a line~a · Ctrl+O to expand" total (if (= total 1) "" "s"))]))

(define (first-non-empty-line body)
  (define lines (string-split body "\n" #:repeat? #f))
  (findf (lambda (line) (not (string=? (string-trim line) ""))) lines))

(define (neutral-detail-label)
  "Thinking · no detail available")
