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
         (contract-out
          [make-empty-disclosure-state (-> disclosure-state?)]
          [disclosure-toggle (-> disclosure-state? any/c disclosure-state?)]
          [disclosure-set (-> disclosure-state? any/c boolean? disclosure-state?)]
          [disclosure-expanded? (-> disclosure-state? any/c boolean?)]
          [resolve-toggle-target
           (->* (disclosure-state?)
                (any/c any/c (listof any/c))
                any/c)]
          [make-collapsed-preview (-> string? exact-nonnegative-integer? exact-nonnegative-integer? string?)]
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
;; 1. Explicit focused artifact id (if provided and not #f).
;; 2. Currently active/streaming reasoning artifact id.
;;    Use `active-streaming-artifact-id` as the stable synthetic id for the
;;    in-flight reasoning stream — BOTH the renderer (synthetic transcript
;;    entry) and the key dispatcher MUST use this same sentinel so toggle
;;    state matches the rendered entry.
;; 3. Most recently completed reasoning artifact id from the candidate list.
;;    Candidate ids are given oldest-first (chronological); the LAST valid
;;    (non-#f, non-empty) candidate is the most recently completed.
;; Returns #f if no candidate matches.
(define active-streaming-artifact-id 'streaming-thinking)

(define (resolve-toggle-target state
                               [focused-id #f]
                               [active-reasoning-id #f]
                               [candidate-ids '()])
  (or (and focused-id (not (equal? focused-id "")) focused-id)
      active-reasoning-id
      (and (pair? candidate-ids)
           (findf (lambda (id) (and id (not (equal? id ""))))
                  (reverse candidate-ids)))
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
  (cond
    [(and first-line (> hidden 0))
     (format "~a · ~a lines · Show ~a more · Ctrl+O to expand"
             (string-trim first-line)
             total
             hidden)]
    [first-line
     (format "~a · ~a line~a"
             (string-trim first-line)
             total
             (if (= total 1) "" "s"))]
    [else
     (format "Thinking · ~a line~a · Ctrl+O to expand"
             total
             (if (= total 1) "" "s"))]))

(define (first-non-empty-line body)
  (define lines (string-split body "\n" #:repeat? #f))
  (findf (lambda (line) (not (string=? (string-trim line) ""))) lines))

(define (neutral-detail-label)
  "Thinking · no detail available")
