#lang racket/base

;; runtime/context-assembly/task-checkpoint.rkt
;; STABILITY: evolving
;;
;; W4 (#8941): Bounded active-task checkpoint — renders an
;; active-task-checkpoint (from runtime/task-memory/projection.rkt) into a
;; fixed-budget protocol-safe text record suitable for injection into the
;; provider context as a system message.
;;
;; Security posture (per W4 issue):
;;   - Fixed-budget: rendering truncates to a token cap; overflow is flagged
;;   - Structured: fields rendered in priority order (objective first)
;;   - References evidence (paths/shas), never copies raw tool output
;;   - Protocol-safe: wraps as a system-role message for injection before
;;     the selected conversation history
;;
;; D9 acceptance: provider context must contain objective, owned paths,
;; passing tests, broad-gate state, blockers, next action — derived from
;; runtime-observed events, not voluntary record_conclusion.
;;
;; Layering: CONTEXT-ASSEMBLY module. Imports projection (runtime/task-memory)
;; for the active-task-checkpoint struct. This is the rendering/budgeting layer.

(require racket/contract
         racket/list
         racket/match
         racket/string
         "../task-memory/projection.rkt")

(provide task-checkpoint-record?
         task-checkpoint-record-text
         task-checkpoint-record-token-count
         task-checkpoint-record-overflow?
         render-task-checkpoint
         estimate-tokens
         task-checkpoint->message
         DEFAULT-CHECKPOINT-TOKEN-BUDGET)

;; ============================================================
;; Constants
;; ============================================================

;; Default token budget for the checkpoint record.
;; Generous enough for a rich checkpoint, bounded enough to never dominate.
(define DEFAULT-CHECKPOINT-TOKEN-BUDGET 800)

;; Approximate chars-per-token for budget estimation.
(define CHARS-PER-TOKEN 4)

;; ============================================================
;; Token estimation
;; ============================================================

(define (estimate-tokens s)
  (quotient (max (string-length s) 0) CHARS-PER-TOKEN))

;; ============================================================
;; Result struct
;; ============================================================

(struct task-checkpoint-record (text token-count overflow?) #:transparent)

;; ============================================================
;; Section rendering (priority order)
;; ============================================================

;; Each section is (label . lines). Empty sections are dropped.
(struct section (label lines) #:transparent)

(define (section-non-empty? s)
  (not (null? (section-lines s))))

(define (objective-section cp)
  (define obj (active-task-checkpoint-objective cp))
  (if obj
      (section "Objective" (list obj))
      (section "Objective" '())))

(define (phase-section cp)
  (define ph (active-task-checkpoint-current-phase cp))
  (if ph
      (section "Current phase" (list ph))
      (section "Current phase" '())))

(define (verification-section cp)
  (define vs (active-task-checkpoint-verification-state cp))
  (section "Verification state" (list (symbol->string vs))))

(define (owned-paths-section cp)
  (define paths (active-task-checkpoint-owned-paths cp))
  (if (null? paths)
      (section "Owned artifacts" '())
      (section "Owned artifacts" paths)))

(define (completed-work-section cp)
  (define work (active-task-checkpoint-completed-work cp))
  (if (null? work)
      (section "Completed work" '())
      (section "Completed work"
               (for/list ([w (in-list work)])
                 (define ref (or (work-evidence-ref w) ""))
                 (define summ (or (work-evidence-summary w) ""))
                 (string-append ref " " summ)))))

(define (blockers-section cp)
  (define bl (active-task-checkpoint-blockers cp))
  (if (null? bl)
      (section "Blockers" '())
      (section "Blockers"
               (for/list ([b (in-list bl)])
                 (define msg (or (blocker-message b) ""))
                 (define src (or (blocker-source b) ""))
                 (if (string=? src "")
                     msg
                     (string-append src ": " msg))))))

(define (next-action-section cp)
  (define na (active-task-checkpoint-next-action cp))
  (if na
      (section "Next action" (list na))
      (section "Next action" '())))

(define (constraints-section cp)
  (define cs (active-task-checkpoint-constraints cp))
  (if (null? cs)
      (section "Constraints" '())
      (section "Constraints" cs)))

;; All sections in priority order.
(define (all-sections cp)
  (list (objective-section cp)
        (phase-section cp)
        (verification-section cp)
        (blockers-section cp)
        (owned-paths-section cp)
        (completed-work-section cp)
        (constraints-section cp)
        (next-action-section cp)))

;; ============================================================
;; Rendering
;; ============================================================

;; Render sections to a single text block. Sections are labeled and
;; their lines indented. Truncation is applied AFTER full render if needed.
(define (render-sections-text sections)
  (string-join (for/list ([s (in-list sections)]
                          #:when (section-non-empty? s))
                 (string-append (section-label s)
                                ":\n"
                                (string-join (for/list ([ln (in-list (section-lines s))])
                                               (string-append "  " ln))
                                             "\n")))
               "\n\n"))

;; Truncate text to fit within a token budget. Returns truncated text.
(define (truncate-to-budget text budget)
  (define max-chars (* budget CHARS-PER-TOKEN))
  (if (<= (string-length text) max-chars)
      text
      (string-append (substring text 0 (max 0 (- max-chars 3))) "...")))

;; Render an active-task-checkpoint to a bounded text record.
(define (render-task-checkpoint cp #:token-budget [token-budget DEFAULT-CHECKPOINT-TOKEN-BUDGET])
  (define sections (all-sections cp))
  (define full-text (render-sections-text sections))
  (define truncated (truncate-to-budget full-text token-budget))
  (define tokens (estimate-tokens truncated))
  (define overflow? (> (estimate-tokens full-text) token-budget))
  (task-checkpoint-record truncated tokens overflow?))

;; ============================================================
;; Protocol-safe message
;; ============================================================

;; Wrap the checkpoint as a system-role message hash suitable for injection
;; before the selected conversation history. Returns a hash with 'role and
;; 'content keys (jsexpr-compatible).
(define (task-checkpoint->message cp #:token-budget [token-budget DEFAULT-CHECKPOINT-TOKEN-BUDGET])
  (define rec (render-task-checkpoint cp #:token-budget token-budget))
  (hasheq 'role
          "system"
          'content
          (string-append "# Active task checkpoint\n\n" (task-checkpoint-record-text rec))))
