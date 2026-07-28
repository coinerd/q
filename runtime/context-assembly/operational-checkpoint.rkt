#lang racket/base

;; runtime/context-assembly/operational-checkpoint.rkt
;; STABILITY: new
;;
;; R1 — Compact canonical operational coordinates injected every turn.
;;
;; The audit (session 01KYKMVEH4BW6EF0YSN1Z3EVVQ) found that the agent
;; repeatedly lost operational orientation: it discovered repo-root then
;; later reverted to outer workspace, it found correct planning files then
;; searched wrong paths. This module provides a highest-authority checkpoint
;; that is injected ahead of memory and working-set entries.
;;
;; The checkpoint is a compact struct that fits in ~256 tokens.

(require racket/contract
         racket/match
         racket/string)

;; ────────────────────────────────────────────────────────────
;; Operational Checkpoint struct
;; ────────────────────────────────────────────────────────────

(struct operational-checkpoint
        (repo-root ; string: /home/user/src/q-agent/q
         planning-root ; string: /home/user/src/q-agent/.planning
         active-milestone ; (or/c string? #f): "v0.99.73"
         active-wave ; (or/c string? #f): "W9"
         dirty-tree-files ; (listof string): files to preserve
         last-action ; (or/c string? #f): last successful tool + args
         next-action ; (or/c string? #f): next intended action
         last-error ; (or/c string? #f): last error class, if any
         error-count ; nonnegative-integer: count of consecutive errors
         )
  #:transparent)

;; ────────────────────────────────────────────────────────────
;; Default/empty checkpoint
;; ────────────────────────────────────────────────────────────

(define (make-empty-checkpoint)
  (operational-checkpoint "" "" #f #f '() #f #f #f 0))

;; ────────────────────────────────────────────────────────────
;; Compact text representation for context injection
;; ────────────────────────────────────────────────────────────

(define (checkpoint->text cp)
  (define root (operational-checkpoint-repo-root cp))
  (if (or (not root) (equal? root ""))
      ""
      (string-append
       "═══ Operational Checkpoint ═══\n"
       (format "repo-root:       ~a\n" root)
       (format "planning-root:   ~a\n" (operational-checkpoint-planning-root cp))
       (let ([ms (operational-checkpoint-active-milestone cp)])
         (if ms
             (format "milestone:       ~a\n" ms)
             ""))
       (let ([wv (operational-checkpoint-active-wave cp)])
         (if wv
             (format "wave:            ~a\n" wv)
             ""))
       (let ([dt (operational-checkpoint-dirty-tree-files cp)])
         (if (pair? dt)
             (format "dirty-files:     ~a\n" (string-join dt ", "))
             ""))
       (let ([la (operational-checkpoint-last-action cp)])
         (if la
             (format "last_action:     ~a\n" la)
             ""))
       (let ([na (operational-checkpoint-next-action cp)])
         (if na
             (format "next_action:     ~a\n" na)
             ""))
       (let ([le (operational-checkpoint-last-error cp)])
         (if le
             (format "last_error:      ~a (~a)\n" le (operational-checkpoint-error-count cp))
             ""))
       "═══════════════════════════════════════\n")))

;; ────────────────────────────────────────────────────────────
;; Checkpoint mutators (functional update)
;; ────────────────────────────────────────────────────────────

(define (checkpoint-set-repo-root cp root)
  (struct-copy operational-checkpoint cp [repo-root root] [last-error #f] [error-count 0]))

(define (checkpoint-set-planning-root cp root)
  (struct-copy operational-checkpoint cp [planning-root root]))

(define (checkpoint-set-milestone cp ms)
  (struct-copy operational-checkpoint cp [active-milestone ms]))

(define (checkpoint-set-wave cp wv)
  (struct-copy operational-checkpoint cp [active-wave wv]))

(define (checkpoint-set-last-action cp action)
  (struct-copy operational-checkpoint cp [last-action action]))

(define (checkpoint-set-next-action cp action)
  (struct-copy operational-checkpoint cp [next-action action]))

(define (checkpoint-set-error cp error-class)
  (struct-copy operational-checkpoint
               cp
               [last-error error-class]
               [error-count (add1 (operational-checkpoint-error-count cp))]))

(define (checkpoint-clear-error cp)
  (struct-copy operational-checkpoint cp [last-error #f] [error-count 0]))

(define (checkpoint-set-dirty-files cp files)
  (struct-copy operational-checkpoint cp [dirty-tree-files files]))

;; ────────────────────────────────────────────────────────────
;; Token estimation (conservative upper bound)
;; ────────────────────────────────────────────────────────────

(define (checkpoint-estimated-tokens cp)
  ;; ~4 chars/token for ASCII, maximum ~1,000 chars
  (let ([text (checkpoint->text cp)]) (quotient (string-length text) 4)))

;; ────────────────────────────────────────────────────────────
;; Supersession logic
;; ────────────────────────────────────────────────────────────

;; When a named milestone STATE file is read, any generic STATE.md
;; conclusion becomes stale. This function detects the contradiction.
(define (supercedes-generic-planning? path-name)
  ;; Is this a named milestone STATE file like "STATE-v0.99.73-ZERO-FAILING-TESTS.md"?
  (and (string? path-name)
       (or (regexp-match? #px"STATE-v[0-9]+\\.[0-9]+\\.[0-9]+" path-name)
           (regexp-match? #px"PLAN-v[0-9]+\\.[0-9]+\\.[0-9]+" path-name))))

(define (contradicts-generic-planning? generic-path named-path)
  ;; Does named-path contradict the generic planning artifact at generic-path?
  (and generic-path
       named-path
       (regexp-match? #px"(STATE|PLAN|VALIDATION)\\.md$" generic-path)
       (supercedes-generic-planning? named-path)))

;; ────────────────────────────────────────────────────────────
;; Checkpoint parameter (current session checkpoint)
;; ────────────────────────────────────────────────────────────

(define current-operational-checkpoint (make-parameter (make-empty-checkpoint)))

;; ────────────────────────────────────────────────────────────
;; Inject the checkpoint into a context message list
;; ────────────────────────────────────────────────────────────

(define (inject-checkpoint-message cp messages)
  ;; Prepend checkpoint as a system-originated message at the front
  (if (and cp
           (operational-checkpoint-repo-root cp)
           (not (equal? (operational-checkpoint-repo-root cp) "")))
      (let ([text (checkpoint->text cp)])
        (if (string=? text "")
            messages
            (cons (hash 'role
                        "system"
                        'content
                        (list (hash 'type "text" 'text text))
                        'kind
                        "checkpoint"
                        'id
                        "op-checkpoint")
                  messages)))
      messages))

(provide operational-checkpoint
         operational-checkpoint-repo-root
         operational-checkpoint-planning-root
         operational-checkpoint-active-milestone
         operational-checkpoint-active-wave
         operational-checkpoint-dirty-tree-files
         operational-checkpoint-last-action
         operational-checkpoint-next-action
         operational-checkpoint-last-error
         operational-checkpoint-error-count
         make-empty-checkpoint
         checkpoint->text
         checkpoint-set-repo-root
         checkpoint-set-planning-root
         checkpoint-set-milestone
         checkpoint-set-wave
         checkpoint-set-last-action
         checkpoint-set-next-action
         checkpoint-set-error
         checkpoint-clear-error
         checkpoint-set-dirty-files
         checkpoint-estimated-tokens
         supercedes-generic-planning?
         contradicts-generic-planning?
         current-operational-checkpoint
         inject-checkpoint-message)
