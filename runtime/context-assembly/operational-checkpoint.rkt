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
         racket/path
         racket/string
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/message/message.rkt" make-message message? message-id))

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
         planning-authority ; latest successfully-read named planning artifact
         )
  #:transparent)

;; ────────────────────────────────────────────────────────────
;; Default/empty checkpoint
;; ────────────────────────────────────────────────────────────

(define (make-empty-checkpoint)
  (operational-checkpoint "" "" #f #f '() #f #f #f 0 #f))

;; ────────────────────────────────────────────────────────────
;; Compact text representation for context injection
;; ────────────────────────────────────────────────────────────

(define MAX-CHECKPOINT-CHARS (* 512 4))

(define (checkpoint->text cp)
  (define root (operational-checkpoint-repo-root cp))
  (define raw
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
         (let ([authority (operational-checkpoint-planning-authority cp)])
           (if authority
               (format "planning-authority: ~a\n" authority)
               ""))
         "═══════════════════════════════════════\n")))
  (if (> (string-length raw) MAX-CHECKPOINT-CHARS)
      (substring raw 0 MAX-CHECKPOINT-CHARS)
      raw))

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

(define (checkpoint-set-planning-authority cp path)
  (struct-copy operational-checkpoint cp [planning-authority path]))

;; ────────────────────────────────────────────────────────────
;; Token estimation (conservative upper bound)
;; ────────────────────────────────────────────────────────────

(define (checkpoint-estimated-tokens cp)
  ;; Conservative ceiling at ~4 characters/token.
  (let ([text (checkpoint->text cp)]) (quotient (+ (string-length text) 3) 4)))

;; ────────────────────────────────────────────────────────────
;; Supersession logic
;; ────────────────────────────────────────────────────────────

;; Named planning artifacts supersede only the generic artifact in the same
;; family. Recognition is deliberately anchored to the basename so paths such
;; as notes-STATE-v1.2.3.md and backup suffixes cannot gain authority.
(define named-planning-rx #px"^(PLAN|STATE|VALIDATION)-v[0-9]+\\.[0-9]+\\.[0-9]+.*\\.md$")
(define generic-planning-rx #px"^(PLAN|STATE|VALIDATION)\\.md$")

(define (path-basename path-name)
  (and (string? path-name)
       (let ([name (file-name-from-path (string->path path-name))]) (and name (path->string name)))))

(define (planning-family path-name rx)
  (define basename (path-basename path-name))
  (and basename (let ([match (regexp-match rx basename)]) (and match (cadr match)))))

(define (supercedes-generic-planning? path-name)
  (and (planning-family path-name named-planning-rx) #t))

(define (contradicts-generic-planning? generic-path named-path)
  (define generic-family (planning-family generic-path generic-planning-rx))
  (define named-family (planning-family named-path named-planning-rx))
  (and generic-family named-family (string=? generic-family named-family)))

;; ────────────────────────────────────────────────────────────
;; Checkpoint parameter (current session checkpoint)
;; ────────────────────────────────────────────────────────────

;; ────────────────────────────────────────────────────────────
;; Inject the checkpoint into a context message list
;; ────────────────────────────────────────────────────────────

(define CHECKPOINT-ID "op-checkpoint")

(define (checkpoint-message? item)
  (and (message? item) (equal? (message-id item) CHECKPOINT-ID)))

(define (inject-checkpoint-message cp messages)
  ;; Checkpoints are ephemeral assembly records, never history. Remove a prior
  ;; copy first so repeated assembly remains idempotent.
  (define cleaned (filter (lambda (item) (not (checkpoint-message? item))) messages))
  (if (and cp
           (operational-checkpoint-repo-root cp)
           (not (equal? (operational-checkpoint-repo-root cp) "")))
      (let ([text (checkpoint->text cp)])
        (if (string=? text "")
            cleaned
            (cons (make-message CHECKPOINT-ID
                                #f
                                'system
                                'checkpoint
                                (list (make-text-part text))
                                (current-seconds)
                                (hasheq 'ephemeral #t 'source 'operational-checkpoint))
                  cleaned)))
      cleaned))

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
         operational-checkpoint-planning-authority
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
         checkpoint-set-planning-authority
         checkpoint-estimated-tokens
         supercedes-generic-planning?
         contradicts-generic-planning?
         inject-checkpoint-message)
