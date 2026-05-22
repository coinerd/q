#lang racket/base

;; q/tui/input/editing-ops.rkt — pure cursor movement, deletion, selection, undo/redo
;;
;; All functions take an input-state and return a new input-state.
;; No terminal I/O. No side effects.

(require racket/contract
         racket/list
         "../char-width.rkt"
         "state-types.rkt")

(provide input-state?
         (contract-out [input-insert-char (-> any/c char? any/c)]
                       [input-insert-newline (-> any/c any/c)]
                       [input-backspace (-> any/c any/c)]
                       [input-delete (-> any/c any/c)]
                       [input-cursor-left (-> any/c any/c)]
                       [input-cursor-right (-> any/c any/c)]
                       [input-home (-> any/c any/c)]
                       [input-end (-> any/c any/c)]
                       [input-clear (-> any/c any/c)]
                       ;; Undo/Redo
                       [input-undo (-> any/c any/c)]
                       [input-redo (-> any/c any/c)]
                       ;; Kill ring
                       [input-kill-word-backward (-> any/c any/c)]
                       [input-kill-to-beginning (-> any/c any/c)]
                       [input-kill-to-end (-> any/c any/c)]
                       [input-yank (-> any/c any/c)]
                       ;; Word navigation
                       [input-cursor-word-left (-> any/c any/c)]
                       [input-cursor-word-right (-> any/c any/c)]
                       ;; Paste
                       [input-insert-string (-> any/c string? any/c)]
                       ;; Query
                       [input-at-beginning? (-> any/c boolean?)]
                       [input-at-end? (-> any/c boolean?)]
                       [input-empty? (-> any/c boolean?)]
                       [input-current-text (-> any/c string?)]
                       ;; Internal word helpers (shared with completion)
                       [find-word-start-backward
                        (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
                       [find-word-end-forward
                        (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]))

;; ============================================================
;; Basic editing
;; ============================================================

(define (input-insert-char st ch)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define new-buf (string-append (substring buf 0 cur) (string ch) (substring buf cur)))
  (push-undo st (struct-copy input-state st [buffer new-buf] [cursor (+ cur 1)])))

(define (input-backspace st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (let* ([prev-start (prev-grapheme-start buf cur)]
             [new-buf (string-append (substring buf 0 prev-start) (substring buf cur))])
        (push-undo st (struct-copy input-state st [buffer new-buf] [cursor prev-start])))))

(define (input-insert-newline st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define new-buf (string-append (substring buf 0 cur) "\n" (substring buf cur)))
  (push-undo st (struct-copy input-state st [buffer new-buf] [cursor (add1 cur)] [history-idx #f])))

(define (input-delete st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (>= cur (string-length buf))
      st
      (let* ([next-start (next-grapheme-start buf cur)]
             [new-buf (string-append (substring buf 0 cur) (substring buf next-start))])
        (push-undo st (struct-copy input-state st [buffer new-buf] [cursor cur])))))

(define (input-cursor-left st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (struct-copy input-state st [cursor (prev-grapheme-start buf cur)])))

(define (input-cursor-right st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (>= cur (string-length buf))
      st
      (struct-copy input-state st [cursor (next-grapheme-start buf cur)])))

(define (input-home st)
  (struct-copy input-state st [cursor 0]))

(define (input-end st)
  (struct-copy input-state st [cursor (string-length (input-state-buffer st))]))

(define (input-clear st)
  (push-undo st (struct-copy input-state st [buffer ""] [cursor 0])))

;; ============================================================
;; Query helpers
;; ============================================================

(define (input-current-text st)
  (input-state-buffer st))

(define (input-at-beginning? st)
  (zero? (input-state-cursor st)))

(define (input-at-end? st)
  (= (input-state-cursor st) (string-length (input-state-buffer st))))

(define (input-empty? st)
  (string=? (input-state-buffer st) ""))

;; ============================================================
;; Undo / Redo
;; ============================================================

(define (input-undo st)
  (define stack (input-state-undo-stack st))
  (if (null? stack)
      st
      (let* ([prev (car stack)]
             [rest (cdr stack)]
             [redo (cons (strip-for-undo st) (input-state-redo-stack st))])
        (struct-copy input-state
                     st
                     [buffer (input-state-buffer prev)]
                     [cursor (input-state-cursor prev)]
                     [undo-stack rest]
                     [redo-stack redo]))))

(define (input-redo st)
  (define stack (input-state-redo-stack st))
  (if (null? stack)
      st
      (let* ([next (car stack)]
             [rest (cdr stack)]
             [undo (cons (strip-for-undo st) (input-state-undo-stack st))])
        (struct-copy input-state
                     st
                     [buffer (input-state-buffer next)]
                     [cursor (input-state-cursor next)]
                     [undo-stack undo]
                     [redo-stack rest]))))

;; ============================================================
;; Kill ring
;; ============================================================

(define (input-kill-word-backward st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (let* ([killed-end cur]
             [killed-start (find-word-start-backward buf cur)])
        (define killed-text (substring buf killed-start killed-end))
        (define new-buf (string-append (substring buf 0 killed-start) (substring buf killed-end)))
        (push-undo st
                   (push-kill (struct-copy input-state st [buffer new-buf] [cursor killed-start])
                              killed-text)))))

(define (input-kill-to-beginning st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (let* ([killed-text (substring buf 0 cur)]
             [new-buf (substring buf cur)])
        (push-undo st
                   (push-kill (struct-copy input-state st [buffer new-buf] [cursor 0])
                              killed-text)))))

(define (input-kill-to-end st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (>= cur (string-length buf))
      st
      (let* ([killed-text (substring buf cur)]
             [new-buf (substring buf 0 cur)])
        (push-undo st
                   (push-kill (struct-copy input-state st [buffer new-buf] [cursor cur])
                              killed-text)))))

(define (input-yank st)
  (define ring (input-state-kill-ring st))
  (if (null? ring)
      st
      (input-insert-string st (car ring))))

;; ============================================================
;; Word navigation
;; ============================================================

(define (find-word-start-backward buf pos)
  (let loop ([i (sub1 pos)]
             [skipping-ws #t])
    (cond
      [(< i 0) 0]
      [skipping-ws
       (if (char-whitespace? (string-ref buf i))
           (loop (sub1 i) #t)
           (loop i #f))]
      [else
       (if (char-whitespace? (string-ref buf i))
           (add1 i)
           (if (zero? i)
               0
               (loop (sub1 i) #f)))])))

(define (find-word-end-forward buf pos)
  (define len (string-length buf))
  (let loop ([i pos])
    (cond
      [(>= i len) len]
      [(char-whitespace? (string-ref buf i)) i]
      [else (loop (add1 i))])))

(define (input-cursor-word-left st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (struct-copy input-state st [cursor (find-word-start-backward buf cur)])))

(define (skip-whitespace-forward buf pos)
  (define len (string-length buf))
  (let loop ([i pos])
    (cond
      [(>= i len) len]
      [(char-whitespace? (string-ref buf i)) (loop (add1 i))]
      [else i])))

(define (input-cursor-word-right st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (>= cur (string-length buf))
      st
      (let* ([after-space (skip-whitespace-forward buf cur)]
             [word-end (find-word-end-forward buf after-space)])
        (struct-copy input-state st [cursor word-end]))))

;; ============================================================
;; Paste / Insert string
;; ============================================================

(define (input-insert-string st text)
  (if (string=? text "")
      st
      (let* ([buf (input-state-buffer st)]
             [cur (input-state-cursor st)]
             [new-buf (string-append (substring buf 0 cur) text (substring buf cur))])
        (push-undo
         st
         (struct-copy input-state st [buffer new-buf] [cursor (+ cur (string-length text))])))))
