#lang racket/base

;; q/tui/input/history-ops.rkt — history traversal, filtering, submission
;;
;; Pure functions. No terminal I/O. No side effects.

(require racket/string
         racket/list
         "state-types.rkt")

(provide input-history-push
         input-history-up
         input-history-down
         input-submit)

;; Add text to history if non-empty and different from last entry.
(define (input-history-push st text)
  (if (or (string=? text "")
          (and (not (null? (input-state-history st)))
               (string=? text (last (input-state-history st)))))
      st
      (struct-copy input-state
                   st
                   [history (append (input-state-history st) (list text))]
                   [history-idx #f]
                   [saved-text #f])))

;; Move to older entry in history.
(define (input-history-up st)
  (define hist (input-state-history st))
  (if (null? hist)
      st
      (let* ([current-idx (input-state-history-idx st)]
             [new-idx (if (not current-idx)
                          (- (length hist) 1)
                          (- current-idx 1))])
        (if (< new-idx 0)
            st
            (let ([saved (or (input-state-saved-text st) (input-state-buffer st))])
              (struct-copy input-state
                           st
                           [history-idx new-idx]
                           [buffer (list-ref hist new-idx)]
                           [cursor (string-length (list-ref hist new-idx))]
                           [saved-text saved]))))))

;; Move to newer entry in history.
(define (input-history-down st)
  (define hist (input-state-history st))
  (define current-idx (input-state-history-idx st))
  (if (not current-idx)
      st
      (let ([new-idx (+ current-idx 1)])
        (if (>= new-idx (length hist))
            (let ([saved (or (input-state-saved-text st) "")])
              (struct-copy input-state
                           st
                           [history-idx #f]
                           [buffer saved]
                           [cursor (string-length saved)]
                           [saved-text #f]))
            (struct-copy input-state
                         st
                         [history-idx new-idx]
                         [buffer (list-ref hist new-idx)]
                         [cursor (string-length (list-ref hist new-idx))])))))

;; Returns (values text new-state) or (values #f st) for empty input.
(define (input-submit st)
  (define text (string-trim (input-state-buffer st)))
  (if (string=? text "")
      (values #f st)
      (let ([pushed (input-history-push st text)])
        (values text (struct-copy input-state pushed [buffer ""] [cursor 0])))))
