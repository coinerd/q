#lang racket/base

;; q/tui/input/completion-ops.rkt — file reference expansion, inline bash expansion
;;
;; Pure functions. No terminal I/O. No side effects (except glob for file expansion).

(require racket/string
         racket/file
         racket/path
         file/glob
         "state-types.rkt"
         "editing-ops.rkt")

(provide input-expand-file-ref
         expand-inline-bash)

;; ============================================================
;; Inline bash expansion (G3.3)
;; ============================================================

(define (expand-inline-bash text last-prompt)
  (cond
    [(string-prefix? text "!!")
     (define rest (substring text 2))
     (if last-prompt
         (string-append last-prompt rest)
         text)]
    [else text]))

;; ============================================================
;; File reference expansion (G3.2)
;; ============================================================

(define (find-at-prefix buf cur)
  (let loop ([i (sub1 cur)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref buf i) #\@)
       (if (or (zero? i) (char-whitespace? (string-ref buf (sub1 i)))) i #f)]
      [(char-whitespace? (string-ref buf i)) #f]
      [else (loop (sub1 i))])))

(define (longest-common-prefix strings)
  (if (null? strings)
      ""
      (foldl (lambda (s prefix) (string-common-prefix prefix s)) (car strings) (cdr strings))))

(define (string-common-prefix a b)
  (let loop ([i 0])
    (cond
      [(or (>= i (string-length a)) (>= i (string-length b))) (substring a 0 i)]
      [(char=? (string-ref a i) (string-ref b i)) (loop (add1 i))]
      [else (substring a 0 i)])))

(define (expand-file-path partial)
  (if (string=? partial "")
      #f
      (let ([matches (glob (string-append partial "*"))])
        (define base (current-directory))
        (define paths
          (for/list ([p (in-list matches)]
                     #:when (file-exists? p))
            (path->string (find-relative-path base p))))
        (cond
          [(null? paths) #f]
          [(= (length paths) 1) (car paths)]
          [else
           (define common (longest-common-prefix paths))
           (if (string=? common partial) #f common)]))))

(define (input-expand-file-ref st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define at-pos (find-at-prefix buf cur))
  (cond
    [(not at-pos) st]
    [else
     (define partial (substring buf (add1 at-pos) cur))
     (define expanded (expand-file-path partial))
     (cond
       [(not expanded) st]
       [else
        (define new-buf (string-append (substring buf 0 (add1 at-pos)) expanded (substring buf cur)))
        (define new-cursor (+ (add1 at-pos) (string-length expanded)))
        (push-undo st (struct-copy input-state st [buffer new-buf] [cursor new-cursor]))])]))
