#lang racket/base

;; q/tui/sgr.rkt — SGR post-processor
;;
;; Replaces bg=0 (ANSI black, SGR parameter 40) with default bg
;; (SGR parameter 49) in terminal output. This matches pi's behavior
;; where bg=0 cells render with the terminal's default background.
;;
;; Handles compound SGR sequences and correctly skips extended color
;; parameters (256-color and truecolor) to avoid corrupting
;; \e[38;5;40m (fg=256-color-40) sequences.
;;
;; Extracted from interfaces/tui.rkt for modularity.
;; Pure string processing — no TUI state dependencies.

(require racket/string
         racket/list)

(provide fix-sgr-bg-black)

;; Regex matching SGR escape sequences: ESC [ <params> m
(define sgr-pattern
  (regexp (format "~a\\[([0-9;]*)m" (integer->char 27))))

;; Parse SGR parameters and replace bg=black (40) with default (49).
;; Skips extended color sequences (38;5;N, 48;5;N, 38;2;R;G;B, 48;2;R;G;B).
(define (replace-bg-black-params params)
  (define parts (string-split params ";"))
  (define n (length parts))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i n) (string-join (reverse acc) ";")]
      [else
       (define p (list-ref parts i))
       (cond
         ;; Extended fg: 38;5;N or 38;2;R;G;B — skip all
         [(and (equal? p "38") (< (+ i 1) n))
          (define next (list-ref parts (+ i 1)))
          (cond
            [(and (equal? next "5") (< (+ i 2) n))
             ;; 38;5;N — consume 3 params
             (loop (+ i 3)
                   (cons (list-ref parts (+ i 2)) (cons next (cons p acc))))]
            [(and (equal? next "2") (>= (- n i) 5))
             ;; 38;2;R;G;B — consume 5 params
             (loop (+ i 5)
                   (list* (list-ref parts (+ i 4))
                          (list-ref parts (+ i 3))
                          (list-ref parts (+ i 2))
                          next p acc))]
            [else
             (loop (+ i 1) (cons p acc))])]
         ;; Extended bg: 48;5;N or 48;2;R;G;B — skip all
         [(and (equal? p "48") (< (+ i 1) n))
          (define next (list-ref parts (+ i 1)))
          (cond
            [(and (equal? next "5") (< (+ i 2) n))
             ;; 48;5;N — consume 3 params
             (loop (+ i 3)
                   (cons (list-ref parts (+ i 2)) (cons next (cons p acc))))]
            [(and (equal? next "2") (>= (- n i) 5))
             ;; 48;2;R;G;B — consume 5 params
             (loop (+ i 5)
                   (list* (list-ref parts (+ i 4))
                          (list-ref parts (+ i 3))
                          (list-ref parts (+ i 2))
                          next p acc))]
            [else
             (loop (+ i 1) (cons p acc))])]
         ;; bg=black → default
         [(equal? p "40")
          (loop (+ i 1) (cons "49" acc))]
         [else
          (loop (+ i 1) (cons p acc))])])))

;; Replace SGR parameter "40" (bg=black) with "49" (default bg)
;; in all SGR escape sequences within a string.
;; Handles compound sequences: \e[37;40m → \e[37;49m
;; Skips extended color parameters: \e[38;5;40m stays unchanged
(define (fix-sgr-bg-black str)
  (regexp-replace* sgr-pattern str
    (lambda (whole-match params)
      (if (or (not params) (equal? params ""))
          whole-match
          (string-append (string (integer->char 27)) "["
                         (replace-bg-black-params params) "m")))))
