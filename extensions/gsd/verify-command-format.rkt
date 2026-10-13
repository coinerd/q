#lang racket/base
;; Pure declaration canonicalization shared by execution and offline authority.
;; This is deliberately not a shell parser: ambiguous scripts fail closed.
(require racket/string)
(provide canonical-verify-command
         verify-command-stages)

(define (verify-command-stages command)
  (and
   (string? command)
   (non-empty-string? (string-trim command))
   (let ([s (string-trim command)])
     (cond
       ;; Preserve legacy single-command shell contracts. Strict audit grammar
       ;; still determines whether such a command represents the five gates.
       [(not (regexp-match? #rx"\n|&&" s)) (list s)]
       [else
        (define n (string-length s))
        (define (finish start end acc)
          (define stage (string-trim (substring s start end)))
          (and
           (non-empty-string? stage)
           (not
            (regexp-match?
             #px"^(?:!|if|then|else|elif|fi|for|while|until|do|done|case|esac|function|[{}])(?:[[:space:]]|$)"
             stage))
           (cons stage acc)))
        (let loop ([i 0]
                   [start 0]
                   [quoted #f]
                   [acc '()])
          (cond
            [(= i n)
             (and (not quoted) (let ([result (finish start n acc)]) (and result (reverse result))))]
            [else
             (define c (string-ref s i))
             (define pair-and?
               (and (char=? c #\&) (< (add1 i) n) (char=? (string-ref s (add1 i)) #\&)))
             (cond
               [quoted
                ;; Never reinterpret quoted delimiters or continuations as
                ;; stage boundaries (nor silently accept their shell success).
                (and (not (or (char=? c #\newline)
                              pair-and?
                              (and (char=? quoted #\") (memv c '(#\$ #\`)))
                              (and (char=? c #\\)
                                   (or (char=? quoted #\")
                                       (and (< (add1 i) n)
                                            (char=? (string-ref s (add1 i)) #\newline))))))
                     (loop (add1 i) start (if (char=? c quoted) #f quoted) acc))]
               [(or (char=? c #\') (char=? c #\")) (loop (add1 i) start c acc)]
               [(or pair-and? (char=? c #\newline))
                (define next
                  (if pair-and?
                      (+ i 2)
                      (add1 i)))
                (define result (finish start i acc))
                (and result (loop next next #f result))]
               ;; Placeholders are literal declaration tokens, not redirection.
               [(and (char=? c #\<) (string-prefix? (substring s i) "<project-base>"))
                (loop (+ i (string-length "<project-base>")) start #f acc)]
               [(memv c '(#\; #\| #\& #\\ #\` #\$ #\( #\) #\< #\> #\# #\return)) #f]
               [else (loop (add1 i) start #f acc)])]))]))))

(define (canonical-verify-command command)
  (cond
    [(regexp-match? #rx"\n|&&" command)
     (define stages (verify-command-stages command))
     (cond
       [(not stages) ""]
       [(regexp-match? #rx"\n" command) (string-join stages " && ")]
       [else command])]
    [else command]))
