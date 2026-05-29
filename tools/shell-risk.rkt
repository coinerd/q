#lang racket/base

;; tools/shell-risk.rkt — Structured shell command risk classifier (v0.70.3)
;;
;; Pure classifier: tokenizes a shell command string and returns structured
;; risk findings without executing or rewriting the command.

(require racket/contract
         racket/string)

(provide shell-token
         shell-token?
         shell-token-type
         shell-token-value
         shell-token-start
         shell-token-end
         shell-risk-finding
         shell-risk-finding?
         shell-risk-finding-type
         shell-risk-finding-severity
         shell-risk-finding-message
         shell-risk-finding-position
         (contract-out [tokenize-shell-command (-> string? (listof shell-token?))]
                       [classify-shell-risks (-> (listof shell-token?) (listof shell-risk-finding?))]
                       [shell-risk-summary (-> (listof shell-risk-finding?) (hash/c symbol? any/c))]
                       [risk-severity? (-> symbol? boolean?)]
                       [token-type? (-> symbol? boolean?)]
                       [risk-type? (-> symbol? boolean?)]))

;; ── Data structures ───────────────────────────────────────────────

(struct shell-token (type value start end) #:transparent)
(struct shell-risk-finding (type severity message position) #:transparent)

(define (risk-severity? v)
  (member v '(info low medium high critical)))

(define (token-type? v)
  (member v '(word separator redirect substitution quote whitespace unknown)))

(define (risk-type? v)
  (member v
          '(destructive high-risk
                        network-pipe
                        substitution
                        redirect-sensitive
                        command-substitution
                        eval
                        exec
                        windows-destructive)))

;; ── Tokenizer helpers ─────────────────────────────────────────────

(define (skip-while chars i pred)
  (let loop ([j i])
    (cond
      [(>= j (length chars)) j]
      [(pred (list-ref chars j)) (loop (add1 j))]
      [else j])))

(define (find-char chars i target)
  (let loop ([j i])
    (cond
      [(>= j (length chars)) #f]
      [(char=? (list-ref chars j) target) j]
      [else (loop (add1 j))])))

(define (find-matching-paren chars i close-char)
  (let loop ([j i]
             [depth 1])
    (cond
      [(>= j (length chars)) #f]
      [(char=? (list-ref chars j) #\() (loop (add1 j) (add1 depth))]
      [(char=? (list-ref chars j) close-char)
       (if (= depth 1)
           j
           (loop (add1 j) (sub1 depth)))]
      [else (loop (add1 j) depth)])))

(define (skip-redirect chars i)
  (let loop ([j i])
    (cond
      [(>= j (length chars)) j]
      [(member (list-ref chars j) '(#\> #\< #\& #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (loop (add1 j))]
      [else j])))

;; ── Tokenizer ─────────────────────────────────────────────────────

(define (tokenize-shell-command command)
  (define chars (string->list command))
  (define len (length chars))

  (define (loop i tokens)
    (cond
      [(>= i len) (reverse tokens)]
      [else
       (define c (list-ref chars i))
       (cond
         ;; Whitespace
         [(char-whitespace? c)
          (define j (skip-while chars i char-whitespace?))
          (loop j (cons (shell-token 'whitespace (substring command i j) i j) tokens))]
         ;; Single-quoted string
         [(char=? c #\')
          (define j (find-char chars (add1 i) #\'))
          (loop (if j
                    (add1 j)
                    len)
                (cons (shell-token 'quote
                                   (substring command
                                              i
                                              (if j
                                                  (add1 j)
                                                  len))
                                   i
                                   (if j
                                       (add1 j)
                                       len))
                      tokens))]
         ;; Double-quoted string
         [(char=? c #\")
          (define j (find-char chars (add1 i) #\"))
          (loop (if j
                    (add1 j)
                    len)
                (cons (shell-token 'quote
                                   (substring command
                                              i
                                              (if j
                                                  (add1 j)
                                                  len))
                                   i
                                   (if j
                                       (add1 j)
                                       len))
                      tokens))]
         ;; Command substitution $(...)
         [(and (char=? c #\$) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\())
          (define j (find-matching-paren chars (+ i 2) #\)))
          (loop (if j
                    (add1 j)
                    len)
                (cons (shell-token 'substitution
                                   (substring command
                                              i
                                              (if j
                                                  (add1 j)
                                                  len))
                                   i
                                   (if j
                                       (add1 j)
                                       len))
                      tokens))]
         ;; Backtick substitution
         [(char=? c #\`)
          (define j (find-char chars (add1 i) #\`))
          (loop (if j
                    (add1 j)
                    len)
                (cons (shell-token 'substitution
                                   (substring command
                                              i
                                              (if j
                                                  (add1 j)
                                                  len))
                                   i
                                   (if j
                                       (add1 j)
                                       len))
                      tokens))]
         ;; Separators
         [(char=? c #\;) (loop (add1 i) (cons (shell-token 'separator ";" i (add1 i)) tokens))]
         [(and (char=? c #\&) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\&))
          (loop (+ i 2) (cons (shell-token 'separator "&&" i (+ i 2)) tokens))]
         [(and (char=? c #\|) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\|))
          (loop (+ i 2) (cons (shell-token 'separator "||" i (+ i 2)) tokens))]
         [(char=? c #\|) (loop (add1 i) (cons (shell-token 'separator "|" i (add1 i)) tokens))]
         ;; Redirects
         [(or (char=? c #\>)
              (char=? c #\<)
              (and (char-numeric? c) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\>)))
          (define j (skip-redirect chars i))
          (loop j (cons (shell-token 'redirect (substring command i j) i j) tokens))]
         ;; Word
         [else
          (define j
            (skip-while chars
                        i
                        (lambda (ch)
                          (not (or (char-whitespace? ch)
                                   (member ch '(#\; #\| #\& #\> #\< #\( #\) #\` #\' #\")))))))
          (loop j (cons (shell-token 'word (substring command i j) i j) tokens))])]))

  (loop 0 '()))

;; ── Risk classifier ───────────────────────────────────────────────

(define (classify-shell-risks tokens)
  (define findings '())
  (define (add! type severity msg pos)
    (set! findings (cons (shell-risk-finding type severity msg pos) findings)))

  ;; Build combined strings from word tokens, breaking only on separators
  (define word-runs '())
  (let loop ([toks tokens]
             [current '()]
             [start #f])
    (cond
      [(null? toks)
       (when (pair? current)
         (set! word-runs (cons (cons start (string-join (reverse current) " ")) word-runs)))]
      [(eq? (shell-token-type (car toks)) 'word)
       (loop (cdr toks)
             (cons (shell-token-value (car toks)) current)
             (or start (shell-token-start (car toks))))]
      [(eq? (shell-token-type (car toks)) 'separator)
       (when (pair? current)
         (set! word-runs (cons (cons start (string-join (reverse current) " ")) word-runs)))
       (loop (cdr toks) '() #f)]
      [else (loop (cdr toks) current start)]))
  (set! word-runs (reverse word-runs))

  (for ([run (in-list word-runs)])
    (define pos (car run))
    (define val (string-downcase (cdr run)))

    (cond
      [(and (string-prefix? val "rm") (or (string-contains? val "-rf") (string-contains? val "-fr")))
       (add! 'destructive 'critical (format "Recursive force delete: ~a" val) pos)]
      [(string-prefix? val "rmdir")
       (add! 'destructive 'high (format "Directory removal: ~a" val) pos)]
      [(string-prefix? val "mkfs")
       (add! 'destructive 'critical (format "Filesystem creation: ~a" val) pos)]
      [(and (string-prefix? val "dd") (string-contains? val "of=/dev/"))
       (add! 'destructive 'critical (format "Direct device write: ~a" val) pos)]
      [(string-prefix? val "shutdown")
       (add! 'destructive 'high (format "System shutdown: ~a" val) pos)]
      [(string-prefix? val "reboot") (add! 'destructive 'high (format "System reboot: ~a" val) pos)]
      [(string-prefix? val "format")
       (add! 'windows-destructive 'high (format "Windows format: ~a" val) pos)]
      [(string-prefix? val "del")
       (add! 'windows-destructive 'medium (format "Windows delete: ~a" val) pos)]
      [(and (string-prefix? val "chmod") (string-contains? val "777") (string-contains? val "/"))
       (add! 'destructive 'high (format "Dangerous chmod: ~a" val) pos)]
      [(string-prefix? val "eval") (add! 'eval 'medium (format "Eval indirection: ~a" val) pos)]
      [(string-prefix? val "exec") (add! 'exec 'medium (format "Process replacement: ~a" val) pos)]
      [(and (string-prefix? val "git") (string-contains? val "--force"))
       (add! 'destructive 'high (format "Force push: ~a" val) pos)]
      [(string-prefix? val "mv ") (add! 'destructive 'medium (format "Move operation: ~a" val) pos)]))

  ;; Substitution and pipe detection on individual tokens
  (for ([tok (in-list tokens)]
        [idx (in-naturals)])
    (define type (shell-token-type tok))
    (define val (shell-token-value tok))
    (define pos (shell-token-start tok))

    (when (eq? type 'substitution)
      (add! 'command-substitution 'medium (format "Command substitution: ~a" val) pos))

    (when (and (eq? type 'separator) (string=? (string-downcase val) "|"))
      ;; Skip whitespace to find next word
      (define next-idx
        (let loop ([j (add1 idx)])
          (cond
            [(>= j (length tokens)) #f]
            [(eq? (shell-token-type (list-ref tokens j)) 'whitespace) (loop (add1 j))]
            [else j])))
      (when next-idx
        (define next-tok (list-ref tokens next-idx))
        (when (eq? (shell-token-type next-tok) 'word)
          (define next-val (string-downcase (shell-token-value next-tok)))
          (when (member next-val '("sh" "bash" "cmd" "powershell"))
            (add! 'network-pipe
                  'high
                  (format "Pipe to shell: | ~a" next-val)
                  (shell-token-start next-tok)))))))

  (reverse findings))

;; ── Summary ───────────────────────────────────────────────────────

(define (shell-risk-summary findings)
  (define severities (map shell-risk-finding-severity findings))
  (define severity-order '(info low medium high critical))
  (define (index-of lst item)
    (let loop ([i 0]
               [l lst])
      (cond
        [(null? l) -1]
        [(equal? (car l) item) i]
        [else (loop (add1 i) (cdr l))])))
  (define max-sev
    (for/fold ([best 'info]) ([s (in-list severities)])
      (if (> (index-of severity-order s) (index-of severity-order best)) s best)))
  (hasheq 'count
          (length findings)
          'severities
          severities
          'max-severity
          max-sev
          'critical?
          (and (member 'critical severities) #t)))
