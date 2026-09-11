#lang racket/base

;; util/shell-risk.rkt — Pure structured shell command risk classifier
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
         kill-target
         kill-target?
         kill-target-verb
         kill-target-severity
         kill-target-provenance
         kill-target-pattern
         kill-target-position
         interpreter-kill-targets
         (contract-out [tokenize-shell-command (-> string? (listof shell-token?))]
                       [classify-shell-risks (-> (listof shell-token?) (listof shell-risk-finding?))]
                       [shell-risk-summary (-> (listof shell-risk-finding?) (hash/c symbol? any/c))]
                       [risk-severity? (-> symbol? boolean?)]
                       [token-type? (-> symbol? boolean?)]
                       [risk-type? (-> symbol? boolean?)]
                       [kill-targets (-> (listof shell-token?) (listof kill-target?))]))

;; ── Data structures ───────────────────────────────────────────────

(struct shell-token (type value start end) #:transparent)
(struct shell-risk-finding (type severity message position) #:transparent)

(define (risk-severity? v)
  (and (member v '(info low medium high critical)) #t))

(define (token-type? v)
  (and (member v '(word separator redirect substitution quote whitespace unknown)) #t))

(define (risk-type? v)
  (and (member v
               '(destructive high-risk
                             network-pipe
                             substitution
                             redirect-sensitive
                             command-substitution
                             eval
                             exec
                             windows-destructive
                             process-kill))
       #t))

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
         [(char=? c #\&) (loop (add1 i) (cons (shell-token 'separator "&" i (add1 i)) tokens))]
         [(and (char=? c #\|) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\|))
          (loop (+ i 2) (cons (shell-token 'separator "||" i (+ i 2)) tokens))]
         [(char=? c #\|) (loop (add1 i) (cons (shell-token 'separator "|" i (add1 i)) tokens))]
         ;; Redirects
         [(or (char=? c #\>)
              (char=? c #\<)
              (and (char-numeric? c) (< (add1 i) len) (char=? (list-ref chars (add1 i)) #\>)))
          (define j (skip-redirect chars i))
          (loop j (cons (shell-token 'redirect (substring command i j) i j) tokens))]
         ;; Literal parentheses outside command substitutions. Treat as unknown
         ;; tokens to guarantee tokenizer progress on heredoc/CSS payloads.
         [(or (char=? c #\() (char=? c #\)))
          (loop (add1 i)
                (cons (shell-token 'unknown (substring command i (add1 i)) i (add1 i)) tokens))]
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

;; ── BUG-0066 (#9635): process-kill classification ─────────────────
;;
;; The v1.00.28 W0 crash: the executor's census kill/restart spiral ran
;; `for p in $(pgrep -x racket); do kill "$p"; done`, which matched the q
;; host process and killed the agent itself. Killing by NAME or from an
;; unpinned dynamic process listing is the one command class that can
;; kill the agent host. A kill target is safe only with recorded-PID
;; provenance: a literal numeric PID, `$!`, a shell job spec (%N), a
;; pidfile read (`kill "$(cat service.pid)"`), `pkill -F pidfile`, or
;; `pkill --pid N`. The `kill -0 PID` existence probe is exempt.
;; Everything else — killall/pkill name patterns (including the
;; `[r]acket` bracket trick that hides the name from the process
;; listing), a kill fed from pgrep/pidof/ps/lsof output or an unbound
;; PID variable, and `... | xargs kill` — becomes a 'process-kill
;; finding: 'critical when the pattern can match a bare interpreter name
;; (a self-match kills the agent host), 'high otherwise. Severity
;; follows the mutating-verb taxonomy (rm -rf is critical; ordinary
;; mutation evidence is high).

;; Process names the agent itself runs under; a kill pattern whose
;; normalized tokens contain one of these is a self-match pattern.
(define interpreter-kill-targets
  '("racket" "raco" "node" "deno" "bun" "python" "python3" "ruby" "perl" "java" "q"))

(define kill-command-verbs '("kill" "pkill" "killall"))
(define kill-wrapper-verbs '("sudo" "nohup" "env" "nice" "command" "time" "timeout" "stdbuf"))
;; Loop/block syntax that ends an open kill context at a word boundary.
(define kill-block-keywords '("done" "fi" "esac"))
;; Kill flags whose NEXT token is a value (signal, recorded PID, or
;; pidfile), not a target. Case matters: pkill -F takes a pidfile,
;; pkill -f takes none.
(define kill-value-flags
  '("-s" "--signal" "--pid" "--parent" "--ns" "-F" "--pidfile" "-u" "-t" "-g" "-G" "-P"))

(struct kill-target (verb severity provenance pattern position) #:transparent)

;; Lowercase, unquote, and collapse single-character bracket classes so
;; the classic pgrep self-avoidance trick (`pkill -f "[r]acket"`) cannot
;; hide the interpreter name from the self-match check.
(define (normalize-kill-pattern text)
  (define unquoted
    (string-downcase (string-trim (regexp-replace* #px"^[\"']+|[\"']+$" (string-trim text) ""))))
  (regexp-replace* #rx"\\[([a-z0-9])\\]" unquoted "\\1"))

(define (kill-pattern-self-match? text)
  (for/or ([tok (in-list (regexp-match* #px"[a-z0-9]+" (normalize-kill-pattern text)))])
    (and (member tok interpreter-kill-targets) #t)))

;; Inner text of a token that wraps exactly one command substitution.
(define (kill-substitution-body text)
  (or (let ([m (regexp-match #rx"^\\$\\((.*)\\)$" text)]) (and m (cadr m)))
      (let ([m (regexp-match #rx"^`(.*)`$" text)]) (and m (cadr m)))))

;; Provenance of a substitution body used as (part of) a kill target
;; list. Pidfile reads and `jobs -p` are recorded provenance; numeric
;; bodies are recorded PIDs; process listings and anything unknown are
;; unpinned (fail closed).
(define (kill-body-provenance body)
  (define b (string-downcase (string-trim body)))
  (cond
    [(regexp-match? #rx"\\.pid([\"' ]|$)" b) 'pidfile]
    [(regexp-match? #px"^jobs\\b" b) 'job]
    [(regexp-match? #px"^[0-9]+$" b) 'recorded-pid]
    [else 'unpinned-pattern]))

;; Quote token values include their surrounding quote characters.
(define (strip-outer-quotes text)
  (define s (string-trim text))
  (if (and (>= (string-length s) 2)
           (or (and (string-prefix? s "\"") (string-suffix? s "\""))
               (and (string-prefix? s "'") (string-suffix? s "'"))))
      (substring s 1 (sub1 (string-length s)))
      s))

;; A quoted string fed to bash/sh/eval/etc. whose first word is a kill
;; verb: re-classify the script (its substitution/quote nesting is not
;; tokenized at the outer level).
(define (quoted-kill-script? inner)
  (define toks (string-split (string-downcase inner)))
  (and (not (null? toks)) (and (member (car toks) kill-command-verbs) #t)))

;; Walk the token stream and collect structured kill targets. The
;; tokenizer is shape-preserving (whitespace tokens included, quotes and
;; substitutions kept whole), so provenance tracking is a small state
;; machine: a kill verb opens a context, its word/quote/substitution
;; tokens are classified into recorded vs unpinned provenance, and the
;; context flushes at a separator, block keyword, or end of input.
;; `for`-loop variable bindings record where the PID list came from, so
;; the exact v1.00.28 W0 crash command resolves `kill "$p"` to the
;; unpinned `pgrep` listing and self-matches.
(define (kill-targets tokens)
  (define targets '())
  (define bindings (make-hash)) ; loop var / assignment var -> (list provenance source)
  (define active #f) ; vector [verb position args existence-probe?]
  (define end-of-options? #f)
  (define pending-var #f) ; VAR= whose $(...) value source is still ahead
  (define expect-loop-var #f) ; between `for` and the variable name
  (define expect-loop-in #f) ; between the variable name and `in`
  (define loop-collect #f) ; collecting `for V in ...` sources
  (define loop-var #f)
  (define pipe-source #f) ; (list provenance source) piped into xargs kill
  (define expect-xargs-kill #f) ; `xargs` seen, kill verb may follow
  (define expect-value-for #f) ; flag value kind: 'skip | 'recorded-pid | 'pidfile
  (define prev-word #f) ; previous downcased word (script-runner detection)

  (define (add-target! t)
    (set! targets (cons t targets)))

  (define (arg! prov source)
    (when active
      (vector-set! active 2 (cons (list prov source) (vector-ref active 2)))))

  (define (open-kill! verb pos)
    (set! active (vector verb pos '() #f))
    (set! end-of-options? #f)
    (set! pipe-source #f)
    (set! expect-xargs-kill #f))

  (define (flush!)
    (when active
      (define args (reverse (vector-ref active 2)))
      (define unpinned
        (for/list ([a (in-list args)]
                   #:when (memq (car a) '(unpinned-name unpinned-pattern unbound)))
          a))
      (when (and (pair? unpinned) (not (vector-ref active 3)))
        (define self? (ormap (lambda (a) (kill-pattern-self-match? (cadr a))) unpinned))
        (define provenance
          (cond
            [self? 'self-match]
            [(ormap (lambda (a) (eq? (car a) 'unpinned-name)) unpinned) 'unpinned-name]
            [(ormap (lambda (a) (eq? (car a) 'unbound)) unpinned) 'unbound]
            [else 'unpinned-pattern]))
        (add-target! (kill-target (vector-ref active 0)
                                  (if self? 'critical 'high)
                                  provenance
                                  (string-join (map cadr unpinned) " ")
                                  (vector-ref active 1))))
      (set! active #f)))

  ;; Classify one argument word (or quote-inner text) of an open kill.
  (define (consume-kill-arg! text)
    (cond
      [expect-value-for
       (define kind expect-value-for)
       (set! expect-value-for #f)
       (case kind
         [(recorded-pid) (arg! 'recorded-pid text)]
         [(pidfile) (arg! 'pidfile text)]
         [else (void)])]
      [(equal? text "-0") (vector-set! active 3 #t)] ; existence probe, not a kill
      [(equal? text "--") (set! end-of-options? #t)]
      [(and (not end-of-options?) (member text kill-value-flags))
       (set! expect-value-for
             (cond
               [(member text '("-s" "--signal")) 'skip]
               [(member text '("--pid" "--parent" "--ns")) 'recorded-pid]
               [(member text '("-F" "--pidfile")) 'pidfile]
               [else 'skip]))]
      ;; Plain options (-x, -f, -e, --verbose, ...) carry no target.
      [(and (not end-of-options?) (regexp-match? #px"^-[^0-9]" text)) (void)]
      ;; A literal PID; a negative number is a numeric signal, harmless.
      [(regexp-match? #px"^-?[0-9]+$" text) (arg! 'recorded-pid text)]
      [(equal? text "$!") (arg! 'recorded-pid text)]
      [(regexp-match? #rx"^%[0-9]+$" text) (arg! 'job text)]
      [(kill-substitution-body text)
       =>
       (lambda (body) (arg! (kill-body-provenance body) body))]
      [(regexp-match #rx"^[$]\\{?([A-Za-z_][A-Za-z0-9_]*)\\}?$" text)
       =>
       (lambda (m)
         (define b (hash-ref bindings (string->symbol (cadr m)) #f))
         (if b
             (arg! (car b) (cadr b))
             (arg! 'unbound text)))]
      [else (arg! 'unpinned-name text)]))

  (define (consume-word! raw pos)
    (define stripped (string-trim raw))
    (define v (string-downcase stripped))
    (set! prev-word v)
    (cond
      [active (consume-kill-arg! stripped)]
      [(and sub-assign-var sub-group)
       (set! sub-accum
             (if (string=? sub-accum "")
                 stripped
                 (string-append sub-accum " " stripped)))]
      [(member v kill-wrapper-verbs) (void)]
      [(equal? v "xargs") (set! expect-xargs-kill #t)]
      [(and expect-xargs-kill (member v kill-command-verbs))
       (set! expect-xargs-kill #f)
       ;; Capture the piped-in PID source BEFORE open-kill! resets it.
       (define src (or pipe-source (list 'unbound "xargs-stdin-list")))
       (open-kill! v pos)
       (arg! (car src) (cadr src))]
      [expect-xargs-kill (set! expect-xargs-kill #f)]
      [(equal? v "for") (set! expect-loop-var #t)]
      [expect-loop-var
       (set! loop-var (string->symbol v))
       (set! expect-loop-var #f)
       (set! expect-loop-in #t)]
      [expect-loop-in
       (set! expect-loop-in #f)
       (set! loop-collect #t)]
      ;; Process-listing commands feeding a pipe are unpinned sources.
      [(and (not loop-collect) (member v '("pgrep" "pidof" "ps" "lsof" "fuser")))
       (set! pipe-source (list 'unpinned-pattern v))]
      [(equal? v "do") (set! loop-collect #f)]
      [(member v kill-block-keywords) (flush!)]
      [(member v kill-command-verbs) (open-kill! v pos)]
      [loop-collect (void)]
      [else
       ;; VAR=... assignments record PID provenance for later kills.
       (define m (regexp-match #rx"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$" stripped))
       (when m
         (define val (caddr m))
         (cond
           [(regexp-match? #px"^[0-9]+$" val)
            (hash-set! bindings (string->symbol (cadr m)) (list 'recorded-pid val))]
           [(kill-substitution-body val)
            =>
            (lambda (body)
              (hash-set! bindings (string->symbol (cadr m)) (list (kill-body-provenance body) body)))]
           [(equal? val "") (set! pending-var (string->symbol (cadr m)))]
           ;; `NAME=$` — lexer split of `NAME=$(…)`: the paren group that
           ;; follows completes the substitution.
           [(equal? val "$")
            (set! sub-assign-var (string->symbol (cadr m)))
            (set! sub-group #f)
            (set! sub-accum "")]
           [else (void)]))]))

  ;; Split-form command substitution: the lexer emits `NAME=$` as a word
  ;; followed by `(` … `)` fragments (e.g. `PID=$(cat q.pid)`).  Track the
  ;; pending assignment so the paren-delimited words can be joined into a
  ;; substitution body.  Any other token type while pending cancels the
  ;; binding (fail-safe: the variable stays unbound → kill stays high).
  (define sub-assign-var #f)
  (define sub-group #f)
  (define sub-accum "")
  (define (cancel-sub-assign!)
    (set! sub-assign-var #f)
    (set! sub-group #f)
    (set! sub-accum ""))
  (for ([tok (in-list tokens)])
    (define type (shell-token-type tok))
    (define raw (shell-token-value tok))
    (define pos (shell-token-start tok))
    (cond
      [(eq? type 'whitespace) (void)]
      [(eq? type 'unknown)
       (cond
         [(and sub-assign-var (equal? raw "(")) (set! sub-group #t)]
         [(and sub-assign-var sub-group (equal? raw ")"))
          (hash-set! bindings sub-assign-var (list (kill-body-provenance sub-accum) sub-accum))
          (cancel-sub-assign!)]
         [(and sub-assign-var sub-group) (cancel-sub-assign!)]
         [else (void)])]
      [(eq? type 'redirect)
       (when sub-assign-var
         (cancel-sub-assign!))]
      [(eq? type 'separator)
       (when sub-assign-var
         (cancel-sub-assign!))
       (flush!)
       (set! pending-var #f)
       (set! expect-loop-var #f)
       (set! expect-loop-in #f)
       (set! loop-collect #f)
       (unless (string=? (string-downcase raw) "|")
         (set! pipe-source #f))]
      [(eq? type 'substitution)
       (when sub-assign-var
         (cancel-sub-assign!))
       (define body (or (kill-substitution-body raw) (string-trim raw)))
       (cond
         [active (arg! (kill-body-provenance body) body)]
         [pending-var
          (hash-set! bindings pending-var (list (kill-body-provenance body) body))
          (set! pending-var #f)]
         [loop-collect
          (when loop-var
            (hash-set! bindings loop-var (list (kill-body-provenance body) body)))]
         [else (set! pipe-source (list (kill-body-provenance body) body))])]
      [(eq? type 'quote)
       (when sub-assign-var
         (cancel-sub-assign!))
       (define inner (strip-outer-quotes raw))
       (cond
         [active (consume-kill-arg! inner)]
         [(and prev-word
               (member prev-word '("bash" "sh" "dash" "zsh" "-c" "eval" "source" "exec"))
               (quoted-kill-script? inner))
          (for ([t (in-list (kill-targets (tokenize-shell-command inner)))])
            (add-target! t))]
         [else (void)])]
      [else (consume-word! raw pos)]))

  (flush!)
  (reverse targets))

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
      ;; #9516: bare `mv a b` renames are routine shell usage and were pure
      ;; warning fatigue; only root-target moves (mv /...) stay destructive.
      [(string-prefix? val "mv /")
       (add! 'destructive 'medium (format "Move operation (root target): ~a" val) pos)]))

  ;; Substitution and pipe detection on individual tokens
  ;;
  ;; #9516: benign $(...)/`...` substitutions are downgraded to 'low.
  ;; Flagging every substitution 'high produced constant false alarms
  ;; (e.g. echo $(basename file), p=$(basename "$f")) and trained users
  ;; and models to ignore real findings. Substitutions whose output feeds
  ;; a pipe-to-shell target (| sh / | bash / | cmd / | powershell —
  ;; including a substitution used as the piped command) stay 'high,
  ;; together with the network-pipe finding.
  (define (pipe-target-index idx)
    (let loop ([j (add1 idx)])
      (cond
        [(>= j (length tokens)) #f]
        [(eq? (shell-token-type (list-ref tokens j)) 'whitespace) (loop (add1 j))]
        [else j])))

  (define (shell-target-token? tok)
    (or (eq? (shell-token-type tok) 'substitution)
        (and (eq? (shell-token-type tok) 'word)
             (member (string-downcase (shell-token-value tok)) '("sh" "bash" "cmd" "powershell")))))

  (define pipe-to-shell?
    (for/or ([tok (in-list tokens)]
             [idx (in-naturals)])
      (and (eq? (shell-token-type tok) 'separator)
           (string=? (string-downcase (shell-token-value tok)) "|")
           (cond
             [(pipe-target-index idx)
              =>
              (lambda (j) (and (shell-target-token? (list-ref tokens j)) #t))]
             [else #f]))))

  (for ([tok (in-list tokens)]
        [idx (in-naturals)])
    (define type (shell-token-type tok))
    (define val (shell-token-value tok))
    (define pos (shell-token-start tok))

    (when (eq? type 'substitution)
      ;; #9516: 'low unless the command pipes into a shell interpreter.
      (add! 'command-substitution
            (if pipe-to-shell? 'high 'low)
            (format "Command substitution: ~a" val)
            pos))

    (when (and (eq? type 'separator) (string=? (string-downcase val) "|"))
      ;; Skip whitespace to find next word
      (define next-idx (pipe-target-index idx))
      (when next-idx
        (define next-tok (list-ref tokens next-idx))
        (when (shell-target-token? next-tok)
          (define next-val
            (if (eq? (shell-token-type next-tok) 'word)
                (string-downcase (shell-token-value next-tok))
                (shell-token-value next-tok)))
          (add! 'network-pipe
                'high
                (format "Pipe to shell: | ~a" next-val)
                (shell-token-start next-tok))))))

  ;; BUG-0066 (#9635): process-kill findings. Kills by name, from an
  ;; unpinned process listing, or through any unproven PID source are
  ;; the one command class that can kill the agent host itself;
  ;; recorded-PID kills stay clean (see kill-targets).
  (for ([t (in-list (kill-targets tokens))])
    (add!
     'process-kill
     (kill-target-severity t)
     (format (string-append
              "Process kill without recorded-PID provenance: ~a ~a; record the PID (pidfile or $!)"
              " and kill \"$(cat <file>.pid)\" instead of killing by name or unpinned pattern")
             (kill-target-verb t)
             (kill-target-pattern t))
     (kill-target-position t)))

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
