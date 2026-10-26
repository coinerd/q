#lang racket/base
;; Pure declaration canonicalization shared by execution and offline authority.
;; This is deliberately not a shell parser: ambiguous scripts fail closed.
(require racket/string)
(provide canonical-verify-command
         verify-command-stages
         ;; BUG-0075 (R75): complete declaration extraction — every authored
         ;; command span becomes an executed marker or a named rejection
         ;; carrying the original span; quoted prose is never banned.
         verify-declaration-extraction
         verify-normalizer-version)

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

;; ============================================================
;; BUG-0075 (R75): complete declaration extraction
;; ============================================================
;; One stable named vocabulary, shared by live execution, the frozen
;; offline binding and the review-proof retention layer. Every authored
;; command-shaped span becomes an executed marker (a canonical && stage) or
;; a named rejection carrying the original span. A span is command-shaped
;; only when its FIRST token names a known tool: prose identifiers,
;; filenames and single-word mentions are ignored, and grep inside an
;; accepted span's quoted payload is never inspected — quoted grep-looking
;; prose cannot be false-banned.

;; Bumped when the extraction vocabulary changes semantics. Consumers
;; compare retained normalized authority (never regenerate it); a version
;; change therefore refuses stale rebinding loudly instead of silently
;; reinterpreting a frozen declaration.
(define verify-normalizer-version 2)

;; Accepted first tokens (POSIX sh + this repository's toolchain).
;; `test`/`printf` are explicit: retained wave gates pin both, and their
;; exit semantics are unambiguous (0 pass / nonzero fail — an error can
;; never masquerade as success).
(define verify-accept-first-tokens
  '("racket" "raco" "bash" "sh" "python" "python3" "pytest" "npm" "pnpm"
    "yarn" "make" "git" "cd" "env" "test" "printf"))

;; Command-shaped but unsupported: a named rejection — never a silent drop
;; (the historical hazard: a dropped span made a gate appear to pass while
;; checking nothing) and never an automatic rewrite (inverting `grep -L`
;; into `! grep -q` would turn GNU grep's exit-2/multifile semantics into
;; an apparent pass). grep is called out separately because its exit
;; grammar (0 match / 1 no-match / 2 error) is exactly what can hide a
;; failure; the supported negative-content form is an explicit count
;; composition inside a `bash -c` invocation span
;; (`test "$(grep -c TOKEN file)" -eq 0` — present fails, absent passes,
;; missing/unreadable file errors nonzero).
(define verify-grep-first-tokens '("grep" "egrep" "fgrep" "rg"))
(define verify-unsupported-first-tokens
  '("find" "sed" "awk" "gawk" "ls" "cat" "head" "tail" "wc" "diff" "cmp"
    "sort" "uniq" "tr" "cut" "xargs" "tee" "stat" "readlink" "realpath"
    "which" "command" "type" "sleep" "date" "basename" "dirname"
    "perl" "ruby" "node" "npx" "deno" "bun" "php" "lua" "zsh" "ksh"
    "dash" "fish" "curl" "wget" "docker" "podman" "kubectl" "jq" "yq"
    "tar" "zip" "unzip" "gzip" "gunzip" "mkdir" "rmdir" "rm" "cp" "mv"
    "ln" "chmod" "chown" "touch" "echo" "true" "false"))

(define (verify-first-token s)
  (define m (regexp-match #px"^[^\\s]+" s))
  (and m (car m)))

;; A single bare word can never be a complete gate; it is a prose mention
;; (`unknown`, `distinct_*`, `racket`) and is ignored.
(define (verify-single-word? s)
  (and (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_.+-]*$" s) #t))

(define (verify-metrics-canonical span)
  (cond
    [(string-prefix? span "metrics.rkt ") (string-append "racket scripts/" span)]
    [(string-prefix? span "scripts/metrics.rkt ") (string-append "racket " span)]
    [else span]))

(define (verify-span-metachars? span)
  (regexp-match? #px"[;&|$<>]" span))

;; POSIX test grammar `[ ... ]` is accepted; the bash-only `[[ ... ]]` is a
;; named rejection (its meaning under /bin/sh is unportable).
(define (verify-bracket-command? span)
  (and (string-prefix? span "[")
       (not (string-prefix? span "[["))
       (string-suffix? span "]")
       (>= (string-length span) 4)))

;; → (cons 'accept executable-span) | (cons 'reject reason) | 'prose
(define (verify-classify-span span)
  (cond
    [(string=? span "") 'prose]
    [(verify-single-word? span) 'prose]
    [(or (string-prefix? span "metrics.rkt ")
         (string-prefix? span "scripts/metrics.rkt "))
     (cons 'accept (verify-metrics-canonical span))]
    [(verify-bracket-command? span) (cons 'accept span)]
    [(member (verify-first-token span) verify-accept-first-tokens)
     (cons 'accept (verify-metrics-canonical span))]
    [(or (string-prefix? span "./") (string-prefix? span "<project-base>"))
     (cons 'accept span)]
    [(string-prefix? span "!") (cons 'reject 'inverted-command)]
    [(string-prefix? span "[[") (cons 'reject 'bash-only-construct)]
    [(member (verify-first-token span) verify-grep-first-tokens)
     (cons 'reject 'grep-exit-semantics)]
    [(member (verify-first-token span) verify-unsupported-first-tokens)
     (cons 'reject 'unsupported-command)]
    [(verify-span-metachars? span) (cons 'reject 'ambiguous-shell-span)]
    [else 'prose]))

(define (verify-decorated? s)
  (regexp-match? #px"(^|\n)\\s*[-*+]\\s" s))

;; Complete extraction of one Verify declaration string.
;;   commands   — the COMPLETE validated command list (document order; each
;;                entry is one executed marker)
;;   canonical  — the executable canonical chain, "" exactly when refused
;;   rejections — named rejections: (hasheq 'reason symbol 'span original)
;;   refusal    — structural refusal symbol (#f when the shape is valid):
;;                'empty-declaration 'no-executable-command 'ambiguous-grammar
(define (verify-declaration-extraction declaration)
  (define refused
    (hasheq 'commands '() 'canonical "" 'rejections '() 'refusal 'empty-declaration))
  (if (not (string? declaration))
      refused
      (let ([trimmed (string-trim declaration)])
        (cond
          [(string=? trimmed "") refused]
          [(verify-decorated? trimmed)
           ;; Markdown bullet criteria: only backticked spans can be gates.
           ;; Every span is classified; one rejection voids the whole
           ;; declaration (never a partial green), zero commands refuses.
           (define-values (commands rejections)
             (for/fold ([commands '()] [rejections '()])
                       ([raw (in-list (regexp-match* #rx"`([^`]+)`" trimmed
                                                     #:match-select cadr))])
               (define span (string-trim raw))
               (define verdict (verify-classify-span span))
               (cond
                 [(eq? verdict 'prose) (values commands rejections)]
                 [(eq? (car verdict) 'accept)
                  (values (append commands (list (cdr verdict))) rejections)]
                 [else
                  (values commands
                          (append rejections
                                  (list (hasheq 'reason (cdr verdict) 'span span))))])))
           (cond
             [(pair? rejections)
              ;; One rejection voids the declaration: nothing executes, and
              ;; the marker list is empty by construction (never partial).
              (hasheq 'commands '() 'canonical "" 'rejections rejections 'refusal #f)]
             [(null? commands)
              (hasheq 'commands '() 'canonical "" 'rejections '()
                      'refusal 'no-executable-command)]
             [else
              (hasheq 'commands commands
                      'canonical (string-join commands " && ")
                      'rejections '()
                      'refusal #f)])]
          ;; Legacy single-line shell contract (with or without &&):
          ;; byte-for-byte passthrough through the conservative canonical
          ;; grammar — exactly the pre-R75 behavior, including its refusal of
          ;; ambiguous && shapes.
          [(not (regexp-match? #rx"\n" trimmed))
           (define canonical (canonical-verify-command trimmed))
           (if (non-empty-string? canonical)
               (hasheq 'commands (verify-command-stages canonical)
                       'canonical canonical 'rejections '() 'refusal #f)
               (hasheq 'commands '() 'canonical "" 'rejections '()
                       'refusal 'ambiguous-grammar))]
          [else
           ;; Unfenced multiline: the audited fail-fast && grammar. Every
           ;; stage must be an executable command line — an English line is
           ;; a NAMED rejection, never a launch through /bin/sh.
           (define stages (verify-command-stages trimmed))
           (cond
             [(not stages)
              (hasheq 'commands '() 'canonical "" 'rejections '()
                      'refusal 'ambiguous-grammar)]
             [else
              (define bad
                (for/first ([stage (in-list stages)]
                            #:when (let ([v (verify-classify-span stage)])
                                     (not (and (pair? v) (eq? (car v) 'accept)))))
                  stage))
              (cond
                [bad
                 (hasheq 'commands '() 'canonical ""
                         'rejections (list (hasheq 'reason 'prose-in-unfenced-multiline
                                                   'span bad))
                         'refusal #f)]
                [else
                 (hasheq 'commands stages
                         'canonical (string-join stages " && ")
                         'rejections '()
                         'refusal #f)])])]))))
