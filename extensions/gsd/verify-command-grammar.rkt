#lang racket/base
;; verify-command-grammar.rkt — ONE low-level Verify declaration grammar.
;;
;; This module owns every token-level decision: the fail-closed shell chain
;; splitter, the accepted/rejected first-token vocabulary, span
;; canonicalization, and the closed operator-reason taxonomy. It is
;; deliberately NOT a shell parser: ambiguous scripts fail closed, quoted
;; delimiters are never reinterpreted as stage boundaries, and every
;; command-shaped span either becomes an executable marker or is refused by
;; name.
;;
;; `verify-command-format.rkt` (declaration LAYOUT: bullet / single-line /
;; unfenced-multiline) re-exports these bindings and delegates every token
;; decision here, so there is exactly one grammar. Live execution, the frozen
;; offline binding and the review-proof retention layer all reach the same
;; kernel.
;;
;; Pure: `racket/string` + `racket/list` only. No filesystem, process or
;; parameter authority.
(require racket/string
         racket/list)

;; ---- shell chain grammar ----
(provide verify-command-stages
         canonical-verify-command
         ;; ---- span classification ----
         verify-first-token
         verify-classify-span
         verify-decorated?
         verify-accept-first-tokens
         verify-grep-first-tokens
         verify-unsupported-first-tokens
         verify-normalizer-version
         ;; ---- closed operator-reason taxonomy ----
         verify-operator-reason
         verify-operator-reasons
         verify-reason-kind
         verify-reason-vocabulary
         verify-rejection-reasons
         verify-refusal-reasons)

;; ============================================================
;; Shell chain grammar (fail-closed)
;; ============================================================
;; Split one declaration body into the declared stages of a fail-fast `&&`
;; chain. Returns #f (ambiguous/unsafe) instead of guessing: an unbalanced
;; quote, a quoted delimiter, a continuation, a subshell, a redirection, a
;; backtick/`$(` command substitution, `;`, `|`, a bare `&`, or an
;; English-looking shell keyword stage all refuse the whole declaration.
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

;; Compose the executed fail-fast chain from the declared body. Byte-for-byte
;; passthrough (including the whitespace of the original argument) for the
;; legacy single-command contract; "" exactly when the grammar refuses.
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
;; Span vocabulary and classification
;; ============================================================
;; Bumped when the extraction vocabulary changes semantics. Consumers compare
;; retained normalized authority (never regenerate it); a version change
;; therefore refuses stale rebinding loudly instead of silently
;; reinterpreting a frozen declaration.
(define verify-normalizer-version 2)

;; Accepted first tokens (POSIX sh + this repository's toolchain).
;; `test`/`printf` are explicit: retained wave gates pin both, and their exit
;; semantics are unambiguous (0 pass / nonzero fail — an error can never
;; masquerade as success).
(define verify-accept-first-tokens
  '("racket" "raco"
             "bash"
             "sh"
             "python"
             "python3"
             "pytest"
             "npm"
             "pnpm"
             "yarn"
             "make"
             "git"
             "cd"
             "env"
             "test"
             "printf"))

;; Command-shaped but unsupported: a named rejection — never a silent drop
;; (the historical hazard: a dropped span made a gate appear to pass while
;; checking nothing) and never an automatic rewrite (inverting `grep -L`
;; into `! grep -q` would turn GNU grep's exit-2/multifile semantics into an
;; apparent pass). grep is called out separately because its exit grammar
;; (0 match / 1 no-match / 2 error) is exactly what can hide a failure; the
;; supported negative-content form is an explicit count composition inside a
;; `bash -c` invocation span (`test "$(grep -c TOKEN file)" -eq 0` — present
;; fails, absent passes, missing/unreadable file errors nonzero).
(define verify-grep-first-tokens '("grep" "egrep" "fgrep" "rg"))
(define verify-unsupported-first-tokens
  '("find" "sed"
           "awk"
           "gawk"
           "ls"
           "cat"
           "head"
           "tail"
           "wc"
           "diff"
           "cmp"
           "sort"
           "uniq"
           "tr"
           "cut"
           "xargs"
           "tee"
           "stat"
           "readlink"
           "realpath"
           "which"
           "command"
           "type"
           "sleep"
           "date"
           "basename"
           "dirname"
           "perl"
           "ruby"
           "node"
           "npx"
           "deno"
           "bun"
           "php"
           "lua"
           "zsh"
           "ksh"
           "dash"
           "fish"
           "curl"
           "wget"
           "docker"
           "podman"
           "kubectl"
           "jq"
           "yq"
           "tar"
           "zip"
           "unzip"
           "gzip"
           "gunzip"
           "mkdir"
           "rmdir"
           "rm"
           "cp"
           "mv"
           "ln"
           "chmod"
           "chown"
           "touch"
           "echo"
           "true"
           "false"))

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
    [(or (string-prefix? span "metrics.rkt ") (string-prefix? span "scripts/metrics.rkt "))
     (cons 'accept (verify-metrics-canonical span))]
    [(verify-bracket-command? span) (cons 'accept span)]
    [(member (verify-first-token span) verify-accept-first-tokens)
     (cons 'accept (verify-metrics-canonical span))]
    [(or (string-prefix? span "./") (string-prefix? span "<project-base>")) (cons 'accept span)]
    [(string-prefix? span "!") (cons 'reject 'inverted-command)]
    [(string-prefix? span "[[") (cons 'reject 'bash-only-construct)]
    [(member (verify-first-token span) verify-grep-first-tokens) (cons 'reject 'grep-exit-semantics)]
    [(member (verify-first-token span) verify-unsupported-first-tokens)
     (cons 'reject 'unsupported-command)]
    [(verify-span-metachars? span) (cons 'reject 'ambiguous-shell-span)]
    [else 'prose]))

(define (verify-decorated? s)
  (regexp-match? #px"(^|\n)\\s*[-*+]\\s" s))

;; ============================================================
;; Closed operator-reason taxonomy
;; ============================================================
;; One stable named vocabulary shared by live execution, the frozen offline
;; binding, the retention layer and operator surfaces. Two kinds:
;;
;;   'rejection — one authored SPAN voids the declaration (nothing executes)
;;   'refusal   — the DECLARATION SHAPE itself cannot be executed
;;
;; Every entry carries an operator-facing summary, why it fails closed, and a
;; safe next action. ADDITIVE ONLY: adding a reason must not change the shape
;; of an extraction result (see `verify-command-format.rkt`).
(define (reason code kind summary detail next-action)
  (hasheq 'code code 'kind kind 'summary summary 'detail detail 'next-action next-action))

(define verify-operator-reasons
  ;; ---- span-level rejections ----
  (hasheq 'inverted-command
          (reason 'inverted-command
                  'rejection
                  "an inverted command (`! cmd`) is not an accepted gate"
                  (string-append "`!` inverts an arbitrary exit status, so a genuine error "
                                 "(for example grep's exit 2 for a missing file) becomes an "
                                 "apparent pass; the gate would be checking the wrong thing.")
                  (string-append "Express the negative condition inside an explicit `bash -c` span "
                                 "with a checkable count, e.g. "
                                 "`bash -c 'test \"$(grep -c TOKEN file)\" -eq 0'`."))
          'bash-only-construct
          (reason 'bash-only-construct
                  'rejection
                  "`[[ ... ]]` is a bash-only construct and is not an accepted gate"
                  (string-append "The declared chain executes under /bin/sh semantics, where "
                                 "`[[ ... ]]` is unportable and its failure mode is not the one "
                                 "the author tested.")
                  (string-append "Use the POSIX form `[ ... ]` or `test ...`, or an explicit "
                                 "`bash -c '...'` span when bash semantics are required."))
          'grep-exit-semantics
          (reason 'grep-exit-semantics
                  'rejection
                  "a bare grep gate is not accepted (0 match / 1 no-match / 2 error)"
                  (string-append "grep exit 2 (missing, unreadable or malformed input) is "
                                 "indistinguishable from a normal no-match, so a broken check can "
                                 "look green; the grammar refuses instead of rewriting.")
                  (string-append "Declare the content condition explicitly, e.g. "
                                 "`bash -c 'test \"$(grep -c TOKEN file)\" -ge 1'` for presence or "
                                 "`bash -c 'test \"$(grep -c TOKEN file)\" -eq 0'` for absence."))
          'unsupported-command
          (reason 'unsupported-command
                  'rejection
                  "the span's first token is not in the accepted gate vocabulary"
                  (string-append "Unsupported tools have ambiguous exit semantics for a reviewed "
                                 "gate, so the span would either check nothing or fail for the "
                                 "wrong reason; dropping it silently would fake a green.")
                  (string-append "Use an accepted gate command (racket, raco, bash, sh, git, cd, "
                                 "test, printf, ...) or wrap a real check in `bash -c '...'` with "
                                 "explicit PASS/FAIL exit status."))
          'ambiguous-shell-span
          (reason 'ambiguous-shell-span
                  'rejection
                  "the span contains shell metacharacters that its grammar cannot verify"
                  (string-append "`;`, `|`, `&`, `$`, `<` and `>` outside an accepted first token "
                                 "let one span hide several commands or a redirection, so its exit "
                                 "status would not prove the declared check.")
                  (string-append "Write one gate per span, or put the compound script inside an "
                                 "explicit `bash -c '...'` span (quote the script as a single "
                                 "argument)."))
          'prose-in-unfenced-multiline
          (reason 'prose-in-unfenced-multiline
                  'rejection
                  "an unfenced multiline Verify section contains a non-command line"
                  (string-append "English prose in an unfenced section would otherwise be launched "
                                 "through /bin/sh; the author's intent is a bullet or fenced block, "
                                 "so the whole declaration refuses.")
                  (string-append "Convert the criteria to backticked bullets "
                                 "(\"- `racket tests/a.rkt` green\") or an explicit ```sh fence."))
          ;; ---- declaration-shape refusals ----
          'empty-declaration
          (reason 'empty-declaration
                  'refusal
                  "the Verify declaration is empty"
                  (string-append "An empty declaration proves nothing, so executing it would "
                                 "report a green gate that never ran.")
                  "Declare at least one executable gate, or state explicitly that the wave has none.")
          'no-executable-command
          (reason 'no-executable-command
                  'refusal
                  "the declaration shaped no executable gate"
                  (string-append "All authored spans were prose (or absent), and the grammar "
                                 "never substitutes a fallback gate for a malformed declaration.")
                  (string-append "Add at least one backticked executable criterion, e.g. "
                                 "`racket scripts/run-tests.rkt --suite fast`."))
          'ambiguous-grammar
          (reason 'ambiguous-grammar
                  'refusal
                  "the fail-closed shell grammar refused the declaration shape"
                  (string-append "Dangling `&&`, unbalanced or quoted delimiters, continuations, "
                                 "subshells, redirections and keywords cannot be split into "
                                 "verifiable stages, so nothing is executed instead of guessing.")
                  (string-append "Write one gate per backticked bullet, use a ```sh fence for "
                                 "multiline scripts, and avoid `;`, `|`, `$(...)` and `&` in "
                                 "declared spans."))))

;; Structured operator reason for one reason/refusal symbol, or #f for a
;; symbol outside the closed vocabulary (callers must surface the unknown
;; symbol rather than treating it as a pass).
(define (verify-operator-reason code)
  (and (symbol? code) (hash-ref verify-operator-reasons code #f)))

(define (verify-reasons-of-kind kind)
  (sort (for/list ([code (in-list (hash-keys verify-operator-reasons))]
                   #:when (eq? (hash-ref (hash-ref verify-operator-reasons code) 'kind) kind))
          code)
        symbol<?))

(define (verify-reason-kind code)
  (define entry (verify-operator-reason code))
  (and entry (hash-ref entry 'kind)))

(define verify-reason-vocabulary (sort (hash-keys verify-operator-reasons) symbol<?))

(define verify-rejection-reasons (verify-reasons-of-kind 'rejection))
(define verify-refusal-reasons (verify-reasons-of-kind 'refusal))
