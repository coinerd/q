#lang racket/base

;; tools/builtins/bash-safety.rkt — shared shell safety predicates (SEC-1, v0.99.76 W1)
;;
;; Single source of truth for destructive / high-risk command detection.
;; Imported by:
;;   - tools/builtins/bash.rkt   (main tool-bash path)
;;   - sandbox/worker-tools.rkt  (worker execute-bash / execute-git)
;;
;; Pure functions only — no side effects, no parameters, no tool registry,
;; no sandbox-config coupling. Safe for cross-layer import (worker isolation
;; preserved). See STATE-v0.99.76-SECURITY-CRITICAL.md D1: import, do not
;; duplicate. Adding a pattern here hardens BOTH the main tool and the worker
;; (defense in depth, no relocation).
;;
;; BUG-0054 (v1.00.24 W3): classify behavior, not syntax shapes.
;; Command substitution, backticks, bounded loops, `pgrep`/`ps`/`tail`,
;; `&&`/`||`/`break` are NEUTRAL control syntax: read-only progress polling
;; (e.g. `N=$(grep -c ready log)`; `for i in ...; do ps ...; sleep 2; done`)
;; must pass. Destructive classification derives from mutation EVIDENCE
;; anywhere in the command tree — top level or nested inside a
;; substitution/loop body: destructive regex patterns, mutation verbs
;; (rm/mv/dd/mkfs/truncate/...), sed -i, blanket chmod/chown, file
;; redirection (other than /dev/null), or heredoc writes. Rejections carry a
;; named reason via `destructive-reason`.
;;
;; v1.00.24 W3 (bash safety bypass repairs):
;;   - Anchored destructive checks run on trimmed, whitespace-collapsed,
;;     grouping/wrapper-stripped segments: `echo x;  git push --force`,
;;     `;\tshutdown`, `(rm -rf /)`, `{ git push --force; }`, `! rm -rf /`,
;;     and `sudo rm -rf /` cannot dodge the `^`-anchored patterns.
;;   - `tee` with a file operand (`producer | tee file &`, `tee -a f`) is a
;;     named write reason 'tee-file-write. Process substitutions
;;     (`tee >(cmd)`) are pipes, not files, but their bodies are
;;     evidence-scanned like $().
;;   - Segments detached with a single `&` keep running after the tool result
;;     is produced: detached WRITES are rejected with the underlying write
;;     evidence; detached gate/verification launches are rejected with
;;     'background-gate-launch (an unowned gate verdict is worthless and easy
;;     to fake). Read-only background forms (ps/pgrep/tail/sleep, bounded
;;     read-only loops, `python3 -m http.server`) stay allowed — ampersand is
;;     NOT blanket-banned, and foreground gate runs are unaffected.

(require racket/string
         (only-in "../shell-risk.rkt"
                  tokenize-shell-command
                  classify-shell-risks
                  shell-risk-finding-type
                  shell-risk-finding-severity))

(provide destructive-patterns
         destructive-command?
         destructive-reason
         high-risk-patterns
         high-risk-command?
         structured-destructive-command?
         structured-critical-command?)

;; ── Destructive command patterns (SEC-03, #449) ──
;; Each pattern uses anchors (^|[&;|\n]) or word boundaries to avoid
;; false positives on benign strings like `echo "shutdown notice"`.
;; Patterns are matched case-insensitively against the full command.
;;
;; BUG-0054: the two pure-syntax bypass patterns (#rx"\\$\\(" and the paired
;; backtick pattern) were removed from this list — command substitution is
;; neutral syntax; nested mutation is caught by evidence scanning below.
;; `^eval`/`^exec` remain listed: they are behavior, not shape.
(define destructive-patterns
  ;; Recursive / forceful deletion — anchored at command start
  (list #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f flags
        #rx"^rm[ ]+-rf[ ]+" ;; rm -rf shorthand
        #rx"^rm[ ]+-fr[ ]+" ;; rm -fr shorthand
        #rx"^rm[ ]+-r[ ]+-f[ ]+" ;; rm -r -f
        #rx"^rmdir[ ]+" ;; rmdir
        ;; Also match after pipe/semicolon/&& operators
        #rx"[|;&][ ]*rm[ ]+-rf[ ]+" ;; piped rm -rf
        ;; Disk/filesystem destruction
        #rx"^mkfs[.]" ;; mkfs.*
        #rx"^dd[ ]+if=" ;; dd if=
        #rx"^dd[ ]+.*of=/dev/" ;; dd of=/dev/
        #rx">[ ]*/dev/sd" ;; device file write
        ;; System commands — anchored at command start
        #rx"^shutdown([ ]|$)" ;; shutdown
        #rx"^reboot([ ]|$)" ;; reboot
        #rx"^format[ ]+[A-Za-z]:" ;; Windows format
        #rx"^del[ ]+/" ;; Windows del
        ;; Permission destruction
        #rx"^chmod[ ]+-r[ ]+777[ ]+/" ;; recursive 777 on root
        #rx"^chmod[ ]+000[ ]+/" ;; lock out root
        ;; Pipe-to-shell (must be at pipe boundary)
        #rx"[|][ ]*sh[ ]*$" ;; | sh
        #rx"[|][ ]*bash[ ]*$" ;; | bash
        ;; Critical system file overwrite
        #rx">[ ]*/etc/passwd" ;; passwd overwrite
        #rx">[ ]*/etc/shadow" ;; shadow overwrite
        ;; Git destructive (SEC-1, v0.99.76 W1): force push, destructive clean,
        ;; hard reset. These were previously only blocked in the main tool's
        ;; warn-mode notice; the worker blocks them outright.
        #rx"^git[ ]+push[ ]+.*--force" ;; force push
        #rx"^git[ ]+clean[ ]+.*-[a-zA-Z]*f" ;; git clean with -f
        #rx"^git[ ]+reset[ ]+--hard" ;; hard reset (data loss)
        ;; Root directory operations
        #rx"^mv[ ]+/[ ]+" ;; mv /
        ;; Download-to-shell combos (SEC-A)
        #rx"curl[ ]+.*[|][ ]*sh[ ]*$" ;; curl ... | sh
        #rx"wget[ ]+.*[|][ ]*sh[ ]*$" ;; wget ... | sh
        #rx"eval[ ]+\"[$][(]curl" ;; eval "$(curl ...)"
        #rx"source[ ]+/tmp/" ;; source from temp
        ;; SEC-01 (v0.22.0): Bypass-vector patterns — encoding tricks,
        ;; substitution, and indirection that evade simple pattern matching.
        #rx"[|].*base64" ;; base64 decode pipe bypass
        #rx"[|].*xxd" ;; xxd hex decode pipe bypass
        #rx"^eval[ ]+" ;; eval indirection
        #rx"^exec[ ]+" ;; exec replacement
        ))

(define (pattern-matches-destructive? command)
  (define lower (string-downcase command))
  (for/or ([pattern (in-list destructive-patterns)])
    (regexp-match? pattern lower)))

;; ── BUG-0054: mutation-evidence scanning ─────────────────────────────
;; Conservative denylist of verbs whose presence as a segment command is
;; mutation evidence inside substitution/loop bodies. Read-only tools
;; (grep/wc/ps/pgrep/sleep/tail/cat/echo/ls) never match.
(define mutating-body-verbs '(rm rmdir mv dd mkfs truncate mkfile shutdown reboot))

;; ── v1.00.24 W3: segment normalization helpers ──────────

;; Collapse whitespace runs (space/tab/CR/newline) to single spaces.
;; Anchored destructive patterns use `[ ]`; collapsing denies bypasses like
;; `echo x;\tshutdown` or CR-padded segments. (#px \s: this Racket build's
;; #rx engine does not honor POSIX [[:space:]] classes — verified empirically.)
(define (whitespace-flat text)
  (regexp-replace* #px"\\s+" text " "))

;; Drop shell grouping / negation from segment edges: `(`, `{`, `!` and
;; whitespace. Interior text — including `$(`, backticks, `>(` — is kept
;; intact so the substitution scanners below still see it.
(define (strip-leading-grouping text)
  (regexp-replace* #px"^[({\\s!]+" text ""))

;; Quote characters from token edges.
(define (strip-quote-chars tok)
  ;; Backticks are executable substitution syntax, not inert quote edges;
  ;; preserving them lets command-position checks reject dynamic verbs.
  (regexp-replace* #px"^[\"']+|[\"']+$" tok ""))

;; Verbs that wrap a command without changing what the wrapped command does.
;; Anchored/verb checks look through them so `sudo rm -rf /`,
;; `nohup shutdown -h`, or `timeout 10 mkfs.ext4 ...` cannot dodge detection.
;; Known limitation: flag VALUES are not consumed (e.g. `sudo -u alice rm`);
;; such forms stay undetected rather than risk false positives.
(define command-wrapper-verbs
  '("sudo" "time"
           "env"
           "nohup"
           "setsid"
           "nice"
           "stdbuf"
           "timeout"
           "command"
           "builtin"
           "sh"
           "bash"
           "dash"
           "zsh"))

;; Flags (and for `timeout`, a duration token) that belong to a wrapper verb.
(define (skip-wrapper-args toks wrapper)
  (let skip ([ts toks])
    (cond
      [(null? ts) ts]
      [(and (> (string-length (car ts)) 1) (char=? (string-ref (car ts) 0) #\-)) (skip (cdr ts))]
      [(and (equal? wrapper "timeout") (regexp-match? #px"^[0-9]+(?:\\.[0-9]+)?[smhd]?$" (car ts)))
       (skip (cdr ts))]
      [else ts])))

;; The real command behind a segment: leading grouping chars, `VAR=...`
;; assignments, wrapper verbs, and their flags are stripped. Every iteration
;; consumes at least one token, so arbitrary wrapper depth cannot bypass the
;; classifier and pathological input cannot spin. Tokens are quote-stripped.
(define (command-core text)
  (let loop ([toks (map strip-quote-chars
                        (string-split (strip-leading-grouping (whitespace-flat text))))])
    (cond
      [(null? toks) ""]
      [(member (car toks) command-wrapper-verbs) (loop (skip-wrapper-args (cdr toks) (car toks)))]
      [(and (> (string-length (car toks)) 1) (char=? (string-ref (car toks) 0) #\-))
       (loop (cdr toks))]
      [(regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*=" (car toks)) (loop (cdr toks))]
      [else (string-join toks " ")])))

(define (first-word-of text)
  ;; Command verb after grouping/wrapper stripping; #f when nothing remains.
  (define toks (string-split (command-core text)))
  (and (not (null? toks)) (string-downcase (car toks))))

;; sed with in-place editing mutates a file; streaming sed does not. GNU/BSD
;; backup suffixes (`-i.bak`, `--in-place=.bak`) and short-option clusters
;; still perform an in-place write. Stop option scanning at `--` so an operand
;; such as `-input.txt` is not mistaken for an option.
(define (sed-in-place? segment)
  (and (equal? (first-word-of segment) "sed")
       (let loop ([tokens (cdr (string-split (command-core segment)))])
         (cond
           [(null? tokens) #f]
           [(equal? (car tokens) "--") #f]
           [(regexp-match? #px"^--in-place(?:=.*)?$" (car tokens)) #t]
           [(and (regexp-match? #px"^-[^-]+" (car tokens))
                 (string-contains? (substring (car tokens) 1) "i"))
            #t]
           [else (loop (cdr tokens))]))))

;; chmod/chown count as body mutation only for blanket/recursive modes.
;; Matched case-insensitively so `-R` (uppercase) is not a blind spot.
(define (permission-mutation? segment)
  (define verb (first-word-of segment))
  (and (or (equal? verb "chmod") (equal? verb "chown"))
       ;; Non-recursive chmod 777 on an ordinary path remains high-risk
       ;; warning territory (SEC-1); only lockout/recursive forms are
       ;; destructive evidence here.
       (regexp-match? #px"000|(^|\\s)-r" (string-downcase segment))))

;; Redirection into a real file (anything other than /dev/null, an fd dup,
;; or process substitution) is mutation evidence. Process substitution is a
;; pipe endpoint, not a filesystem target; its body is scanned separately.
(define (redirects-to-file? text)
  (define without-process-substitution (regexp-replace* #rx">\\([^)]*\\)" text ""))
  (for/or ([m (in-list
               (regexp-match* #px">+\\s*(\\S+)" without-process-substitution #:match-select cadr))])
    (define target (string-trim m))
    (not (or (equal? target "/dev/null") (string-prefix? target "&") (string=? target "")))))

;; Heredocs write files (or feed mutation streams); treat as evidence.
(define (heredoc-write? text)
  (regexp-match? #rx"<<-?[ ]*[\"']?[A-Za-z_]" text))

;; Inner text of every `$( ... )` span; allows one level of nested parens
;; (arithmetic `$(())`) but stops before pathological nesting.
(define (substitution-bodies text)
  (for/list ([m (in-list (regexp-match* #rx"\\$\\(([^()]*(?:\\([^()]*\\)[^()]*)*)\\)"
                                        text
                                        #:match-select cadr))])
    m))

;; Backtick spans (non-nested by definition).
(define (backtick-bodies text)
  (for/list ([m (in-list (regexp-match* #rx"`([^`]*)`" text #:match-select cadr))])
    m))

;; Process substitution bodies `>( ... )` — e.g. a tee target that runs a
;; command (v1.00.24 W3). Scanned like $() so nested mutation evidence
;; (`tee >(rm -rf /tmp)`) is caught while benign bodies stay clean.
(define (process-substitution-bodies text)
  (for/list ([m (in-list (regexp-match* #rx">\\(([^()]*(?:\\([^()]*\\)[^()]*)*)\\)"
                                        text
                                        #:match-select cadr))])
    m))

;; Bounded for/while/until loop bodies (`do ... done`), non-greedy.
(define (loop-bodies text)
  (for/list ([m (in-list (regexp-match* #px"(?s:do[ ](.*?)done)" text #:match-select cadr))])
    m))

;; ── v1.00.24 W3: tee with a file operand ──────────────
;; `tee` (and `tee -a`) writes every non-option operand, so
;; `producer | tee file &` persists data after the tool call returns. A
;; file operand is mutation evidence ('tee-file-write). Process
;; substitutions (`tee >(grep x)`) and the bare stdin form (`tee -`) are
;; not file writes; their command bodies are still scanned by the
;; nested-evidence scanner.
(define (tee-file-write? segment)
  (define toks (map string-downcase (string-split (command-core (whitespace-flat segment)))))
  (and (not (null? toks))
       (equal? (car toks) "tee")
       (let loop ([rest (cdr toks)]
                  [end-of-options? #f])
         (cond
           [(null? rest) #f]
           [(and (not end-of-options?) (equal? (car rest) "--")) (loop (cdr rest) #t)]
           ;; bare `-` operand: stdin, not a file
           [(and (not end-of-options?) (equal? (car rest) "-")) (loop (cdr rest) end-of-options?)]
           [(and (not end-of-options?)
                 (> (string-length (car rest)) 1)
                 (char=? (string-ref (car rest) 0) #\-))
            (loop (cdr rest) end-of-options?)]
           ;; process substitution: a pipe, not a file operand
           [(string-prefix? (car rest) ">(") (loop (cdr rest) end-of-options?)]
           [else #t]))))

;; ── v1.00.24 W3: segment splitting that keeps operators ──
;; (list segment following-operator) pairs in order. The operator that
;; TERMINATES a segment is kept: "" (end of text), "&&", "||", ";", "|",
;; "&", or a newline. A segment terminated by a single `&` is detached.
;; A single `&` is an operator only when it is not part of fd redirection
;; (`2>&1`) or the `&>` redirection spelling. Treating the ampersand in an fd
;; duplication as detachment falsely blocked ordinary foreground gates.
(define shell-operator-regex #px"&&|\\|\\||[;|\n]|(?<!>)&(?![&>])")

(define (annotated-segments text)
  (let loop ([from 0]
             [acc '()])
    (define m (regexp-match-positions shell-operator-regex text from))
    (if (not m)
        (reverse (cons (list (substring text from) "") acc))
        (let* ([span (car m)]
               [op (substring text (car span) (cdr span))])
          (loop (cdr span) (cons (list (substring text from (car span)) op) acc))))))

;; Mutation evidence for ONE segment (no operator context). Anchored
;; destructive checks run on the trimmed/whitespace-collapsed text and on
;; the wrapper/grouping-stripped core, so leading spaces, tabs, `(`
;; subshells, `{ ... }` groups, `!` negation or sudo-style wrappers cannot
;; dodge them (v1.00.24 W3).
(define (core-segment-reason seg)
  (define flat (whitespace-flat seg))
  (define trimmed (string-trim flat))
  (and (> (string-length trimmed) 0)
       (let ([core (command-core trimmed)])
         (or (and (pattern-matches-destructive? trimmed) 'destructive-pattern-in-body)
             (and (> (string-length core) 0)
                  (pattern-matches-destructive? core)
                  'destructive-pattern-in-body)
             (and (equal? (first-word-of trimmed) "sed") (sed-in-place? trimmed) 'sed-in-place)
             (and (permission-mutation? trimmed) 'permission-mutation)
             (and (redirects-to-file? trimmed) 'redirection)
             (and (heredoc-write? trimmed) 'heredoc-write)
             (and (tee-file-write? trimmed) 'tee-file-write)
             (let ([verb (first-word-of trimmed)])
               (and verb (memq (string->symbol verb) mutating-body-verbs) 'mutating-verb))))))

;; ── v1.00.24 W3: detached (`&`) launches ─────────────
;; A segment terminated by a single `&` keeps running after the tool call
;; returns: nobody owns it and its result/evidence is never observed.
;; Detached WRITES are rejected with the underlying write evidence;
;; detached gate/verification launches get 'background-gate-launch.
;; Read-only status forms (ps/pgrep/tail/sleep, bounded read-only loops,
;; `python3 -m http.server`) stay allowed; foreground gate runs are
;; unaffected.
(define launch-wrapper-verbs '("nohup" "setsid" "bash" "sh" "dash" "zsh" "env" "time" "nice"))

;; Path/dir components named like a gate: gate.sh, milestone-gate.rkt,
;; run-gate.py, gsd-gates/, run-tests.rkt. Boundary-anchored so
;; `delegate`, `gateway`, `investigate` do not match.
(define (gate-like-name? tok)
  (define clean (strip-quote-chars tok))
  (and (> (string-length clean) 0)
       (for/or ([component (in-list (string-split clean "/"))])
         (define stem (regexp-replace #px"\\.[A-Za-z0-9]+$" component ""))
         (and (> (string-length stem) 0)
              (regexp-match? #px"(^|[-_.])gates?([-_.]|[0-9]|$)|runs?[-_]tests" stem)))))

;; Targets/subcommands that make a runner a verification run
;; (`make check`, `npm test`, `raco test`, `cargo test`, `pytest`, ...).
(define gate-runner-target-regex #px"^(check|test|verify|gates?|ci|coverage)([-_.].*)?$")

(define (gate-runner-form? toks)
  (define head (car toks))
  (define rest (cdr toks))
  (define (target-ish? tok)
    (and (not (string-prefix? tok "-")) (regexp-match? gate-runner-target-regex tok)))
  (cond
    [(equal? head "raco") (and (not (null? rest)) (equal? (car rest) "test"))]
    [(member head '("cargo" "go" "dotnet"))
     (and (not (null? rest)) (member (car rest) '("test" "verify")))]
    [(member head '("make" "rake" "gradle" "mvn" "mill" "bazel"))
     (for/or ([tok (in-list rest)])
       (target-ish? tok))]
    [(member head '("npm" "pnpm" "yarn" "bun" "deno"))
     (for/or ([tok (in-list rest)])
       (target-ish? tok))]
    [(member head '("pytest" "py.test" "tox" "nox" "unittest")) #t]
    [(member head '("python" "python3"))
     (let scan ([ts rest])
       (cond
         [(null? ts) #f]
         [(member (car ts) '("pytest" "py.test" "unittest")) #t]
         [(and (equal? (car ts) "-m")
               (not (null? (cdr ts)))
               (member (cadr ts) '("pytest" "unittest")))
          #t]
         [else (scan (cdr ts))]))]
    [else #f]))

;; Read-only commands whose gate-named ARGUMENTS are files being inspected,
;; not gate launches: `grep error gate.log &` or `tail -f gate.log &`
;; stay allowed.
(define read-only-file-verbs
  '("grep" "cat"
           "tail"
           "head"
           "ls"
           "wc"
           "rg"
           "awk"
           "find"
           "stat"
           "file"
           "less"
           "more"
           "diff"
           "du"
           "strings"))

(define (gate-like-command? seg)
  (let loop ([text (whitespace-flat (command-core seg))]
             [depth 3])
    (define toks
      (for/list ([tok (in-list (string-split text))]
                 #:when (> (string-length (strip-quote-chars tok)) 0))
        (strip-quote-chars tok)))
    (and (not (null? toks))
         (> depth 0)
         (not (member (car toks) read-only-file-verbs))
         (or (for/or ([tok (in-list toks)])
               (gate-like-name? tok))
             (gate-runner-form? toks)
             (and (member (car toks) launch-wrapper-verbs)
                  (loop (string-join (cdr toks) " ") (sub1 depth)))))))

;; Classification of one detached segment: the underlying write evidence
;; when the segment mutates, 'background-gate-launch when it starts
;; unowned gate/verification work, #f when it may run detached.
(define (background-segment-reason seg)
  (define trimmed (string-trim seg))
  (and (> (string-length trimmed) 0)
       (or (core-segment-reason trimmed) (and (gate-like-command? trimmed) 'background-gate-launch))))

;; Mutation evidence anywhere in `text`'s segments, plus detached-segment
;; classification (v1.00.24 W3): a segment terminated by a single `&` runs
;; after the tool result is produced.
(define dynamic-command-position-rx #px"^(?:\\$\\(|`)")

(define (dynamic-command-position-reason text)
  (for/or ([seg+op (in-list (annotated-segments text))])
    (define core (command-core (whitespace-flat (string-trim (car seg+op)))))
    (and (regexp-match? dynamic-command-position-rx core) 'dynamic-command-name)))

(define (segment-mutation-reason text)
  (for/or ([seg+op (in-list (annotated-segments text))])
    (or (core-segment-reason (car seg+op))
        (and (equal? (cadr seg+op) "&") (background-segment-reason (car seg+op))))))

;; Recursively scan nested bodies (bounded depth 6) for mutation evidence.
(define (nested-mutation-reason text [depth 6])
  (and (> depth 0)
       (or (segment-mutation-reason text)
           (for/or ([body (in-list (append (substitution-bodies text)
                                           (backtick-bodies text)
                                           (process-substitution-bodies text)
                                           (loop-bodies text)))])
             (nested-mutation-reason body (sub1 depth))))))

;; Named rejection reason (BUG-0054 action 5): 'none when clean; otherwise a
;; pattern string or evidence symbol naming the actual destructive token.
;; (v1.00.24 W3) The pattern phase matches a trimmed, whitespace-collapsed,
;; wrapper-stripped view so leading spaces/tabs/grouping cannot dodge the
;; `^`-anchored patterns.
(define (destructive-reason command)
  (define lower (string-downcase (command-core (whitespace-flat (string-trim command)))))
  (define pattern-hit
    (for/or ([pattern (in-list destructive-patterns)])
      (and (regexp-match? pattern lower) pattern)))
  (cond
    [pattern-hit (format "~a" pattern-hit)]
    [(dynamic-command-position-reason command)
     =>
     values]
    [(nested-mutation-reason command)
     =>
     values]
    [else 'none]))

;; Check if a command is destructive: top-level pattern list OR mutation
;; evidence anywhere in nested substitution/loop bodies. Neutral control
;; syntax alone (substitution, loops, pgrep/ps/sleep/tail, &&/||) is clean.
(define (destructive-command? command)
  (not (eq? (destructive-reason command) 'none)))

;; ── High-risk patterns (RA-1b, v0.24.7) ──
;; Subset of destructive-patterns that are especially dangerous.
;; When in warn-only mode, these get a stronger notice in tool output.
(define high-risk-patterns
  (list #rx"^rm[ ]+-rf[ ]+" ;; rm -rf
        #rx"^rm[ ]+-fr[ ]+" ;; rm -fr
        #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f
        #rx"^mkfs[.]" ;; mkfs.*
        #rx"^dd[ ]+.*of=/dev/" ;; dd of=/dev/
        #rx"^format[ ]+[A-Za-z]:" ;; Windows format
        #rx">[ ]*/etc/passwd" ;; passwd overwrite
        #rx">[ ]*/etc/shadow")) ;; shadow overwrite

;; Check if a command matches any high-risk pattern.
(define (high-risk-command? command)
  (define lower (string-downcase command))
  (for/or ([pattern (in-list high-risk-patterns)])
    (regexp-match? pattern lower)))

;; Structured classifier source-of-truth helper for user-visible warnings.
;; BUG-0054: command-substitution findings are telemetry here, not blocks.
(define (structured-destructive-command? command)
  (define findings (classify-shell-risks (tokenize-shell-command command)))
  (for/or ([f (in-list findings)])
    (member (shell-risk-finding-type f)
            '(destructive high-risk windows-destructive network-pipe eval exec))))

;; SEC-1 (v0.99.76 W1): fail-closed structured check — any CRITICAL-severity
;; finding blocks execution. Used by the worker (which has no interactive
;; approval channel) to catch obfuscated commands the regex blocklist misses.
;; BUG-0054: a bare command-substitution finding is no longer critical by
;; itself; the regex/evidence scanners above decide whether the nested body
;; actually mutates.
(define (structured-critical-command? command)
  (define findings (classify-shell-risks (tokenize-shell-command command)))
  (for/or ([f (in-list findings)])
    (and (eq? (shell-risk-finding-severity f) 'critical)
         (not (eq? (shell-risk-finding-type f) 'command-substitution)))))
