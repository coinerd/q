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
  (list
   ;; Recursive / forceful deletion — anchored at command start
   #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f flags
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
(define mutating-body-verbs
  '(rm rmdir mv dd mkfs truncate mkfile shutdown reboot))

(define (first-word-of text)
  ;; #px + \s: this Racket build's #rx engine does not honor POSIX
  ;; [[:space:]] classes (verified empirically); #px does.
  (define trimmed (string-trim text))
  (and (> (string-length trimmed) 0)
       (let ([m (regexp-match #px"^[^\\s;|&]+" trimmed)])
         (and m
              (string-downcase
               (list->string
                (for/list ([c (in-string (car m))]
                           #:unless (memq c '(#\" #\' #\\)))
                  c)))))))

;; sed with in-place editing mutates a file; streaming sed does not.
;; (#px for \s — see first-word-of note.)
(define (sed-in-place? segment)
  (and (equal? (first-word-of segment) "sed")
       (regexp-match? #px"(^|\\s)-\\S*i(\\s|$)|--in-place(\\s|$)"
                      segment)))

;; chmod/chown count as body mutation only for blanket/recursive modes.
(define (permission-mutation? segment)
  (define verb (first-word-of segment))
  (and (or (equal? verb "chmod") (equal? verb "chown"))
       (regexp-match? #px"777|000|(^|\\s)-r" segment)))

;; Redirection into a real file (anything other than /dev/null or an fd dup
;; like 2>&1) is mutation evidence. (#px for \s — see first-word-of note.)
(define (redirects-to-file? text)
  (for/or ([m (in-list (regexp-match* #px">+\\s*(\\S+)" text #:match-select cadr))])
    (define target (string-trim m))
    (not (or (equal? target "/dev/null")
             (string-prefix? target "&")
             (string=? target "")))))

;; Heredocs write files (or feed mutation streams); treat as evidence.
(define (heredoc-write? text)
  (regexp-match? #rx"<<-?[ ]*[\"']?[A-Za-z_]" text))

;; Inner text of every `$( ... )` span; allows one level of nested parens
;; (arithmetic `$(())`) but stops before pathological nesting.
(define (substitution-bodies text)
  (for/list ([m (in-list
                 (regexp-match*
                  #rx"\\$\\(([^()]*(?:\\([^()]*\\)[^()]*)*)\\)"
                  text
                  #:match-select cadr))])
    m))

;; Backtick spans (non-nested by definition).
(define (backtick-bodies text)
  (for/list ([m (in-list (regexp-match* #rx"`([^`]*)`" text #:match-select cadr))])
    m))

;; Bounded for/while/until loop bodies (`do ... done`), non-greedy.
(define (loop-bodies text)
  (for/list ([m (in-list (regexp-match* #px"(?s:do[ ](.*?)done)" text
                                        #:match-select cadr))])
    m))

;; Mutation evidence directly in one text's segments (split on ; | & \n).
(define (segment-mutation-reason text)
  (for/or ([seg (in-list (string-split text #rx"[;|&\n]"))])
    (cond
      [(pattern-matches-destructive? seg) 'destructive-pattern-in-body]
      [(equal? (first-word-of seg) "sed") (and (sed-in-place? seg) 'sed-in-place)]
      [(permission-mutation? seg) 'permission-mutation]
      [(redirects-to-file? seg) 'redirection]
      [(heredoc-write? seg) 'heredoc-write]
      [(and (first-word-of seg)
            (memq (string->symbol (first-word-of seg)) mutating-body-verbs))
       'mutating-verb]
      [else #f])))

;; Recursively scan nested bodies (bounded depth 6) for mutation evidence.
(define (nested-mutation-reason text [depth 6])
  (and (> depth 0)
       (or (segment-mutation-reason text)
           (for/or ([body (in-list (append (substitution-bodies text)
                                           (backtick-bodies text)
                                           (loop-bodies text)))])
             (nested-mutation-reason body (sub1 depth))))))

;; Named rejection reason (BUG-0054 action 5): 'none when clean; otherwise a
;; pattern string or evidence symbol naming the actual destructive token.
(define (destructive-reason command)
  (define lower (string-downcase command))
  (define pattern-hit
    (for/or ([pattern (in-list destructive-patterns)])
      (and (regexp-match? pattern lower) pattern)))
  (cond
    [pattern-hit (format "~a" pattern-hit)]
    [(nested-mutation-reason command) => values]
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
    (member
     (shell-risk-finding-type f)
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
