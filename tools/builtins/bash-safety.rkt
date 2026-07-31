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

(require racket/string
         (only-in "../shell-risk.rkt"
                  tokenize-shell-command
                  classify-shell-risks
                  shell-risk-finding-type
                  shell-risk-finding-severity))

(provide destructive-patterns
         destructive-command?
         high-risk-patterns
         high-risk-command?
         structured-destructive-command?
         structured-critical-command?)

;; ── Destructive command patterns (SEC-03, #449) ──
;; Each pattern uses anchors (^|[&;|\n]) or word boundaries to avoid
;; false positives on benign strings like `echo "shutdown notice"`.
;; Patterns are matched case-insensitively against the full command.
(define destructive-patterns
  ;; Recursive / forceful deletion — anchored at command start
  (list
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
   #rx"^git[ ]+clean[ ]+.*-[a-zA-Z]*f" ;; git clean with -f (covers -fd/-fdx/-xdf)
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
   #rx"\\$\\(" ;; $(...) command substitution
   #rx"`[^`]+`" ;; AUDIT-01: paired backtick command substitution (avoids false positives on lone backticks)
   #rx"^eval[ ]+" ;; eval indirection
   #rx"^exec[ ]+" ;; exec replacement
   ))

;; Check if a command matches any destructive pattern.
;; Uses regexp matching for token-awareness to avoid false positives.
(define (destructive-command? command)
  (define lower (string-downcase command))
  (for/or ([pattern (in-list destructive-patterns)])
    (regexp-match? pattern lower)))

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
(define (structured-destructive-command? command)
  (define findings (classify-shell-risks (tokenize-shell-command command)))
  (for/or ([f (in-list findings)])
    (member
     (shell-risk-finding-type f)
     '(destructive high-risk windows-destructive network-pipe command-substitution eval exec))))

;; SEC-1 (v0.99.76 W1): fail-closed structured check — any CRITICAL-severity
;; finding blocks execution. Used by the worker (which has no interactive
;; approval channel) to catch obfuscated commands the regex blocklist misses.
(define (structured-critical-command? command)
  (define findings (classify-shell-risks (tokenize-shell-command command)))
  (for/or ([f (in-list findings)])
    (eq? (shell-risk-finding-severity f) 'critical)))
