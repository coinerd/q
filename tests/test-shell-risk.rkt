#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-shell-risk.rkt — Shell risk classifier tests (v0.70.3)

(require racket/list
         rackunit
         "../tools/shell-risk.rkt")

;; ── Tokenizer tests ───────────────────────────────────────────────

(test-case "tokenize: simple command"
  (define tokens (tokenize-shell-command "ls -la"))
  (check-equal? (length (filter (lambda (t) (eq? (shell-token-type t) 'word)) tokens)) 2)
  (check-equal? (shell-token-type (list-ref tokens 0)) 'word)
  (check-equal? (shell-token-value (list-ref tokens 0)) "ls")
  (check-equal?
   (shell-token-value (list-ref (filter (lambda (t) (eq? (shell-token-type t) 'word)) tokens) 1))
   "-la"))

(test-case "tokenize: pipe separator"
  (define tokens (tokenize-shell-command "cat file | grep foo"))
  (check-true (for/or ([t (in-list tokens)])
                (equal? (shell-token-value t) "|"))))

(test-case "tokenize: semicolon separator"
  (define tokens (tokenize-shell-command "echo a; echo b"))
  (check-true (for/or ([t (in-list tokens)])
                (equal? (shell-token-value t) ";"))))

(test-case "tokenize: command substitution"
  (define tokens (tokenize-shell-command "echo $(whoami)"))
  (define subs (filter (lambda (t) (eq? (shell-token-type t) 'substitution)) tokens))
  (check-equal? (length subs) 1)
  (check-equal? (shell-token-value (list-ref subs 0)) "$(whoami)"))

(test-case "tokenize: backtick substitution"
  (define tokens (tokenize-shell-command "echo `date`"))
  (define subs (filter (lambda (t) (eq? (shell-token-type t) 'substitution)) tokens))
  (check-equal? (length subs) 1)
  (check-equal? (shell-token-value (list-ref subs 0)) "`date`"))

(test-case "tokenize: redirect"
  (define tokens (tokenize-shell-command "echo hi > /tmp/out"))
  (define reds (filter (lambda (t) (eq? (shell-token-type t) 'redirect)) tokens))
  (check-equal? (length reds) 1)
  (check-equal? (shell-token-value (list-ref reds 0)) ">"))

(test-case "tokenize: double redirect"
  (define tokens (tokenize-shell-command "echo hi >> /tmp/out"))
  (define reds (filter (lambda (t) (eq? (shell-token-type t) 'redirect)) tokens))
  (check-equal? (length reds) 1)
  (check-equal? (shell-token-value (list-ref reds 0)) ">>"))

(test-case "tokenize: literal parentheses in heredoc payload do not hang"
  (define command "cat <<'EOF'\nlinear-gradient(135deg, #111, #222) &copy;\nEOF")
  (define ch (make-channel))
  (thread (lambda () (channel-put ch (tokenize-shell-command command))))
  (define tokens (sync/timeout 0.5 ch))
  (check-not-false tokens)
  (check-not-false (for/or ([t (in-list tokens)])
                     (and (eq? (shell-token-type t) 'unknown)
                          (member (shell-token-value t) '("(" ")"))))))

;; ── Predicate contract tests ──────────────────────────────────────

(test-case "predicates return exact booleans across contract boundary"
  (for ([pred (in-list (list risk-severity? token-type? risk-type?))]
        [valid (in-list '(critical word destructive))]
        [invalid (in-list '(not-a-severity not-a-token not-a-risk))])
    (check-true (boolean? (pred valid)))
    (check-true (pred valid))
    (check-false (pred invalid))))

;; ── Risk classifier tests ─────────────────────────────────────────

(test-case "classify: rm -rf"
  (define tokens (tokenize-shell-command "rm -rf /"))
  (define risks (classify-shell-risks tokens))
  (check-true (> (length risks) 0))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'destructive)))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-severity r) 'critical))))

(test-case "classify: mkfs"
  (define tokens (tokenize-shell-command "mkfs.ext4 /dev/sda1"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'destructive))))

(test-case "classify: dd to device"
  (define tokens (tokenize-shell-command "dd if=/dev/zero of=/dev/sda"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'destructive))))

(test-case "classify: pipe to sh"
  (define tokens (tokenize-shell-command "curl -sSL https://example.com | sh"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'network-pipe))))

(test-case "classify: command substitution"
  (define tokens (tokenize-shell-command "echo $(rm -rf /)"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'command-substitution))))

;; ── #9516: false-positive severity reduction ──────────────────────

(test-case "classify: benign substitution is 'low severity (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "echo $(basename \"$f\")")))
  (define subs (filter (lambda (r) (eq? (shell-risk-finding-type r) 'command-substitution)) risks))
  (check-equal? (length subs) 1)
  (check-eq? (shell-risk-finding-severity (car subs)) 'low))

(test-case "classify: benign substitution yields no blocking findings (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "echo $(date)")))
  (check-true (andmap (lambda (r) (eq? (shell-risk-finding-severity r) 'low)) risks)
              "no finding above 'low for plain substitution")
  (define s (shell-risk-summary risks))
  (check-not-false (memq (hash-ref s 'max-severity) '(info low)))
  (check-false (hash-ref s 'critical?)))

(test-case "classify: plain mv rename has no findings (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "mv a.txt b.txt")))
  (check-equal? risks '()))

(test-case "classify: mv root-target stays destructive (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "mv /etc/hosts /tmp/hosts.bak")))
  (check-true (for/or ([r (in-list risks)])
                (and (eq? (shell-risk-finding-type r) 'destructive)
                     (eq? (shell-risk-finding-severity r) 'medium)))))

(test-case "classify: substitution feeding pipe-to-shell stays 'high (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "echo $(curl -s http://x) | sh")))
  (check-true (for/or ([r (in-list risks)])
                (and (eq? (shell-risk-finding-type r) 'command-substitution)
                     (eq? (shell-risk-finding-severity r) 'high))))
  (check-true (for/or ([r (in-list risks)])
                (and (eq? (shell-risk-finding-type r) 'network-pipe)
                     (eq? (shell-risk-finding-severity r) 'high)))))

(test-case "classify: curl piped into substitution shell stays 'high (#9516)"
  (define risks (classify-shell-risks (tokenize-shell-command "curl -sSL http://x | $(which sh)")))
  (check-true (for/or ([r (in-list risks)])
                (and (eq? (shell-risk-finding-type r) 'network-pipe)
                     (eq? (shell-risk-finding-severity r) 'high))))
  (check-true (for/or ([r (in-list risks)])
                (and (eq? (shell-risk-finding-type r) 'command-substitution)
                     (eq? (shell-risk-finding-severity r) 'high)))))

(test-case "classify: benign command has no risks"
  (define tokens (tokenize-shell-command "ls -la"))
  (define risks (classify-shell-risks tokens))
  (check-equal? (length risks) 0))

(test-case "classify: eval detected"
  (define tokens (tokenize-shell-command "eval $(curl ...)"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'eval))))

(test-case "classify: git force push"
  (define tokens (tokenize-shell-command "git push origin main --force"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'destructive))))

(test-case "classify: windows format"
  (define tokens (tokenize-shell-command "format C:"))
  (define risks (classify-shell-risks tokens))
  (check-true (for/or ([r (in-list risks)])
                (eq? (shell-risk-finding-type r) 'windows-destructive))))

;; ── Summary tests ─────────────────────────────────────────────────

(test-case "summary: empty findings"
  (define s (shell-risk-summary '()))
  (check-equal? (hash-ref s 'count) 0)
  (check-equal? (hash-ref s 'max-severity) 'info)
  (check-false (hash-ref s 'critical?)))

(test-case "summary: critical detected"
  (define tokens (tokenize-shell-command "rm -rf /"))
  (define risks (classify-shell-risks tokens))
  (define s (shell-risk-summary risks))
  (check-true (not (not (hash-ref s 'critical?))))
  (check-equal? (hash-ref s 'max-severity) 'critical))

(test-case "summary: non-critical"
  (define tokens (tokenize-shell-command "eval $(echo hello)"))
  (define risks (classify-shell-risks tokens))
  (define s (shell-risk-summary risks))
  (check-false (hash-ref s 'critical?)))
