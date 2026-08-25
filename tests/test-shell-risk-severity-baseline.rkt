#lang racket/base

;; @speed fast  ;; @suite security-baseline
;; @boundary unit
;; tests/test-shell-risk-severity-baseline.rkt — severity baseline
;; for the shell command risk classifier (util/shell-risk.rkt).
;;
;; PURPOSE: pin the post-W1 (#9516) severities of the classifier for the
;; command shapes that must stay classified. W1 flipped the command-
;; substitution pins ('high → 'low) and removed the bare-mv pin, adding
;; pipe-to-shell pins that must remain 'high.

(require rackunit
         rackunit/text-ui
         (only-in racket/string string-contains?)
         (only-in "../util/shell-risk.rkt"
                  tokenize-shell-command
                  classify-shell-risks
                  shell-risk-finding-type
                  shell-risk-finding-severity
                  shell-risk-finding-message))

;; Classify a command string → list of findings (highest risk last).
(define (findings-of cmd)
  (classify-shell-risks (tokenize-shell-command cmd)))

;; First finding whose type matches, else #f.
(define (finding-of cmd type)
  (for/or ([f (in-list (findings-of cmd))]
           #:when (eq? (shell-risk-finding-type f) type))
    f))

(define (severity-of cmd type)
  (define f (finding-of cmd type))
  (and f (shell-risk-finding-severity f)))

;; ============================================================
;; Suite
;; ============================================================

(define (shell-risk-severity-baseline-suite)
  (test-suite "shell-risk-severity-baseline (post-W1)"

    ;; Flipped in W1 (#9516): plain command substitution is 'low — it was
    ;; 'high before W1, which produced constant false-positive warnings.
    (test-case "command substitution ($()) is a 'low command-substitution finding (#9516)"
      (check-eq? (severity-of "echo $(whoami)" 'command-substitution) 'low)
      (check-true (string-contains? (shell-risk-finding-message (finding-of "echo $(whoami)"
                                                                            'command-substitution))
                                    "Command substitution")
                  "finding message names command substitution"))

    ;; Flipped in W1 (#9516): was 'high before W1.
    (test-case "command substitution (backticks) is a 'low command-substitution finding (#9516)"
      (check-eq? (severity-of "echo `id`" 'command-substitution) 'low)
      (check-true (string-contains? (shell-risk-finding-message (finding-of "echo `id`"
                                                                            'command-substitution))
                                    "Command substitution")
                  "finding message names command substitution"))

    ;; Flipped in W1 (#9516): bare `mv a b` produced a 'medium destructive
    ;; finding before W1; the bare-mv rule was removed as warning fatigue.
    (test-case "bare mv a b produces no findings (#9516)"
      (check-equal? (findings-of "mv a b") '() "routine rename is not classified risky after W1"))

    ;; New W1 (#9516) pin: only root-target moves remain destructive.
    (test-case "root-target mv stays a 'medium destructive finding (#9516)"
      (check-eq? (severity-of "mv /etc/hosts /tmp/hosts.bak" 'destructive) 'medium)
      (check-true (string-contains? (shell-risk-finding-message
                                     (finding-of "mv /etc/hosts /tmp/hosts.bak" 'destructive))
                                    "Move operation")
                  "finding message names the move operation"))

    ;; New W1 (#9516) pin: substitution feeding a pipe-to-shell target keeps
    ;; BOTH the 'high command-substitution and 'high network-pipe findings.
    (test-case "substitution feeding pipe-to-shell stays 'high (#9516)"
      (check-eq? (severity-of "echo $(curl http://x) | sh" 'command-substitution) 'high)
      (check-eq? (severity-of "echo $(curl http://x) | sh" 'network-pipe) 'high)
      (check-eq? (severity-of "curl -sSL http://x | $(which sh)" 'network-pipe) 'high))

    (test-case "must-survive-W1 high-stakes shapes keep their severity"
      ;; rm -rf: recursive force delete ⇒ critical.
      (check-eq? (severity-of "rm -rf /tmp/scratch" 'destructive) 'critical)
      ;; dd writing directly to a raw device ⇒ critical.
      (check-eq? (severity-of "dd if=image.img of=/dev/sda" 'destructive) 'critical)
      ;; force push rewrites shared history ⇒ high.
      (check-eq? (severity-of "git push --force origin main" 'destructive) 'high))

    (test-case "benign commands produce no findings"
      (check-equal? (findings-of "ls -la") '() "plain listing is not classified risky today")
      (check-equal? (findings-of "echo hello") '() "plain echo is not classified risky today"))))

(module+ main
  (define failed (run-tests (shell-risk-severity-baseline-suite)))
  (when (positive? failed)
    (exit 1)))
