#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; test-pr-ops-idempotence.rkt — W6 (BUG-0011)
;; Pure-function tests for gh-pr create lookup-first idempotence,
;; gh-wave-finish already-committed detection, and durable wave
;; checkpoints (read/skip/write-no-op) — no live gh/git required.

(require rackunit
         racket/file
         racket/string
         json
         "../extensions/github/helpers.rkt"
         "../extensions/github/handlers/pr-ops.rkt")

;; ============================================================
;; gh-pr create: lookup-first idempotence (pure pieces)
;; ============================================================

(test-case "pr-lookup-command: targets open PRs for the head branch"
  (check-equal? (pr-lookup-command "feature/w6")
                '("pr" "list"
                       "--state"
                       "open"
                       "--head"
                       "feature/w6"
                       "--limit"
                       "1"
                       "--json"
                       "number,title,url,state")))

(test-case "open-pr-from-lookup: parses an existing open PR"
  (define pr
    (open-pr-from-lookup "[{\"number\":42,\"title\":\"W6\",\"url\":\"u\",\"state\":\"OPEN\"}]"))
  (check-true (hash? pr))
  (check-equal? (hash-ref pr 'number) 42))

(test-case "open-pr-from-lookup: empty array / blank / garbage → #f"
  (check-false (open-pr-from-lookup "[]"))
  (check-false (open-pr-from-lookup ""))
  (check-false (open-pr-from-lookup "   "))
  (check-false (open-pr-from-lookup "not json")))

(test-case "open-pr-from-lookup: JSON object (not array) → #f"
  (check-false (open-pr-from-lookup "{\"number\":42}")))

(test-case "find-open-pr-for-head: lookup failure is fail-closed"
  (check-exn exn:fail?
             (lambda ()
               (find-open-pr-for-head "feature/w6"
                                      #:gh (lambda _args (values 1 "" "lookup unavailable"))))))

;; ============================================================
;; gh-wave-finish: already-committed tree/content check
;; ============================================================

(define (fake-git expected-ec expected-out)
  (lambda args (values expected-ec expected-out "")))

(test-case "wave-already-committed?: clean tree → #t (already applied)"
  (check-true (wave-already-committed? '("a.rkt" "b.rkt") #:git (fake-git 0 ""))))

(test-case "wave-already-committed?: dirty files → #f (needs commit)"
  (check-false (wave-already-committed? '("a.rkt") #:git (fake-git 0 " M a.rkt"))))

(test-case "wave-already-committed?: git status failure is fail-closed"
  (check-exn exn:fail?
             (lambda ()
               (wave-already-committed? '("a.rkt") #:git (fake-git 128 "fatal: not a repo")))))

(test-case "unrelated-staged-paths: permits allowlist and requests raw NUL paths"
  (define seen-args #f)
  (check-equal? (unrelated-staged-paths '("a.rkt" "dir/b.rkt")
                                        #:git (lambda args
                                                (set! seen-args args)
                                                (values 0 "a.rkt\0dir/b.rkt\0" "")))
                '())
  (check-equal? seen-args '("diff" "--cached" "--name-only" "-z" "--")))

(test-case "unrelated-staged-paths: reports pre-staged paths outside allowlist"
  (check-equal? (unrelated-staged-paths '("a.rkt")
                                        #:git (lambda _args (values 0 "a.rkt\0unrelated.txt\0" "")))
                '("unrelated.txt")))

(test-case "unrelated-staged-paths: preserves leading whitespace in path names"
  (check-equal? (unrelated-staged-paths '(" leading.rkt")
                                        #:git (lambda _args (values 0 " leading.rkt\0" "")))
                '()))

(test-case "unrelated-staged-paths: preserves trailing whitespace in path names"
  (check-equal? (unrelated-staged-paths '("trailing.rkt ")
                                        #:git (lambda _args (values 0 "trailing.rkt \0" "")))
                '()))

(test-case "unrelated-staged-paths: preserves Git-special characters without quoting"
  (check-equal? (unrelated-staged-paths '("tab\tname.rkt" "back\\slash.rkt")
                                        #:git (lambda _args
                                                (values 0 "tab\tname.rkt\0back\\slash.rkt\0" "")))
                '()))

(test-case "unrelated-staged-paths: staged lookup failure is fail-closed"
  (check-exn exn:fail?
             (lambda ()
               (unrelated-staged-paths '("a.rkt")
                                       #:git (lambda _args (values 128 "" "cannot inspect index"))))))

;; ============================================================
;; Durable checkpoints in STATE.md
;; ============================================================

(test-case "wave-checkpoint-section: renders header, wave id, checked steps"
  (check-equal? (wave-checkpoint-section "W6" '("s1" "s2"))
                "## Wave checkpoints\n\n### W6\n- [x] s1\n- [x] s2\n"))

(test-case "read-wave-checkpoints: parses wave/step pairs"
  (define content
    "Intro narrative.\n\n## Wave checkpoints\n\n### W6\n- [x] s1\n- [x] s2\n\n### W5\n- [x] old\n")
  (check-equal? (read-wave-checkpoints content) '(("W6" . "s1") ("W6" . "s2") ("W5" . "old"))))

(test-case "read-wave-checkpoints: ignores checked lines outside the section"
  (define content "- [x] stray\n\n## Other\n- [x] nope\n")
  (check-equal? (read-wave-checkpoints content) '()))

(test-case "read-wave-checkpoints: unchecked steps are not recorded"
  (define content "## Wave checkpoints\n\n### W6\n- [ ] todo\n- [x] done\n")
  (check-equal? (read-wave-checkpoints content) '(("W6" . "done"))))

(test-case "wave-step-completed?: true only for recorded wave+step"
  (define content "## Wave checkpoints\n\n### W6\n- [x] s1\n")
  (check-true (wave-step-completed? content "W6" "s1"))
  (check-false (wave-step-completed? content "W6" "s2"))
  (check-false (wave-step-completed? content "W5" "s1")))

(test-case "read-state-content: missing file → empty string"
  (check-equal? (read-state-content "/nonexistent/STATE.md") ""))

;; --- write / resume round-trips against a scratch file ---

(define scratch-dir (make-temporary-file "w6-state-~a" 'directory))
(define scratch-state (build-path scratch-dir "STATE.md"))

(define (reset-scratch! [initial "# State\n\nSome narrative.\n"])
  (when (file-exists? scratch-state)
    (delete-file scratch-state))
  (display-to-file initial scratch-state))

(test-case "write-wave-checkpoint!: creates section + records first step"
  (reset-scratch!)
  (check-equal? (write-wave-checkpoint! scratch-state "W6" "s1") 'recorded)
  (define c1 (file->string scratch-state))
  (check-true (string-contains? c1 "## Wave checkpoints"))
  (check-true (wave-step-completed? c1 "W6" "s1")))

(test-case "write-wave-checkpoint!: second step joins the same wave block"
  (reset-scratch!)
  (write-wave-checkpoint! scratch-state "W6" "s1")
  (write-wave-checkpoint! scratch-state "W6" "s2")
  (define c (file->string scratch-state))
  (check-true (wave-step-completed? c "W6" "s1"))
  (check-true (wave-step-completed? c "W6" "s2"))
  ;; both steps live under one ### W6 header
  (check-equal? (length (regexp-match* #px"(?m:^### W6$)" c)) 1))

(test-case "write-wave-checkpoint!: re-recording is a no-op"
  (reset-scratch!)
  (write-wave-checkpoint! scratch-state "W6" "s1")
  (define before (file->string scratch-state))
  (check-equal? (write-wave-checkpoint! scratch-state "W6" "s1") 'no-op)
  (check-equal? (file->string scratch-state) before))

(test-case "resume semantics: completed steps skipped, next step recorded"
  (reset-scratch!)
  ;; simulated interrupted run completed step 1 of 3
  (write-wave-checkpoint! scratch-state "W6" "step-1")
  (define resumed (file->string scratch-state))
  ;; resumed run consults the checklist instead of re-executing
  (check-true (wave-step-completed? resumed "W6" "step-1"))
  (check-false (wave-step-completed? resumed "W6" "step-2"))
  ;; it executes step-2, checkpoints it, then step-3
  (write-wave-checkpoint! scratch-state "W6" "step-2")
  (write-wave-checkpoint! scratch-state "W6" "step-3")
  (define done (file->string scratch-state))
  (for ([s '("step-1" "step-2" "step-3")])
    (check-true (wave-step-completed? done "W6" s)))
  ;; no duplicate entries survived the two-run split
  (check-equal? (read-wave-checkpoints done)
                '(("W6" . "step-1") ("W6" . "step-2") ("W6" . "step-3"))))

(test-case "write-wave-checkpoint!: distinct waves keep separate blocks"
  (reset-scratch!)
  (write-wave-checkpoint! scratch-state "W5" "a")
  (write-wave-checkpoint! scratch-state "W6" "b")
  (define c (file->string scratch-state))
  (check-true (wave-step-completed? c "W5" "a"))
  (check-true (wave-step-completed? c "W6" "b"))
  (check-false (wave-step-completed? c "W5" "b")))

(delete-directory/files scratch-dir)
