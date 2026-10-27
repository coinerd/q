#lang racket
;; @covers extensions/gsd/prompts.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: integration

;; tests/test-gsd-prompts.rkt — Prompt template tests
;;
;; Wave 3a: Verify all prompt templates contain required content.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         "../extensions/gsd/prompts.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-executor.rkt"
         "../extensions/gsd/wave-docs.rkt"
         "../tools/scratch.rkt"
         (only-in "../util/safe-mode/safe-mode-state.rkt"
                  current-safe-mode-config
                  make-safe-mode-config))

(test-case "retry context supplements rather than masks durable verifier diagnostics"
  (check-false (wave-retry-context #f #f))
  (check-equal? (wave-retry-context "verify failure" #f) "verify failure")
  (check-equal? (wave-retry-context #f "provider retry") "provider retry")
  (check-equal? (wave-retry-context "verify failure" "provider retry")
                "verify failure\nprovider retry"))

;; ============================================================
;; Planning prompt (W0: renamed from exploring-prompt)
;; ============================================================

(test-case "planning-prompt includes planning instructions"
  (define p (planning-prompt "Fix the bug"))
  (check-true (or (string-contains? p "Plan") (string-contains? p "plan")) "mentions plan")
  (check-true (string-contains? p "planning-write") "mentions planning-write"))

(test-case "planning-prompt includes user request"
  (define p (planning-prompt "Fix the login bug"))
  (check-true (string-contains? p "Fix the login bug")))

(test-case "planning-prompt works with empty request"
  (define p (planning-prompt ""))
  (check-true (string? p))
  (check-false (string-contains? p "User request:")))

(test-case "planning-prompt enforces write-immediately workflow"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "IMMEDIATELY write") "mentions write-immediately"))

(test-case "R96: planning-prompt read rules are bounded and honest"
  (define p (planning-prompt "test"))
  ;; no absolute full-file promises (BUG-0096)
  (check-false (string-contains? p "full file") "no full-file promise")
  (check-false (string-contains? p "no offset needed") "no no-offset promise")
  ;; bounded pages + follow notices
  (check-true (string-contains? p "bounded") "mentions bounded reads")
  (check-true (string-contains? p "truncation notice") "mentions truncation notices")
  (check-true (string-contains? p "offset") "mentions offset continuation")
  ;; rereading for continuation is allowed; no NEVER-reread wording
  (check-false (string-contains? p "NEVER re-read") "no absolute never-reread rule")
  (check-true (string-contains? p "rereading for continuation")
              "rereading for continuation is allowed"))

(test-case "R96: planning-implement-prompt keeps read-before-edit without full-file promises"
  (check-true (string-contains? planning-implement-prompt "Read each target file BEFORE editing")
              "read-before-edit rule retained")
  (check-false (string-contains? planning-implement-prompt "full file") "no full-file promise")
  (check-false (string-contains? planning-implement-prompt "NEVER re-read")
               "no absolute never-reread rule"))

(test-case "W0: planning-prompt contains - File: format instruction"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "- File: <path>") "shows exact File syntax"))

(test-case "W0: planning-prompt contains SEPARATE instruction"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "SEPARATE") "emphasizes separate files per wave"))

(test-case "W0: planning-prompt contains STEP 1-4 structure"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "STEP 1") "has step 1")
  (check-true (string-contains? p "STEP 2") "has step 2")
  (check-true (string-contains? p "STEP 3") "has step 3")
  (check-true (string-contains? p "STEP 4") "has step 4"))

(test-case "W0: planning-prompt warns against quarantined gh-wave-finish"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "gh-wave-finish") "mentions gh-wave-finish")
  (check-true (string-contains? p "QUARANTINED") "warns that gh-wave-finish is quarantined")
  (check-true (string-contains? p "external authenticated PR workflow")
              "points to the external finalization authority"))

(test-case "planning-prompt mentions planning-read tool"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "planning-read") "mentions planning-read"))

(test-case "planning-prompt includes filesystem path guidance"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "filesystem path") "mentions filesystem path"))

(test-case "planning-prompt wave template is language-agnostic"
  (define p (planning-prompt "test"))
  (check-false (string-contains? p "q/path/to") "no q/ hardcoded path")
  (check-false (string-contains? p "raco test") "no raco test hardcoded"))

;; ============================================================
;; BUG-0095 (#9854): the emitted mandatory wave-doc template must pass
;; canonical lint WITHOUT test-added repairs. planning-write stores the
;; author's content VERBATIM, so the exact bytes the prompt instructs
;; the author to emit must already carry the sanctioned machine header
;; (one `# Wave N` H1 + exactly one `Status: Inbox` line).
;; ============================================================

(define (string-index-of s sub [from 0])
  (define len (string-length sub))
  (let loop ([i from])
    (cond
      [(> (+ i len) (string-length s)) #f]
      [(string=? (substring s i (+ i len)) sub) i]
      [else (loop (add1 i))])))

;; Extract the exact fenced mandatory wave-doc template emitted by
;; planning-prompt — byte-for-byte, with NO test-added repairs.
(define (emitted-wave-doc-template)
  (define p (planning-prompt "test"))
  (define m (regexp-match-positions #rx"### Wave Doc Format[^\n]*\n```\n" p))
  (check-true (pair? m) "wave-doc format fence found in planning-prompt")
  (if (not m)
      #f
      (let ([body-start (cdr (car m))]
            [close (string-index-of p "\n```" (cdr (car m)))])
        (check-not-false close "closing fence found")
        (and close (substring p body-start close)))))

(define (count-lines-matching s rx)
  (count (lambda (line) (regexp-match? rx line)) (string-split s "\n")))

(test-case "BUG-0095: emitted template carries the sanctioned machine header"
  (define tpl (emitted-wave-doc-template))
  (check-true (string? tpl))
  (when (string? tpl)
    (check-equal? (count-lines-matching tpl #rx"^# Wave [0-9]+$") 1 "exactly one # Wave N H1")
    (check-equal? (count-lines-matching tpl #px"^Status:") 1 "exactly one Status line")
    (check-true (string-prefix? tpl "# Wave 0\nStatus: Inbox\n")
                "header is the document prefix: H1 then Status: Inbox")))

(test-case "BUG-0095: emitted template names the wave-index substitution rule"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "SUBSTITUTE the wave index")
              "explicit wave-index substitution instruction")
  (check-true (string-contains? p "# Wave 2") "shows a substituted nonzero-wave example"))

(test-case "BUG-0095: emitted template (W0 literal) lints clean with zero violations"
  (define tpl (emitted-wave-doc-template))
  (check-true (string? tpl))
  (when (string? tpl)
    (define doc (parse-wave-doc-from-string tpl 0 "short-title"))
    (check-true (hash-ref doc 'status-header?) "managed status header present")
    (check-equal? (hash-ref doc 'status) "Inbox")
    (define violations (lint-wave-doc doc))
    (check-equal? (map wave-doc-violation-section violations)
                  '()
                  "canonical lint-wave-doc reports zero violations")))

(test-case "BUG-0095: substituted nonzero-wave template (W2) lints clean"
  (define tpl (emitted-wave-doc-template))
  (check-true (string? tpl))
  (when (string? tpl)
    (define tpl2 (string-replace tpl "# Wave 0" "# Wave 2"))
    (define doc (parse-wave-doc-from-string tpl2 2 "short-title"))
    (check-true (hash-ref doc 'status-header?) "managed status header present")
    (check-equal? (hash-ref doc 'status) "Inbox")
    (check-equal? (count-lines-matching tpl2 #rx"^# Wave [0-9]+$") 1 "still exactly one H1")
    (check-equal? (map wave-doc-violation-section (lint-wave-doc doc))
                  '()
                  "canonical lint-wave-doc reports zero violations")))

(test-case "BUG-0095: emitted template keeps the executor-contract sections"
  (define tpl (emitted-wave-doc-template))
  (check-true (string? tpl))
  (when (string? tpl)
    (check-true (string-contains? tpl "## Files") "Files section present")
    (check-true (string-contains? tpl "- File: ") "File bullet syntax present")
    (check-true (string-contains? tpl "## Verify") "Verify section present")
    (check-true (string-contains? tpl "## Done") "Done section present")))

;; ============================================================
;; Executing prompt
;; ============================================================

(test-case "executing-prompt includes wave count"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Setup" 'pending "" '("a.rkt") '() "test" '())
                    (gsd-wave 1 "Fix" 'pending "" '("b.rkt") '() "test" '())
                    (gsd-wave 2 "Verify" 'pending "" '("c.rkt") '() "test" '()))
              ""
              '()
              '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "3 waves")))

(test-case "executing-prompt mentions wave-skip"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "/skip") "mentions skip"))

(test-case "executing-prompt includes error recovery"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "Error recovery") "mentions error recovery"))

(test-case "executing-prompt mentions /replan"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "/replan") "mentions replan"))

(test-case "executing-prompt includes edit rules (#2165)"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "Edit rules") "mentions edit rules")
  (check-true (string-contains? p "\u226420") "mentions 20-line limit")
  (check-true (string-contains? p "\u22642000") "mentions 2000-char limit"))

;; ============================================================
;; Wave failure prompt
;; ============================================================

(test-case "wave-failure-prompt includes wave number"
  (define p (wave-failure-prompt 2 "compile error"))
  (check-true (string-contains? p "Wave 2")))

(test-case "wave-failure-prompt includes reason"
  (define p (wave-failure-prompt 3 "test failure"))
  (check-true (string-contains? p "test failure")))

(test-case "wave-failure-prompt mentions skip"
  (define p (wave-failure-prompt 1 "error"))
  (check-true (string-contains? p "/skip")))

(test-case "wave-failure-prompt handles #f reason"
  (define p (wave-failure-prompt 0 #f))
  (check-true (string-contains? p "Wave 0")))

;; ============================================================
;; Verifying prompt
;; ============================================================

(test-case "verifying-prompt includes PASS/FAIL"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "PASS") "mentions PASS")
  (check-true (string-contains? p "FAIL") "mentions FAIL"))

(test-case "verifying-prompt includes verify commands"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test a" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "raco test a")))

(test-case "verifying-prompt handles no verify command"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "no verify command")))

(test-case "verification repair context preserves exact diagnostics and requires complete verify"
  (define p
    (verification-repair-context-block
     "cmd=racket tests/failing.rkt exit=1 state=failed log=/tmp/verify.log\nfailed-output-summary:\nactual 14 expected 12"))
  (check-true (string-contains? p "cmd=racket tests/failing.rkt"))
  (check-true (string-contains? p "log=/tmp/verify.log"))
  (check-true (string-contains? p "actual 14 expected 12"))
  (check-true (string-contains? p "COMPLETE declared Verify chain"))
  (check-true (string-contains? p "declared Files and Done criteria"))
  (check-true (string-contains? p "Do not change the tests or Verify command merely to hide")))

(test-case "verification repair context bounds adversarially large diagnostics"
  (define p (verification-repair-context-block (make-string 7000 #\x)))
  (check-true (string-contains? p "diagnostics truncated"))
  (check-true (< (string-length p) 7000)))

;; ============================================================
;; Status prompt
;; ============================================================

(test-case "status-prompt shows mode"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (status-prompt 'executing exec))
  (check-true (string-contains? p "executing")))

(test-case "status-prompt shows summary"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (wave-complete! exec 0)
  (define p (status-prompt 'executing exec))
  (check-true (string-contains? p "Completed")))

;; ============================================================
;; W2 scratch guidance (one trusted session root)
;; ============================================================

(test-case "scratch-guidance-section renders the one trusted session root"
  (define base (make-temporary-file "q-gsd-scratch-prompt-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-scratch-base-dir base])
       (reset-session-scratch-registry!)
       ;; The runtime already allocated this session's root; the
       ;; renderer PRESERVES it instead of moving it.
       (define allocated (allocate-session-scratch-root! "0123456789abcdef" "0123456789abcdef"))
       (define expected (path->string allocated))
       (define section
         (scratch-guidance-section "0123456789abcdef" "/tmp/proj/.planning/scratch/legacy-token"))
       (check-true (string-contains? section (format "Path: `~a`" expected)))
       (check-false (string-contains? section ".planning/scratch")
                    "the obsolete .planning scratch path is not advertised")
       (check-true (directory-exists? expected))
       (check-equal? (file-or-directory-permissions expected 'bits) #o700)
       (check-true (string-contains? section "write"))
       (check-true (string-contains? section "edit"))
       (check-true (string-contains? section "sanctioned"))
       (check-true (string-contains? section "disposable"))
       (check-true (string-contains? section "deliverable"))
       ;; A trusted session root supplied by a migrated caller wins.
       (define trusted (path->string (build-path base "fedcba9876543210")))
       (define section2 (scratch-guidance-section "0123456789abcdef" trusted))
       (check-true (string-contains? section2 trusted))
       (check-false (string-contains? section2 expected))
       (reset-session-scratch-registry!)))
   (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "scratch-guidance-section never falls back to the scratch base"
  ;; W2: without a trusted root or an opaque token there is no scratch
  ;; allocation for this session, so no scratch destination is advertised. The
  ;; base container is never rendered as if it were a session root.
  (define base (make-temporary-file "q-gsd-scratch-prompt-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-scratch-base-dir base])
                    (reset-session-scratch-registry!)
                    (define section (scratch-guidance-section #f #f))
                    (check-equal? section "" "no trusted root, no advertised scratch path")
                    (check-false (string-contains? section (path->string base)))
                    (reset-session-scratch-registry!)))
                (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "scratch guidance states safe mode is read-only instead of promising writes"
  ;; W2 review: safe mode blocks bash/edit/write and the structured tools refuse
  ;; the scratch root. The prompt must not instruct a safe-mode executor to
  ;; write there; a normal session renders the section unchanged (no note).
  (define base (make-temporary-file "q-gsd-scratch-safe-~a" 'directory))
  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t #:project-root base)])
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-scratch-base-dir base])
         (reset-session-scratch-registry!)
         (define section (scratch-guidance-section "0123456789abcdef" #f))
         (check-true (string-contains? section
                                       (format "Path: `~a`"
                                               (path->string (allocated-session-scratch-root
                                                              "0123456789abcdef")))))
         (check-true (string-contains? section "Safe mode is read-only")
                     "safe mode explicitly disclaims writable scratch")
         (check-false (regexp-match? #px"Create diagnostic scripts" section)
                      "no write instruction is issued in safe mode")
         (reset-session-scratch-registry!)))
     (lambda () (delete-directory/files base #:must-exist? #f)))))
