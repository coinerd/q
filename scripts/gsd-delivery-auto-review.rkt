#lang racket/base

;; S12 native port of the automatic independent review dispatch
;; (`scripts/gsd-delivery.py` action `auto-review`, phases `prepare` and
;; `finalize`).
;;
;; The Python action is the behaviour contract: every refusal message and
;; every refusal ORDER is byte-identical, the typed request/result JSON keeps
;; the same shape, and the same laws hold:
;;
;;   * `prepare` decides dispatchability ONLY through the unchanged validation
;;     paths. A genuinely missing implementation trio (or a staged
;;     pending-review binding trio) is dispatchable and gets a deliberately
;;     gate-red template plus a typed request; an already-valid trio passes
;;     through as `reviewed`; a real refusal (source drift, stale generation,
;;     malformed artifact) stays a typed `delivery-pending` stop and is NEVER
;;     turned into a dispatch.
;;   * the reviewer must be a genuine NON-AUTHOR independent session: the
;;     reviewer identity is validated structurally here (sentinels and
;;     compound placeholders refuse) and the request carries
;;     `fresh-session-required`/`non-author-review-required` for the runner.
;;   * the reviewer's typed output is validated structurally; reviewer PROSE
;;     is never authority — verdicts, heads and the content digest are stamped
;;     by this controller, and the digest binds to the evidence computed from
;;     the fetched tip. Malformed output/sentinels/session failures are typed
;;     review-infrastructure failures: never a rejection, never an approval.
;;   * only the unchanged strict gate (`gsd-wave-gate.rkt`) can publish a
;;     genuinely APPROVED review, at the exact pinned head, through an
;;     evidence-only commit + pinned non-force push + exact readback.
;;   * an explicit head-bound REQUEST_CHANGES persists its findings through a
;;     trusted artifact commit and surfaces as `review-rejected` with the exact
;;     receipt head/branch/attempt/fence; the APPROVED gate itself is never
;;     weakened to carry a rejection.
;;   * a published rejection REPLAYS on the next prepare only when the
;;     committed artifact is still a receipt-bound evidence TAIL (ancestry and
;;     no reverted source commit), with the exact generation/attempt/fence and
;;     a matching owned-proof authority; otherwise the drift/ancestry rule
;;     refuses.
;;   * owned-proof mode copies the pinned authority onto the request and
;;     RE-AUTHENTICATES it (receipt, generation, evidence registration, proof
;;     SHA, audit binding) before the commit and again after the fetch before
;;     the push. `manual-review-compatibility` is an explicit legacy fixture
;;     mode, never a production fallback.
;;   * dispatch-request guards (missing carrier/stage/plan/wave/generation/
;;     head/branch/staging identity) refuse BEFORE any mutation.
;;
;; Reused seams (S1–S11): the shared bounded git/gh runner and ONE process
;; budget (`gsd-delivery-git.rkt`), the single-session gh read layer and its
;; memo (`gsd-delivery-api.rkt`), the read ladder's pure helpers
;; (`gsd-delivery-pr.rkt`), the unchanged strict gate
;; (`gsd-delivery-status.rkt`), the staged binding trio reader
;; (`gsd-delivery-binding.rkt`) and the trusted datum/journal CLI
;; (`gsd-binding-data.rkt`, always a bounded subprocess). ONE session/budget
;; serves one action; every subprocess is bounded; every write is attempted
;; once.
;;
;; Documented deviations (all fail closed, never open):
;;   * the Python action shells the whole controller as a subprocess; this port
;;     runs in-process, so the only process boundaries left are the trusted
;;     datum helper and the strict gate. Infrastructure faults (budget/timeout/
;;     exec boundary) keep their typed cause and are RE-RAISED by
;;     `auto-review-seam-run!`, so the dispatcher still maps them to
;;     review-infra rather than a controller refusal.
;;   * JSON payloads use this port's sorted-key writer (`json.dumps` semantics);
;;     they are semantically identical but not byte-identical wherever Python's
;;     spacing/escaping differs.
;;   * a reviewer-output file that is not JSON refuses with the typed
;;     "the reviewer output is not valid JSON: <reader message>" instead of
;;     Python's `json.JSONDecodeError` text.
;;   * `Path(x).resolve()` is spelled `simplify-path` + `path->complete-path`
;;     (no symlink resolution); a symlinked staging component is still refused
;;     by the deterministic-staging check.

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/set
         file/sha1
         openssl
         "gsd-delivery-git.rkt"
         "gsd-delivery-api.rkt"
         "gsd-delivery-pr.rkt"
         "gsd-delivery-merge.rkt"
         "gsd-delivery-status.rkt"
         "gsd-delivery-binding.rkt"
         (only-in "../extensions/gsd/delivery-handoff.rkt" redact-delivery-text))

(provide auto-review!
         auto-review-argv!
         auto-review-run!
         auto-review-seam-run!
         auto-review-main
         validate-reviewer-output
         audit-binding
         owned-audit-mode
         auto-review-stages
         implementation-review-staging
         ;; The owned-proof authority seam and the committed-rejection recovery.
         ;; Exported for the bounded native regression suites
         ;; (tests/test-gsd-evidence-rejection-binding.rkt and
         ;; tests/test-gsd-supplemental-evidence-audit.rkt), which inject a fake
         ;; git/run seam to pin every guard without an owned gate execution.
         owned-authority
         bind-owned-request
         reauthenticate-owned-request
         recover-rejected-artifact
         ;; Pure prompt/gate-fact builders and the candidate publication seam.
         ;; Exported so the owned-audit integration suite can pin the exact
         ;; audit prompt and the controller-shaped strict-gate trio, and can
         ;; prove the pre-push reauthentication guard, without re-deriving any
         ;; of them (Python `_review_prompt`, `_owned_approval_gates`,
         ;; `_publish_candidate`).
         review-prompt
         owned-approval-gates
         publish-candidate!
         ;; The typed reviewer-infrastructure stop (Python `_ReviewInfra`), so a
         ;; regression can assert it instead of a broad `exn:fail?`.
         exn:fail:review-infra?)

(define-runtime-path this-source "gsd-delivery-auto-review.rkt")
(define scripts-dir (path-only this-source))
(define binding-data-script (build-path scripts-dir "gsd-binding-data.rkt"))

;; Python's helper subprocesses all pass `timeout=60`; the bounded runner clamps
;; it to the remaining total process budget.
(define helper-command-timeout 60.0)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

(define owned-audit-mode "owned-proof-audit-v1")
(define auto-review-stages '("implementation-review" "binding-review"))
(define min-review-narrative 64)
(define min-red-first-failure 32)
(define max-review-findings 50)
(define max-finding-chars 2000)
(define campaign-id-rx #px"^[0-9a-f]{64}$")
(define iso-timestamp-rx #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
(define sha256-hex-rx #px"^[0-9a-f]{64}$")

;; Register F12: a sentinel reviewer identity is never a genuine session.
(define sentinel-text-rx
  #px"^(?i:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx|n/a|na)[:.;!,?]*$")
(define sentinel-compound-rx
  #px"(?i:(?:^|[^a-z])(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx)(?:[^a-z]|$))")

(define audit-keys
  '(plan-id wave generation head tree branch attempt-id attempt-fence content-digest proof-sha256))

;; ============================================================
;; Typed reviewer-infrastructure failure
;; ============================================================

;; The in-process mirror of Python's `_ReviewInfra`: `auto-review!` turns it
;; into {"status":"review-infra","reason":...}, and it can never masquerade as
;; a rejection or an approval.
(struct exn:fail:review-infra exn:fail () #:transparent)

(define (review-infra! message)
  (raise (exn:fail:review-infra message (current-continuation-marks))))

;; ============================================================
;; Small helpers (same semantics as the S3–S7 modules)
;; ============================================================

;; Canonical JSON comparison. Hash datums read from JSON are `eq?`-based
;; `hasheq` while datums this module builds are `equal?`-based `hash`, and
;; `equal?` distinguishes those two hash KINDS even when every key/value is
;; identical. Comparing the canonical JSON rendering keeps the comparison about
;; DATA (exactly what Python's `dict == dict` means).
(define (same-json? left right)
  (equal? (sorted-json-string left) (sorted-json-string right)))

;; Python `require(ok, reason)`.
(define (ensure! ok message)
  (unless ok
    (refuse! message)))

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

;; Python dig: any non-hash along the path yields None (#f).
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

;; Python `x is None` for a JSON null Racket read as the symbol 'null.
(define (absent? value)
  (or (not value) (eq? value 'null)))

(define (as-path value)
  (cond
    [(path? value) value]
    [else (string->path value)]))

(define (absolute-string value)
  (path->string (simplify-path (path->complete-path (as-path value)))))

(define (exists-any? p)
  (or (file-exists? p) (directory-exists? p) (link-exists? p)))

(define (regular-file? p)
  (and (file-exists? p) (not (link-exists? p))))

(define (short-sha value)
  (if (string? value)
      (substring value 0 (min 12 (string-length value)))
      (format "~a" value)))

(define (sha256-hex bytes)
  (bytes->hex-string (sha256-bytes bytes)))

(define (zeroish? value)
  (or (not value) (equal? value 0)))

;; Right-biased merge of JSON hash datums (Python `dict(a, **b)`).
(define (hash-merge base . extras)
  (for/fold ([acc base]) ([extra (in-list extras)])
    (for/fold ([inner acc]) ([(key value) (in-hash extra)])
      (hash-set inner key value))))

(define (zero-pad value width)
  (define text (number->string value))
  (string-append (make-string (max 0 (- width (string-length text))) #\0) text))

(define (iso-now)
  (define d (seconds->date (current-seconds) #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (zero-pad (date-year d) 4)
          (zero-pad (date-month d) 2)
          (zero-pad (date-day d) 2)
          (zero-pad (date-hour d) 2)
          (zero-pad (date-minute d) 2)
          (zero-pad (date-second d) 2)))

(define (sentinel-text? value)
  (and (string? value) (regexp-match? sentinel-text-rx (string-trim value))))

(define (sentinel-compound? value)
  (and (string? value) (regexp-match? sentinel-compound-rx (string-trim value))))

(define (posix-parts value)
  (string-split value "/"))

(define (excluded-path? relative)
  (and (string? relative)
       (not (abs-path? relative))
       (not (member ".." (posix-parts relative)))
       (for/or ([prefix (in-list excluded-prefixes)])
         (string-prefix? relative prefix))))

(define (abs-path? relative)
  (string-prefix? relative "/"))

;; Python's `json.dumps(value, sort_keys=True)`, including nested datums, so
;; the request payload and the pinned audit binding read exactly as the
;; controller wrote them.
(define (sorted-json-string value)
  (define out (open-output-string))
  (define (emit v)
    (cond
      [(hash? v)
       (display "{" out)
       (define pairs
         (sort (for/list ([(key item) (in-hash v)])
                 (cons (if (symbol? key) (symbol->string key) (format "~a" key)) item))
               string<? #:key car))
       (for ([pair (in-list pairs)] [index (in-naturals)])
         (when (> index 0)
           (display ", " out))
         (write-json (car pair) out)
         (display ": " out)
         (emit (cdr pair)))
       (display "}" out)]
      [(list? v)
       (display "[" out)
       (for ([item (in-list v)] [index (in-naturals)])
         (when (> index 0)
           (display ", " out))
         (emit item))
       (display "]" out)]
      [(eq? v 'null) (display "null" out)]
      [else (write-json v out)]))
  (emit value)
  (get-output-string out))

;; ============================================================
;; The action context (ONE session / ONE budget per action)
;; ============================================================

;; `make-delivery-pr-context` owns the single bounded runner: every gh read,
;; every repo git command and every helper subprocess below draws down the same
;; budget. A second context inside one action would grant a fresh budget, so
;; exactly ONE is created per call and passed everywhere.

(define (origin-main git)
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  main)

;; A git command in an arbitrary directory through the SAME bounded runner (the
;; candidate worktree is a second checkout, never a second budget).
(define (git-at run dir args)
  (run "git" (append (list "-C" (absolute-string dir)) args) command-timeout-budget))

;; Python write_rktd: serialize one JSON datum through the trusted binding-data
;; module (never string-built Racket syntax here).
(define (write-rktd! run datum path)
  (define payload (make-temporary-file "q-datum-~a.json"))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (path-only (as-path path)))
     (call-with-output-file payload
                            #:exists 'truncate
                            (lambda (out) (write-string (sorted-json-string datum) out)))
     (run "racket"
          (list (path->string binding-data-script)
                "write-datum"
                (path->string payload)
                (path->string (as-path path)))
          helper-command-timeout)
     (as-path path))
   (lambda () (when (file-exists? payload) (delete-file payload)))))

;; One committed `.rktd` blob through the trusted reader.
(define (read-datum-bytes run bytes)
  (define payload (if (bytes? bytes) bytes (string->bytes/utf-8 bytes)))
  (define root (make-temporary-file "q-auto-review-datum-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define path (build-path root "datum.rktd"))
     (call-with-output-file path #:exists 'truncate (lambda (out) (write-bytes payload out)))
     (read-binding-datum run path))
   (lambda () (delete-directory/files root #:must-exist? #f))))

;; ============================================================
;; Campaign journal receipt / tracker facts
;; ============================================================

(define (journal-root repo campaign-root)
  (campaign-journal-root repo campaign-root))

(define (receipt-for-review ctx repo plan wave campaign-root expected-head expected-branch)
  (define journal
    (campaign-journal (delivery-pr-context-run ctx)
                      (journal-root repo campaign-root)
                      plan
                      wave))
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (ensure! (and (hash? receipt)
                (equal? (hash-ref receipt 'head #f) expected-head)
                (equal? (hash-ref receipt 'branch #f) expected-branch)
                (nonblank-string? (hash-ref receipt 'attempt-id #f))
                (exact-integer? (hash-ref receipt 'attempt-fence #f)))
           "the dispatch identity does not match the durable receipt; rerun prepare")
  receipt)

;; The Python `review_tracker_facts`: fully numeric frozen snapshot metadata is
;; used as-is; legacy facts resolve ONLY a missing milestone from the exact
;; verified frozen issue, authenticated as the same repository.
(define (review-tracker-facts ctx git campaign-root plan wave)
  (define run (delivery-pr-context-run ctx))
  (define facts (snapshot-facts run campaign-root plan wave))
  (define issue (hash-ref facts 'issue #f))
  (ensure! (and (exact-integer? issue) (> issue 0))
           "the frozen wave facts carry no positive issue for the review trio")
  (cond
    [(hash-has-key? facts 'milestone)
     (define milestone (hash-ref facts 'milestone))
     (ensure! (and (exact-integer? milestone) (> milestone 0))
              "the frozen wave facts carry no positive milestone for the review trio")
     facts]
    [else
     (define title (hash-ref facts 'frozen-plan-title #f))
     (ensure! (and (string? title) (not (string=? title "")) (equal? title (string-trim title)))
              "missing unique verified frozen PLAN title for milestone resolution")
     (define slug (repository git))
     (define data ((delivery-pr-context-api ctx) slug (format "issues/~a" issue)))
     (ensure! (and (hash? data)
                   (not (hash-has-key? data 'pull_request))
                   (exact-integer? (hash-ref data 'number #f))
                   (equal? (hash-ref data 'number) issue)
                   (equal? (hash-ref data 'html_url #f)
                           (format "https://github.com/~a/issues/~a" slug issue)))
              "authenticated tracker response is not the exact frozen same-repo issue")
     (define milestone (hash-ref data 'milestone #f))
     (ensure! (and (hash? milestone)
                   (exact-integer? (hash-ref milestone 'number #f))
                   (> (hash-ref milestone 'number) 0))
              "the authenticated frozen issue has no positive milestone")
     (define number (hash-ref milestone 'number))
     (ensure! (and (equal? (hash-ref milestone 'url #f)
                           (format "https://api.github.com/repos/~a/milestones/~a" slug number))
                   (equal? (hash-ref milestone 'title #f) title))
              "authenticated milestone repository/number/title differs from the verified frozen PLAN")
     (hash-set facts 'milestone number)]))

;; ============================================================
;; Reviewer output validation (the trusted boundary)
;; ============================================================

;; Reviewer-output validation raises TYPED REVIEW-INFRA stops (never a delivery
;; refusal): malformed output is infrastructure, never a repository-state stop.
(define (infra-ensure! ok message)
  (unless ok
    (review-infra! message)))

(define (genuine-reviewer? value)
  (and (nonblank-string? value)
       (not (sentinel-text? value))
       (not (sentinel-compound? value))))

(define (reviewer-narrative! value label minimum)
  (infra-ensure! (and (nonblank-string? value) (not (sentinel-text? value)))
                 (format "the reviewer output ~a is missing or a placeholder" label))
  (infra-ensure! (>= (string-length (string-trim value)) minimum)
                 (format "the reviewer output ~a carries fewer than ~a characters of content"
                         label
                         minimum))
  (string-trim value))

;; Python `_audit_binding(request)`: the exact pinned audit identity a reviewer
;; acknowledgement must reproduce.
(define (audit-binding request)
  (define binding
    (for/hash ([key (in-list audit-keys)])
      (values key (hash-ref request key #f))))
  (define authority (hash-ref request 'owned-authority #f))
  (define reference (dig authority 'reference))
  (define promotion (hash-ref request 'supplemental-promotion-sha256 #f))
  (define expected (and (hash? authority) (hash-ref authority 'supplemental-promotion-sha256 #f)))
  (define supplemental? (and (hash? reference) (hash-has-key? reference 'supplemental-contract)))
  (define with-promotion
    (if (or supplemental? (not (absent? promotion)) (not (absent? expected)))
        (begin
          (unless (and (string? promotion)
                       (regexp-match? sha256-hex-rx promotion)
                       (equal? promotion expected))
            (review-infra! "missing or mismatched supplemental promotion binding"))
          (hash-set binding 'supplemental-promotion-sha256 promotion))
        binding))
  (define revision (hash-ref request 'evidence-revision 0))
  (cond
    [(zeroish? revision) with-promotion]
    [else
     (hash-set (hash-set with-promotion 'evidence-revision revision)
               'evidence-registration-sha256
               (hash-ref request 'evidence-registration-sha256 #f))]))

(define (exact-audit-binding? actual expected)
  (and (hash? actual) (same-json? actual expected)))

(define (validated-findings! value)
  (infra-ensure! (and (list? value)
                      (pair? value)
                      (<= (length value) max-review-findings)
                      (for/and ([item (in-list value)])
                        (and (string? item)
                             (not (string=? (string-trim item) ""))
                             (<= (string-length item) max-finding-chars))))
                 "the REQUEST_CHANGES findings are missing or malformed")
  (for/list ([item (in-list value)]) item))

;; The owned-proof audit acknowledgement: the reviewer must reproduce its audit
;; binding, must not author gate facts, and must acknowledge exactly the LINKED
;; behavioral RED (or none at all).
(define (owned-audit-result! output kind request result)
  (define ack (hash-ref output 'audit-ack #f))
  (infra-ensure! (and (hash? ack)
                      (equal? (hash-ref ack 'mode #f) owned-audit-mode)
                      (equal? (hash-ref ack 'verification-origin #f) "audited-owned")
                      (equal? (hash-ref ack 'source-read #f) #t)
                      (equal? (hash-ref ack 'raw-output-read #f) #t)
                      (exact-audit-binding? (hash-ref ack 'binding #f) (audit-binding request)))
                 "missing or stale owned-proof audit acknowledgement")
  (when (hash-has-key? output 'validation)
    (review-infra! "model-authored gate facts are forbidden in owned-proof audit mode"))
  (define authority (and (hash? request) (hash-ref request 'owned-authority #f)))
  (define red (and (hash? authority) (hash-ref authority 'red #f)))
  (cond
    [(hash? red)
     (define red-sha (dig red 'reference 'sha256))
     (infra-ensure! (and (hash? (hash-ref ack 'red-ack #f))
                         (equal? (dig ack 'red-ack 'proof-sha256) red-sha)
                         (equal? (dig ack 'red-ack 'raw-output-read) #t))
                   "missing or stale owned behavioral RED acknowledgement")]
    [(hash-has-key? ack 'red-ack)
     (review-infra! "a behavioral RED was acknowledged without a linked owned red proof")])
  (hash-set result 'audit-ack ack))

;; The approved-review gate facts the strict gate re-validates.
(define (approved-gates! output result)
  (define validation (hash-ref output 'validation #f))
  (unless (hash? validation)
    (review-infra! "the approved review carries no validation gate facts"))
  (define red (hash-ref validation 'red-first #f))
  (unless (hash? red)
    (review-infra! "the red-first gate evidence is incomplete"))
  (define command (hash-ref red 'command #f))
  (unless (and (nonblank-string? command) (not (sentinel-text? command)))
    (review-infra! "the red-first command fact is missing or a placeholder"))
  (define failure (hash-ref red 'failure #f))
  (unless (and (nonblank-string? failure) (not (sentinel-text? failure)))
    (review-infra! "the red-first observed failure is missing or a placeholder"))
  (unless (>= (string-length (string-trim failure)) min-red-first-failure)
    (review-infra! (format "the red-first observed failure carries fewer than ~a characters of content"
                           min-red-first-failure)))
  (define gates
    (hash 'red-first
          (hash 'command (string-trim command) 'failure (string-trim failure))))
  (for ([gate (in-list '("focused-tests" "format-compile" "lint" "fast"))])
    (define facts (hash-ref validation (string->symbol gate) #f))
    (unless (and (hash? facts)
                 (equal? (hash-ref facts 'result #f) "passed")
                 (nonblank-string? (hash-ref facts 'command #f)))
      (review-infra! (format "the ~a gate facts are missing or not passed" gate)))
    (set! gates
          (hash-set gates
                    (string->symbol gate)
                    (hash 'result "passed" 'command (string-trim (hash-ref facts 'command))))))
  (define remaining (hash-ref validation 'remaining-items '()))
  (unless (list? remaining)
    (review-infra! "remaining-items must be a list"))
  (set! gates (hash-set gates 'remaining-items remaining))
  (unless (equal? (hash-ref validation 'planning-sync #f) "current")
    (review-infra! "the reviewer planning-sync fact is not current"))
  (hash-set result 'validation gates))

;; Python `validate_reviewer_output`: trusts the ROLE, never prose.
(define (validate-reviewer-output output request)
  (define (infra reason) (review-infra! reason))
  (unless (hash? output)
    (infra "the reviewer output is not an object"))
  (define kind (hash-ref output 'kind #f))
  (unless (member kind '("approved" "request-changes" "review-failed"))
    (infra "the reviewer output kind is unknown"))
  (when (equal? kind "review-failed")
    (define reason (hash-ref output 'reason #f))
    (unless (nonblank-string? reason)
      (infra "the reviewer session failure carries no reason"))
    (define trimmed (string-trim reason))
    (review-infra! (format "the reviewer session failed: ~a"
                           (substring trimmed 0 (min 300 (string-length trimmed))))))
  (unless (genuine-reviewer? (hash-ref output 'reviewer #f))
    (infra "the reviewer output carries no genuine reviewer identity"))
  (define base
    (hash 'kind
          kind
          'reviewer
          (string-trim (hash-ref output 'reviewer))
          'scope
          (reviewer-narrative! (hash-ref output 'scope #f) "review scope" min-review-narrative)
          'report
          (reviewer-narrative! (hash-ref output 'report #f) "review report" min-review-narrative)))
  (define owned?
    (and request (equal? (hash-ref request 'review-mode #f) owned-audit-mode)))
  (define with-ack (if owned? (owned-audit-result! output kind request base) base))
  (cond
    [(and owned? (equal? kind "approved")) with-ack]
    [(equal? kind "request-changes")
     (hash-set with-ack 'findings (validated-findings! (hash-ref output 'findings #f)))]
    [else (approved-gates! output with-ack)]))

(define (load-reviewer-output path request)
  (define p (as-path path))
  (unless (regular-file? p)
    (review-infra! (format "the reviewer output is absent at ~a" p)))
  (define datum
    (with-handlers ([exn:fail?
                     (lambda (error)
                       (review-infra! (format "the reviewer output is not valid JSON: ~a"
                                              (exn-message error))))])
      (call-with-input-file p read-json)))
  (validate-reviewer-output datum request))

;; ============================================================
;; Owned-proof authority
;; ============================================================

;; The authenticated owned-proof authority for this plan/wave, read through the
;; trusted binding-data CLI. Any failure is the typed "missing or invalid
;; authenticated owned proof" infrastructure stop (Python `_owned_authority`).
(define (owned-authority run campaign-root plan wave staging)
  (define refusal "missing or invalid authenticated owned proof")
  (define text
    (with-handlers ([exn:fail?
                     (lambda (error)
                       (review-infra! (format "~a: ~a" refusal (exn-message error))))])
      (run "racket"
           (list (path->string binding-data-script)
                 "owned-review"
                 (absolute-string campaign-root)
                 plan
                 (number->string wave)
                 (absolute-string staging))
           helper-command-timeout)))
  (with-handlers ([exn:fail?
                   (lambda (error)
                     (review-infra! (format "~a: ~a" refusal (exn-message error))))])
    (string->jsexpr text)))

;; The pinned identity an owned authority must reproduce for this request.
(define (authority-receipt-mismatch? authority request)
  (or (not (equal? (hash-ref authority 'generation #f) (hash-ref request 'generation #f)))
      (for/or ([key (in-list '(head tree branch attempt-id attempt-fence))])
        (not (equal? (hash-ref request key #f) (dig authority 'receipt key))))))

;; Python `_bind_owned_request`: copy the pinned authority onto the request and
;; pin the audit binding into the prompt. Manual compatibility is explicit.
(define (bind-owned-request request run campaign-root plan wave staging manual?)
  (cond
    [manual? (hash-set request 'review-mode "manual-compatibility")]
    [else
     (define authority (owned-authority run campaign-root plan wave staging))
     (when (authority-receipt-mismatch? authority request)
       (review-infra! "stale owned-proof receipt/generation"))
     (define revision (hash-ref authority 'evidence-revision 0))
     (define bound
       (hash-set*
        (hash-set
         (hash-set
          (hash-set request 'review-mode owned-audit-mode)
          'proof-sha256
          (dig authority 'reference 'sha256))
         'evidence-revision
         revision)
        'owned-authority
        authority))
     (define bound*
       (if (zeroish? revision)
           (hash-remove bound 'evidence-registration-sha256)
           (hash-set bound
                     'evidence-registration-sha256
                     (hash-ref authority 'evidence-registration-sha256 #f))))
     (define bound**
       (if (hash-has-key? authority 'supplemental-promotion-sha256)
           (hash-set bound*
                     'supplemental-promotion-sha256
                     (hash-ref authority 'supplemental-promotion-sha256))
           (hash-remove bound* 'supplemental-promotion-sha256)))
     (define binding (audit-binding bound**))
     (define bound*** (hash-set bound** 'owned-authority (hash-set authority 'audit-binding binding)))
     (define red (hash-ref authority 'red #f))
     (hash-set bound***
               'prompt
               (string-append
                (hash-ref bound*** 'prompt)
                "\nPinned audit binding: "
                (sorted-json-string binding)
                "\nOwned manifest/raw log bundle: "
                (path->string (build-path (as-path staging) (hash-ref authority 'bundle)))
                "\nRead the manifest AND actual raw outputs and source before acknowledging."
                (if (hash? red)
                    (format (string-append "\nLinked behavioral RED proof (separate authentic negative "
                                           "execution, same bundle directory): read its manifest and "
                                           "actual raw failure output, then acknowledge it with red-ack "
                                           "{\"proof-sha256\": \"~a\", \"raw-output-read\": true}. ")
                            (dig red 'reference 'sha256))
                    (string-append "\nNo behavioral RED proof is linked to this green execution: "
                                   "acknowledge none and report the gap honestly."))))]))

;; Python `_reauthenticate_owned_request`: the authority is re-read and every
;; pinned fact must still hold before publication (and again after the fetch).
(define (reauthenticate-owned-request request run campaign-root plan wave staging manual?)
  (cond
    [manual?
     (unless (equal? (hash-ref request 'review-mode #f) "manual-compatibility")
       (review-infra! "manual compatibility cannot consume a production audit request"))
     #f]
    [else
     (unless (equal? (hash-ref request 'review-mode #f) owned-audit-mode)
       (review-infra! "production review requires owned-proof audit mode"))
     (define fresh (owned-authority run campaign-root plan wave staging))
     (when (authority-receipt-mismatch? fresh request)
       (review-infra! "stale owned receipt/generation before publication"))
     (when (or (not (equal? (hash-ref request 'evidence-revision 0)
                            (hash-ref fresh 'evidence-revision 0)))
               (not (equal? (hash-ref request 'evidence-registration-sha256 #f)
                            (hash-ref fresh 'evidence-registration-sha256 #f)))
               (not (equal? (hash-ref request 'supplemental-promotion-sha256 #f)
                            (hash-ref fresh 'supplemental-promotion-sha256 #f))))
       (review-infra! "evidence registration drifted since prepare"))
     (define bound (hash-set fresh 'audit-binding (audit-binding request)))
     (define previous (or (hash-ref request 'owned-authority #f) (hash)))
     (unless (same-json? bound previous)
       (define changed
         (sort (for/list ([key (in-list (set-union (hash-keys bound) (hash-keys previous)))]
                          #:unless (same-json? (hash-ref bound key #f)
                                               (hash-ref previous key #f)))
                 (format "~a" key))
               string<?))
       (define detail
         (if (member "publication-files" changed)
             (format "; publication-files prepared=~a fresh=~a"
                     (sorted-json-string (hash-ref previous 'publication-files '()))
                     (sorted-json-string (hash-ref bound 'publication-files '())))
             ""))
       (review-infra! (string-append "owned proof or receipt drifted since prepare: "
                                     (string-join changed ", ")
                                     detail)))
     (unless (equal? (hash-ref request 'proof-sha256 #f) (dig fresh 'reference 'sha256))
       (review-infra! "owned proof SHA drifted since prepare"))
     bound]))

;; The gate facts an approval publishes: manual compatibility keeps the
;; reviewer's (already validated) facts; owned audit fills EVERY fact from the
;; authenticated owner record — there is no model-prose fallback.
(define (owned-approval-gates model-gates authority)
  (cond
    [(not authority) model-gates]
    [else
     (define red (hash-ref authority 'red #f))
     (unless (hash? red)
       (review-infra! (string-append "missing-meaningful-red-proof: authenticated behavioral RED "
                                     "unavailable; owned green and independent approval are not "
                                     "delivery approval")))
     (define green (hash-ref authority 'green-gates #f))
     (define red-facts
       (if (equal? (hash-ref red 'relation #f) "baseline-with-test-overlay-v1")
           (hash 'command
                 (hash-ref red 'command #f)
                 'failure
                 (hash-ref red 'failure-summary #f)
                 'result
                 "passed"
                 'verification-origin
                 "audited-owned"
                 'owner-job-id
                 (hash-ref red 'owner-job-id #f)
                 'origin
                 (hash-ref red 'origin #f)
                 'relation
                 (hash-ref red 'relation))
           (hash 'command
                 (hash-ref red 'command #f)
                 'failure
                 (hash-ref red 'failure-summary #f)
                 'result
                 "passed"
                 'verification-origin
                 "audited-owned"
                 'owner-job-id
                 (hash-ref red 'owner-job-id #f))))
     (hash 'red-first
           red-facts
           'focused-tests
           (hash-ref green 'focused-tests)
           'format-compile
           (hash-ref green 'format-compile)
           'lint
           (hash-ref green 'lint)
           'fast
           (hash-ref green 'fast)
           'remaining-items
           '())]))

;; ============================================================
;; Deterministic review staging (campaign-local, generation-scoped)
;; ============================================================

(define (implementation-review-staging campaign-root plan wave generation staging-root)
  (define root (absolute-string campaign-root))
  (define staging (absolute-string staging-root))
  (define expected
    (path->string
     (build-path root ".planning" "campaigns" plan (format "review-w~a-g~a" wave generation))))
  (ensure! (equal? staging expected)
           (format "the review dispatch staging must be the deterministic campaign directory ~a"
                   expected))
  (for/fold ([current (as-path root)]) ([part (in-list (list ".planning" "campaigns" plan))])
    (define next (build-path current part))
    (when (link-exists? next)
      (refuse! "the review staging path contains a symlink"))
    next)
  (as-path staging))

;; Write one gate-red template artifact idempotently: an existing file must be
;; byte-identical (a different dispatch already staged here is never silently
;; overwritten; old generations are separate directories).
(define (stage-template! run staging relative datum)
  (define target (build-path (as-path staging) relative))
  (define temp (make-temporary-file "q-template-~a" 'directory))
  (define payload
    (dynamic-wind
     void
     (lambda ()
       (define candidate (build-path temp "template.rktd"))
       (write-rktd! run datum candidate)
       (file->bytes candidate))
     (lambda () (delete-directory/files temp #:must-exist? #f))))
  (cond
    [(exists-any? target)
     (ensure! (and (regular-file? target) (equal? (file->bytes target) payload))
              (string-append "the review staging already holds a different dispatch for "
                             relative))]
    [else (write-rktd! run datum target)]))

;; The controller-owned dispatch request; rewritten on every prepare so it
;; always reflects the CURRENT pinned identity and controller context.
(define (stage-request! staging request)
  (ensure! (directory-exists? (as-path staging)) "the review staging directory is missing")
  (define payload (build-path (as-path staging) "request.json"))
  (call-with-output-file payload
                         #:exists 'truncate
                         (lambda (out) (write-string (sorted-json-string request) out)))
  payload)

;; Deliberately gate-red trio: honest PENDING placeholders, no verdict, no
;; acceptance, no fabricated digest (the finalize phase stamps those).
(define (implementation-templates plan wave facts names expected-head expected-branch paths)
  (define milestone (hash-ref facts 'milestone))
  (define issue (hash-ref facts 'issue))
  (define artifact
    (hash 'schema-version
          2
          'plan-id
          plan
          'wave
          (format "W~a" wave)
          'milestone
          milestone
          'issue
          issue
          'status
          "pending-review"
          'implementation-sha
          expected-head
          'content-digest
          "PENDING"
          'branch
          expected-branch
          'required-checks
          (for/list ([name (in-list names)]) name)
          'required-pr-checks
          (for/list ([name (in-list names)]) name)
          'review-artifact
          (hash-ref paths 'review)
          'validation-artifact
          (hash-ref paths 'validation)
          'review-stage
          "fresh-independent-review-required"))
  (define review
    (hash 'reviewer
          "PENDING"
          'verdict
          "PENDING"
          'timestamp
          "PENDING"
          'reviewed-sha
          expected-head
          'content-digest
          "PENDING"
          'scope
          "PENDING"
          'report
          "PENDING"))
  (define validation
    (hash 'status
          "pending"
          'planning-sync
          "pending"
          'milestone
          milestone
          'wave
          (format "W~a" wave)
          'issue
          issue
          'branch
          expected-branch
          'implementation-sha
          expected-head
          'content-digest
          "PENDING"
          'red-first
          (hash 'command "PENDING" 'failure "PENDING")
          'focused-tests
          (hash 'result "PENDING" 'command "PENDING")
          'format-compile
          (hash 'result "PENDING" 'command "PENDING")
          'lint
          (hash 'result "PENDING" 'command "PENDING")
          'fast
          (hash 'result "PENDING" 'command "PENDING")
          'review-artifact
          (hash-ref paths 'review)
          'remaining-items
          (list (hash 'classification
                      "deferred-noncritical"
                      'owner
                      "PENDING"
                      'rationale
                      (string-append "PENDING: a genuine independent non-author review is "
                                     "required before finalization")))))
  (values artifact review validation))

(define (review-prompt stage plan wave head branch paths staging digest-value names)
  (define names-text (string-join names ", "))
  (cond
    [(equal? stage "implementation-review")
     (format
      (string-append
       "Fresh independent non-author review of campaign ~a wave W~a at the pinned "
       "receipt head ~a on branch ~a.\n"
       "A genuinely gate-red template trio (honest PENDING placeholders, no verdict) "
       "is staged at ~a (evidence ~a, review ~a, validation ~a).\n"
       "The controller-computed excluded-diff content digest is ~a; the required "
       "checks are: ~a.\n"
       "AUDIT the pinned implementation and authenticated owned manifest plus actual "
       "raw outputs using read/bash. Do not rerun owned full fast, focused, format, "
       "compile, lint or RED gates; never claim you executed them. When the pinned "
       "owned authority links a behavioral RED proof, read its manifest and actual "
       "raw failure output and acknowledge it with red-ack "
       "{\"proof-sha256\": <linked red sha256>, \"raw-output-read\": true}. "
       "Return an exact audit-ack and genuine verdict/findings, not gate facts. "
       "Missing authenticated behavioral RED, canary or F11 stays unproven. "
       "The review deadline is 600 seconds, including inspection; do not start long replay jobs.")
      plan
      wave
      head
      branch
      staging
      (hash-ref paths 'evidence)
      (hash-ref paths 'review)
      (hash-ref paths 'validation)
      digest-value
      names-text)]
    [else
     (format
      (string-append
       "Fresh independent non-author finalization of the staged post-merge binding trio "
       "for campaign ~a wave W~a (implementation receipt head ~a, branch ~a).\n"
       "The staged draft is pending-review (honest placeholders; no approval inferred). "
       "Staging: ~a (evidence ~a, review ~a, validation ~a). The publication commit "
       "changes only excluded evidence directories, so the finalized digest is the "
       "empty-diff digest ~a.\n"
       "AUDIT the original execution proof and implementation with read/bash. Do not rerun "
       "owned full fast, focused, format, compile, lint or RED gates; never claim you ran them. "
       "Return audit-ack and genuine findings/verdict, not gate facts. A merged implementation "
       "is NOT an executed head: an unsupported execution/merge relation must stop. "
       "The review deadline remains 600 seconds.")
      plan
      wave
      head
      branch
      staging
      (hash-ref paths 'evidence)
      (hash-ref paths 'review)
      (hash-ref paths 'validation)
      empty-sha)]))

;; ============================================================
;; prepare
;; ============================================================

(define (require-review-evidence-tail git head tip)
  (require-receipt-tip git head tip "review")
  (for ([commit (in-list (string-split (string-trim (git (list "rev-list"
                                                                  (format "~a..~a" head tip))))
                                       "\n"))]
        #:unless (string=? (string-trim commit) ""))
    (define changed
      (string-split (git (list "diff-tree"
                               "-m"
                               "--no-commit-id"
                               "--name-only"
                               "--no-renames"
                               "-r"
                               (string-trim commit)))
                    "\n"))
    (ensure! (for/and ([path (in-list changed)])
               (or (string=? (string-trim path) "") (excluded-path? (string-trim path))))
             "the review ancestry contains non-evidence paths")))

(define (prepare-implementation! ctx repo plan wave expected-head expected-branch
                                 campaign-root generation staging-root manual?)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define root (as-path campaign-root))
  (define facts (review-tracker-facts ctx git root plan wave))
  (define milestone (hash-ref facts 'milestone))
  (define issue (hash-ref facts 'issue))
  (define actual-generation (journal-generation run (journal-root repo root) plan wave))
  (ensure! (= generation actual-generation)
           "the review binding generation does not match the authoritative journal")
  (define paths (active-declared-outputs facts repo plan wave actual-generation))
  (refresh git)
  (define main (origin-main git))
  (git (list "fetch" "--no-tags" "origin" (string-append "refs/heads/" expected-branch)))
  (define tip (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
  (ensure! (full-sha? tip) "malformed fetched receipt branch tip")
  (require-receipt-tip git expected-head tip "review")
  (require-review-evidence-tail git expected-head tip)
  (define receipt (receipt-for-review ctx repo plan wave root expected-head expected-branch))
  ;; Publication can precede the journal checkpoint: recover committed,
  ;; receipt-bound rejection evidence BEFORE deciding whether to dispatch.
  (define recovery-authority
    (and (not manual?)
         (hash-ref receipt 'owned-proof #f)
         (blob-or-none git tip (hash-ref paths 'review))
         (let ([staging (implementation-review-staging root
                                                       plan
                                                       wave
                                                       actual-generation
                                                       staging-root)])
           (make-directory* staging)
           (owned-authority run root plan wave staging))))
  (define rejected
    (recover-rejected-artifact run
                               git
                               plan
                               wave
                               actual-generation
                               receipt
                               tip
                               (hash-ref paths 'review)
                               recovery-authority))
  (cond
    [rejected rejected]
    [else
     (define passthrough
       (and (blob-or-none git tip (hash-ref paths 'evidence))
            (delivery-review! repo
                              plan
                              wave
                              expected-head
                              expected-branch
                              #:campaign-root root
                              #:generation generation
                              #:context ctx)))
     (cond
       [(and passthrough (equal? (hash-ref passthrough 'status #f) "reviewed"))
        (unless manual?
          (review-infra! "legacy manual approval is not production owned-proof audit approval"))
        passthrough]
       [else
        (define names (policy-names git main))
        (define digest-value (content-digest git main tip))
        (define staging (implementation-review-staging root
                                                       plan
                                                       wave
                                                       actual-generation
                                                       staging-root))
        (make-directory* staging)
        (define-values (artifact review-datum validation)
          (implementation-templates plan
                                    wave
                                    facts
                                    names
                                    expected-head
                                    expected-branch
                                    paths))
        (stage-template! run staging (hash-ref paths 'evidence) artifact)
        (stage-template! run staging (hash-ref paths 'review) review-datum)
        (stage-template! run staging (hash-ref paths 'validation) validation)
        (define request
          (hash 'kind
                "implementation-review"
                'plan-id
                plan
                'wave
                wave
                'generation
                actual-generation
                'head
                expected-head
                'branch
                expected-branch
                'tip
                tip
                'tree
                (hash-ref receipt 'tree)
                'attempt-id
                (hash-ref receipt 'attempt-id)
                'attempt-fence
                (hash-ref receipt 'attempt-fence)
                'evidence
                (hash-ref paths 'evidence)
                'review
                (hash-ref paths 'review)
                'validation
                (hash-ref paths 'validation)
                'content-digest
                digest-value
                'required-checks
                (for/list ([name (in-list names)]) name)
                'milestone
                milestone
                'issue
                issue
                'staging
                (path->string staging)
                'reviewer-output
                (path->string (build-path staging "reviewer-output.json"))
                'fresh-session-required
                #t
                'non-author-review-required
                #t
                'prompt
                (review-prompt "implementation-review"
                               plan
                               wave
                               expected-head
                               expected-branch
                               paths
                               (path->string staging)
                               digest-value
                               names)))
        (define bound (bind-owned-request request run root plan wave staging manual?))
        (stage-request! staging bound)
        (hash 'status "awaiting-review" 'stage "implementation-review" 'request bound)])]))

(define (prepare-binding! ctx repo plan wave expected-head expected-branch
                          campaign-root generation staging-root manual?)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define root (as-path campaign-root))
  (define staging (require-binding-output staging-root root plan wave generation))
  (define staged
    (delivery-binding-review! repo
                              plan
                              wave
                              staging
                              #:campaign-root root
                              #:expected-branch expected-branch
                              #:context ctx))
  (cond
    [(equal? (hash-ref staged 'status #f) "reviewed")
     (unless manual?
       (review-infra! "existing manual binding approval is not production audit approval"))
     staged]
    [else
     (ensure! (equal? (hash-ref staged 'status #f) "pending-review")
              (format
               (string-append "the binding staging is ~a; only a pending-review draft may "
                              "dispatch a fresh reviewer (run binding-prepared first)")
               (hash-ref staged 'status #f)))
     (define receipt (receipt-for-review ctx repo plan wave root expected-head expected-branch))
     (refresh git)
     (define main (origin-main git))
     (define names (policy-names git main))
     (define artifact (path->string (file-name-from-path (as-path (hash-ref staged 'binding)))))
     (define paths
       (hash 'evidence
             (hash-ref staged 'binding)
             'review
             (format "docs/reports/gsd-wave-reviews/~a" artifact)
             'validation
             (format "docs/reports/gsd-wave-validation/~a" artifact)))
     (define request
       (hash 'kind
             "binding-review"
             'plan-id
             plan
             'wave
             wave
             'generation
             generation
             'head
             expected-head
             'branch
             expected-branch
             'merge-sha
             (hash-ref staged 'merge-sha)
             'delivery-head-sha
             (hash-ref staged 'delivery-head-sha)
             'binding-branch
             (hash-ref staged 'branch)
             'tree
             (hash-ref receipt 'tree)
             'attempt-id
             (hash-ref receipt 'attempt-id)
             'attempt-fence
             (hash-ref receipt 'attempt-fence)
             'evidence
             (hash-ref paths 'evidence)
             'review
             (hash-ref paths 'review)
             'validation
             (hash-ref paths 'validation)
             'content-digest
             empty-sha
             'required-checks
             (for/list ([name (in-list names)]) name)
             'staging
             (path->string staging)
             'reviewer-output
             (path->string (build-path staging "reviewer-output.json"))
             'fresh-session-required
             #t
             'non-author-review-required
             #t
             'prompt
             (review-prompt "binding-review"
                            plan
                            wave
                            expected-head
                            expected-branch
                            paths
                            (path->string staging)
                            empty-sha
                            names)))
     (define bound (bind-owned-request request run root plan wave staging manual?))
     (unless manual?
       (unless (equal? (hash-ref staged 'merge-sha) expected-head)
         (review-infra!
          "unsupported-postmerge-owned-proof-relation: execution head is not merged head")))
     (stage-request! staging bound)
     (hash 'status "awaiting-review" 'stage "binding-review" 'request bound)]))

(define (prepare! ctx repo plan wave stage expected-head expected-branch
                  campaign-root generation staging-root manual?)
  (if (equal? stage "binding-review")
      (prepare-binding! ctx
                        repo
                        plan
                        wave
                        expected-head
                        expected-branch
                        campaign-root
                        generation
                        staging-root
                        manual?)
      (prepare-implementation! ctx
                               repo
                               plan
                               wave
                               expected-head
                               expected-branch
                               campaign-root
                               generation
                               staging-root
                               manual?)))

;; ============================================================
;; Publication (evidence-only candidate, pinned non-force push, readback)
;; ============================================================

(define (path-in-parts? relative parts)
  (for/or ([part (in-list parts)])
    (equal? part relative)))

(define (owned-publication-files run staging authority)
  (cond
    [(not authority) (hash)]
    [else
     (define bundle (hash-ref authority 'bundle))
     (define refs (list (hash-ref authority 'reference) (hash-ref authority 'log-reference)))
     (define red (hash-ref authority 'red #f))
     (define refs*
       (append refs
               (if (and (hash? red)
                        (not (equal? (hash-ref red 'relation #f)
                                     "baseline-with-test-overlay-v1")))
                   (list (hash-ref red 'reference) (hash-ref red 'log-reference))
                   '())))
     (define base-files
       (for/fold ([acc (hash)]) ([ref (in-list refs*)])
         (define relative (string-append bundle "/" (hash-ref ref 'path)))
         (hash-set acc relative (build-path (as-path staging) relative))))
     (define files
       (for/fold ([acc base-files])
                 ([relative (in-list (hash-ref authority 'publication-files '()))])
         (define target (as-path relative))
         (ensure! (and (not (absolute-path? target))
                       (not (member ".." (posix-parts relative)))
                       (string-prefix? relative (string-append bundle "/")))
                  "unsafe portable proof path")
         (define source (build-path (as-path staging) relative))
         (ensure! (regular-file? source)
                  (string-append "missing portable owned proof: " relative))
         (when (equal? (path-get-extension source) #".json")
           (define name
             (path->string (file-name-from-path (path-replace-extension source #""))))
           (when (regexp-match? sha256-hex-rx name)
             (ensure! (equal? (sha256-hex (file->bytes source)) name)
                      (string-append "portable proof digest mismatch: " relative))))
         (when (equal? (file-name-from-path source) (string->path "objects.bundle"))
           (define metadata
             (call-with-input-file (build-path (path-only source) "bundle.json") read-json))
           (ensure! (equal? (sha256-hex (file->bytes source))
                            (dig metadata 'git-bundle-sha256))
                    "portable Git bundle digest mismatch"))
         (hash-set acc relative source)))
     ;; The committed authority payload lets push CI validate the audit trio with
     ;; the exact trusted authority the controller used. Written from the FINAL
     ;; reauthenticated authority, so the bytes match the publication exactly.
     (define payload (build-path (as-path staging) bundle "authority.json"))
     (make-directory* (path-only payload))
     (call-with-output-file payload
                            #:exists 'truncate
                            (lambda (out) (write-string (sorted-json-string authority) out)))
     (hash-set files (string-append bundle "/authority.json") payload)]))

;; Create a LOCAL evidence-only candidate commit; remote refs are never touched
;; here (Python `_commit_candidate`).
(define (commit-candidate! ctx base-tip datums message raw-files)
  (define run (delivery-pr-context-run ctx))
  (define git (delivery-pr-context-git ctx))
  (ensure! (and (hash? datums) (positive? (hash-count datums)))
           "the review publication has no artifacts")
  (define raw (or raw-files (hash)))
  (for ([relative (in-list (append (hash-keys datums) (hash-keys raw)))])
    (ensure! (excluded-path? relative)
             (string-append "the review publication may only touch excluded evidence paths: "
                            relative)))
  (define temp (make-temporary-file "q-auto-review-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define worktree (build-path temp "wt"))
     (git (list "worktree" "add" "--detach" (path->string worktree) base-tip))
     (dynamic-wind
      void
      (lambda ()
        (for ([(relative datum) (in-hash datums)])
          (define target (build-path worktree relative))
          (ensure! (not (link-exists? target)) "the candidate artifact is a symlink")
          (when (exists-any? target)
            (ensure! (file-exists? target) "the candidate artifact is not a file")
            (delete-file target))
          (write-rktd! run datum target))
        (for ([(relative source) (in-hash raw)])
          (define target (build-path worktree relative))
          (ensure! (regular-file? source) "unsafe raw proof source")
          ;; Walk every ancestor from the target's directory up to (and
          ;; including) the candidate worktree, refusing a symlinked component
          ;; exactly like the Python oracle's `for parent in target.parents`.
          ;; The walk is over DIRECTORY paths: `path-only` is idempotent on a
          ;; path that already ends in a separator, so stepping with it would
          ;; never advance (the loop below is the same law, spelled so every
          ;; iteration strictly loses one component).
          (define worktree-dir (path->directory-path worktree))
          (let loop ([parent (path->directory-path (path-only target))])
            (ensure! (not (link-exists? parent)) "symlinked candidate proof directory")
            (unless (equal? parent worktree-dir)
              (define-values (next _name _dir?) (split-path parent))
              (when next (loop next))))
          (make-directory* (path-only target))
          (ensure! (not (link-exists? target)) "candidate raw proof is a symlink")
          (define data (file->bytes source))
          (cond
            [(exists-any? target)
             (ensure! (equal? (file->bytes target) data) "published addressed proof differs")]
            [else
             (call-with-output-file target
                                    #:exists 'truncate
                                    (lambda (out) (write-bytes data out)))]))
        (git-at run
                worktree
                (append (list "add" "--")
                        (sort (append (hash-keys datums) (hash-keys raw)) string<?)))
        (git-at run
                worktree
                (list "-c"
                      "user.name=gsd-delivery-controller"
                      "-c"
                      "user.email=gsd-delivery@invalid"
                      "commit"
                      "--no-gpg-sign"
                      "-m"
                      message))
        (string-trim (git-at run worktree (list "rev-parse" "HEAD"))))
      (lambda ()
        (with-handlers ([exn:fail:delivery-action? void])
          (git (list "worktree" "remove" "--force" (path->string worktree)))))))
   (lambda () (delete-directory/files temp #:must-exist? #f))))

(define (publish-candidate! ctx repo branch base-tip commit guard)
  (define git (delivery-pr-context-git ctx))
  (git (list "fetch" "--no-tags" "origin" (string-append "refs/heads/" branch)))
  (define remote (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
  (unless (equal? remote base-tip)
    (review-infra!
     (format
      (string-append "the remote branch diverged from the prepared review tip during review "
                     "(~a != ~a); the dispatch is stale and was not published")
      (short-sha remote)
      (short-sha base-tip))))
  (when guard (guard))
  (define readback
    (with-handlers ([exn:fail:delivery-action?
                     (lambda (error)
                       (review-infra!
                        (format "publishing the reviewed artifacts failed: ~a"
                                (exn-message error))))])
      (git (list "push" "--no-tags" "origin" (format "~a:refs/heads/~a" commit branch)))
      (git (list "fetch"
                 "--no-tags"
                 "origin"
                 (format "refs/heads/~a:refs/remotes/origin/~a" branch branch)))
      (string-trim (git (list "rev-parse" (string-append "refs/remotes/origin/" branch))))))
  (ensure! (equal? readback commit)
           "the reviewed publication readback does not match the published commit")
  commit)

;; ============================================================
;; Rejection recovery / verification
;; ============================================================

(define (verify-rejected-artifact ctx git run ref relative expected-head findings)
  (define blob (blob-or-none git ref relative))
  (ensure! blob "the rejected review artifact is absent at the published tip")
  (define datum (read-datum-bytes run blob))
  (ensure! (and (hash? datum)
                (equal? (hash-ref datum 'verdict #f) "REQUEST_CHANGES")
                (equal? (hash-ref datum 'reviewed-sha #f) expected-head)
                (equal? (hash-ref datum 'head #f) expected-head)
                (equal? (hash-ref datum 'findings #f) findings))
           "the committed rejection failed its structural validation"))

(define (recover-rejected-artifact run git plan wave generation receipt tip relative authority)
  (define blob (blob-or-none git tip relative))
  (cond
    [(not blob) #f]
    [else
     (define datum (read-datum-bytes run blob))
     (ensure! (hash? datum) "the committed review is malformed")
     (cond
       [(equal? (hash-ref datum 'verdict #f) "APPROVED") #f]
       [else
        (ensure! (equal? (hash-ref datum 'verdict #f) "REQUEST_CHANGES")
                 "the committed review has an invalid verdict")
        (define receipt-head (hash-ref receipt 'head))
        (ensure! (and (not (equal? tip receipt-head))
                      (not (equal? blob (blob-or-none git receipt-head relative))))
                 "the rejected review is not a published evidence tail")
        (define identity
          (hash 'stage
                "implementation-review"
                'plan-id
                plan
                'wave
                (format "W~a" wave)
                'generation
                generation
                'head
                receipt-head
                'reviewed-sha
                receipt-head
                'branch
                (hash-ref receipt 'branch)
                'attempt-id
                (hash-ref receipt 'attempt-id)
                'attempt-fence
                (hash-ref receipt 'attempt-fence)))
        (ensure! (for/and ([(key value) (in-hash identity)])
                   (equal? (hash-ref datum key #f) value))
                 "the committed rejection does not match the exact receipt/generation")
        (ensure! (and (string? (hash-ref datum 'timestamp #f))
                      (regexp-match? iso-timestamp-rx (hash-ref datum 'timestamp)))
                 "the committed rejection timestamp is malformed")
        (define request
          (and (hash-has-key? datum 'review-mode)
               (let ()
                 (ensure! (equal? (hash-ref datum 'review-mode) owned-audit-mode)
                          "unknown committed review mode")
                 (define ref (hash-ref receipt 'owned-proof #f))
                 (ensure! (and (hash? ref) (equal? (hash-ref datum 'owned-proof #f) ref))
                          "recovered rejection has stale owned execution proof")
                 (ensure! (hash? authority)
                          "recovered owned rejection requires fresh authenticated authority")
                 (ensure! (and (equal? (hash-ref authority 'generation #f) generation)
                               (exact-audit-binding? (hash-ref authority 'receipt #f) receipt)
                               (equal? (hash-ref authority 'reference #f) ref))
                          "recovered rejection has stale owned receipt/generation/proof")
                 (define base
                   (hash-merge receipt
                               (hash 'plan-id
                                     plan
                                     'wave
                                     wave
                                     'generation
                                     generation
                                     'proof-sha256
                                     (hash-ref ref 'sha256)
                                     'review-mode
                                     owned-audit-mode)))
                 (define revision (hash-ref authority 'evidence-revision 0))
                 (define base*
                   (hash-set base
                             'content-digest
                             (content-digest git "refs/remotes/origin/main" receipt-head)))
                 (define base**
                   (hash-set base* 'evidence-revision revision))
                 (define base***
                   (if (zeroish? revision)
                       (hash-remove base** 'evidence-registration-sha256)
                       (hash-set base**
                                 'evidence-registration-sha256
                                 (hash-ref authority 'evidence-registration-sha256 #f))))
                 (define base****
                   (if (hash-has-key? authority 'supplemental-promotion-sha256)
                       (hash-set base***
                                 'supplemental-promotion-sha256
                                 (hash-ref authority 'supplemental-promotion-sha256))
                       (hash-remove base*** 'supplemental-promotion-sha256)))
                 (define with-authority
                   (hash-set base**** 'owned-authority authority))
                 (define red (hash-ref authority 'red #f))
                 (ensure! (equal? (hash-ref datum 'owned-red #f)
                                  (and (hash? red) (hash-ref red 'reference #f)))
                          "recovered rejection has stale owned behavioral RED proof")
                 (ensure! (exact-audit-binding? (hash-ref datum 'audit-binding #f)
                                                (audit-binding with-authority))
                          "recovered rejection has stale audit binding")
                 with-authority)))
        (define output
          (with-handlers ([exn:fail:review-infra?
                           (lambda (error)
                             (refuse! (format "the committed rejection is malformed: ~a"
                                              (exn-message error))))])
            (validate-reviewer-output (hash-set datum 'kind "request-changes") request)))
        (hash 'status
              "review-rejected"
              'stage
              "implementation-review"
              'plan-id
              plan
              'wave
              wave
              'generation
              generation
              'head
              receipt-head
              'branch
              (hash-ref receipt 'branch)
              'attempt-id
              (hash-ref receipt 'attempt-id)
              'attempt-fence
              (hash-ref receipt 'attempt-fence)
              'postmerge
              #f
              'findings
              (hash-ref output 'findings)
              'tip
              tip)])]))

;; ============================================================
;; finalize
;; ============================================================

(define (load-dispatch-request staging plan wave generation expected-head expected-branch stage)
  (define path (build-path (as-path staging) "request.json"))
  (ensure! (regular-file? path)
           (format "no review dispatch request at ~a; run prepare first" path))
  (define request
    (with-handlers ([exn:fail?
                     (lambda (error)
                       (refuse! (format "the review dispatch request is malformed: ~a"
                                        (exn-message error))))])
      (call-with-input-file path read-json)))
  (ensure! (and (hash? request)
                (equal? (hash-ref request 'kind #f) stage)
                (equal? (hash-ref request 'plan-id #f) plan)
                (equal? (hash-ref request 'wave #f) wave)
                (equal? (hash-ref request 'generation #f) generation)
                (equal? (hash-ref request 'head #f) expected-head)
                (equal? (hash-ref request 'branch #f) expected-branch))
           "the dispatch identity does not match the durable receipt; rerun prepare")
  request)

(define (finalize-implementation! ctx repo plan wave expected-head expected-branch
                                  campaign-root generation staging-root reviewer-output
                                  manual?)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define root (as-path campaign-root))
  (define staging (implementation-review-staging root plan wave generation staging-root))
  (define request (load-dispatch-request staging
                                         plan
                                         wave
                                         generation
                                         expected-head
                                         expected-branch
                                         "implementation-review"))
  (define receipt (receipt-for-review ctx repo plan wave root expected-head expected-branch))
  (ensure! (for/and ([key (in-list '(head branch attempt-id attempt-fence))])
             (equal? (hash-ref request key #f) (hash-ref receipt key #f)))
           "the dispatch identity does not match the exact durable receipt; rerun prepare")
  (define facts (review-tracker-facts ctx git root plan wave))
  (define milestone (hash-ref facts 'milestone))
  (define issue (hash-ref facts 'issue))
  (ensure! (and (exact-integer? (hash-ref request 'milestone #f))
                (= (hash-ref request 'milestone) milestone)
                (exact-integer? (hash-ref request 'issue #f))
                (= (hash-ref request 'issue) issue))
           "the tracker binding changed after review dispatch; rerun prepare")
  (define paths (active-declared-outputs facts repo plan wave generation))
  (refresh git)
  (define main (origin-main git))
  (git (list "fetch" "--no-tags" "origin" (string-append "refs/heads/" expected-branch)))
  (define tip (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
  (ensure! (full-sha? tip) "malformed fetched receipt branch tip")
  (require-receipt-tip git expected-head tip "review")
  (when (not (equal? tip (hash-ref request 'tip #f)))
    (review-infra!
     "the receipt branch moved after the review dispatch was prepared; rerun prepare"))
  (define authority (reauthenticate-owned-request request run root plan wave staging manual?))
  (define output (load-reviewer-output reviewer-output request))
  (cond
    [(equal? (hash-ref output 'kind) "request-changes")
     (finalize-rejection! ctx
                          repo
                          plan
                          wave
                          expected-head
                          expected-branch
                          root
                          generation
                          staging
                          request
                          receipt
                          authority
                          paths
                          output
                          manual?)]
    [else
     ;; approved: the controller binds every digest and head; the reviewer's
     ;; prose never becomes authority. Only the strict gate can produce the
     ;; reviewed verdict.
     (define content (content-digest git main tip))
     (define names (policy-names git main))
     (define common
       (if authority
           (let ([base (hash 'review-mode owned-audit-mode
                             'owned-proof
                             (hash-ref authority 'reference)
                             'audit-binding
                             (hash-ref authority 'audit-binding))])
             (if (hash? (hash-ref authority 'red #f))
                 (hash-set base 'owned-red (dig authority 'red 'reference))
                 base))
           (hash)))
     (define evidence
       (hash-merge common
                    (hash 'schema-version
                          2
                          'plan-id
                          plan
                          'wave
                          (format "W~a" wave)
                          'milestone
                          milestone
                          'issue
                          issue
                          'status
                          "ready-for-merge"
                          'implementation-sha
                          expected-head
                          'content-digest
                          content
                          'branch
                          expected-branch
                          'required-checks
                          (for/list ([name (in-list names)]) name)
                          'required-pr-checks
                          (for/list ([name (in-list names)]) name)
                          'review-artifact
                          (hash-ref paths 'review)
                          'validation-artifact
                          (hash-ref paths 'validation))))
     (define evidence* evidence)
     (define review-datum
       (hash-merge common
                   (hash 'reviewer
                         (hash-ref output 'reviewer)
                         'verdict
                         "APPROVED"
                         'timestamp
                         (iso-now)
                         'reviewed-sha
                         expected-head
                         'content-digest
                         content
                         'scope
                         (hash-ref output 'scope)
                         'report
                         (hash-ref output 'report))))
     (define review-final
       (if authority
           (hash-set review-datum 'audit-ack (hash-ref output 'audit-ack))
           review-datum))
     (define gates (owned-approval-gates (hash-ref output 'validation #f) authority))
     (define validation
       (hash-merge common
                   (hash 'status
                         "current"
                         'planning-sync
                         "current"
                         'milestone
                         milestone
                         'wave
                         (format "W~a" wave)
                         'issue
                         issue
                         'branch
                         expected-branch
                         'implementation-sha
                         expected-head
                         'content-digest
                         content
                         'red-first
                         (hash-ref gates 'red-first)
                         'focused-tests
                         (hash-ref gates 'focused-tests)
                         'format-compile
                         (hash-ref gates 'format-compile)
                         'lint
                         (hash-ref gates 'lint)
                         'fast
                         (hash-ref gates 'fast)
                         'review-artifact
                         (hash-ref paths 'review)
                         'remaining-items
                         (hash-ref gates 'remaining-items))))
     (define candidate
       (commit-candidate! ctx
                          tip
                          (hash (hash-ref paths 'evidence)
                                evidence*
                                (hash-ref paths 'review)
                                review-final
                                (hash-ref paths 'validation)
                                validation)
                          (format "W~a independent review: ~a-w~a" wave plan wave)
                          (owned-publication-files run staging authority)))
     (with-handlers ([exn:fail:delivery-action?
                      (lambda (error)
                        (review-infra!
                         (format "the local candidate failed the unchanged strict gate: ~a"
                                 (exn-message error))))])
       (require-review-evidence-tail git expected-head candidate)
       (validate-trio ctx
                      candidate
                      (hash-ref paths 'evidence)
                      main
                      #:owned-authority authority)
       (void))
     (define tip-now
       (publish-candidate! ctx
                           repo
                           expected-branch
                           tip
                           candidate
                           (and authority
                                (lambda ()
                                  (reauthenticate-owned-request request
                                                                run
                                                                root
                                                                plan
                                                                wave
                                                                staging
                                                                manual?)))))
     (require-review-evidence-tail git expected-head tip-now)
     (validate-trio ctx tip-now (hash-ref paths 'evidence) main #:owned-authority authority)
     (hash 'status
           "reviewed"
           'plan-id
           plan
           'wave
           wave
           'head
           expected-head
           'reviewed-sha
           expected-head
           'review-artifact
           (hash-ref paths 'review)
           'tip
           tip-now)]))

(define (finalize-rejection! ctx repo plan wave expected-head expected-branch root generation
                             staging request receipt authority paths output manual?)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define base
    (hash 'stage
          "implementation-review"
          'plan-id
          plan
          'wave
          (format "W~a" wave)
          'verdict
          "REQUEST_CHANGES"
          'reviewer
          (hash-ref output 'reviewer)
          'timestamp
          (iso-now)
          'reviewed-sha
          expected-head
          'head
          expected-head
          'branch
          expected-branch
          'attempt-id
          (hash-ref receipt 'attempt-id)
          'attempt-fence
          (hash-ref receipt 'attempt-fence)
          'generation
          generation
          'findings
          (hash-ref output 'findings)
          'scope
          (hash-ref output 'scope)
          'report
          (hash-ref output 'report)))
  (define rejected
    (if authority
        (let ([with-common (hash-set*
                            (hash-set*
                             (hash-set* base 'review-mode owned-audit-mode)
                             'owned-proof
                             (hash-ref authority 'reference))
                            'audit-binding
                            (audit-binding request))])
          (hash-set*
           (if (hash? (hash-ref authority 'red #f))
               (hash-set with-common 'owned-red (dig authority 'red 'reference))
               with-common)
           'audit-ack
           (hash-ref output 'audit-ack)))
        base))
  (define tip (hash-ref request 'tip))
  (define candidate
    (commit-candidate! ctx
                       tip
                       (hash (hash-ref paths 'review) rejected)
                       (format "W~a review rejected: ~a-w~a" wave plan wave)
                       (owned-publication-files run staging authority)))
  (require-review-evidence-tail git expected-head candidate)
  (verify-rejected-artifact ctx
                            git
                            run
                            candidate
                            (hash-ref paths 'review)
                            expected-head
                            (hash-ref output 'findings))
  (recover-rejected-artifact run
                             git
                             plan
                             wave
                             generation
                             receipt
                             candidate
                             (hash-ref paths 'review)
                             authority)
  (reauthenticate-owned-request request run root plan wave staging manual?)
  (define tip-now
    (publish-candidate!
     ctx
     repo
     expected-branch
     tip
     candidate
     (and authority
          (lambda ()
            (reauthenticate-owned-request request run root plan wave staging #f)))))
  (recover-rejected-artifact run
                             git
                             plan
                             wave
                             generation
                             receipt
                             tip-now
                             (hash-ref paths 'review)
                             authority))

(define (finalize-binding! ctx repo plan wave expected-head expected-branch campaign-root
                           generation staging-root reviewer-output manual?)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define root (as-path campaign-root))
  (define staging (require-binding-output staging-root root plan wave generation))
  (define staged
    (delivery-binding-review! repo
                              plan
                              wave
                              staging
                              #:campaign-root root
                              #:expected-branch expected-branch
                              #:context ctx))
  (cond
    [(equal? (hash-ref staged 'status #f) "reviewed")
     (unless manual?
       (review-infra! "existing manual binding approval is not production audit approval"))
     (hash-set* staged
                'stage
                "binding-review"
                'head
                expected-head
                'branch
                expected-branch
                'postmerge
                #t)]
    [else
     (ensure! (equal? (hash-ref staged 'status #f) "pending-review")
              (format "the binding staging is ~a; only a pending-review draft can be finalized"
                      (hash-ref staged 'status #f)))
     (define request (load-dispatch-request staging
                                            plan
                                            wave
                                            generation
                                            expected-head
                                            expected-branch
                                            "binding-review"))
     (define receipt (receipt-for-review ctx repo plan wave root expected-head expected-branch))
     (define authority (reauthenticate-owned-request request run root plan wave staging manual?))
     (define output (load-reviewer-output reviewer-output request))
     (when (and authority (not (equal? (hash-ref staged 'merge-sha) expected-head)))
       (review-infra! "unsupported-postmerge-owned-proof-relation"))
     (define artifact (path->string (file-name-from-path (as-path (hash-ref staged 'binding)))))
     (define review-rel (format "docs/reports/gsd-wave-reviews/~a" artifact))
     (define evidence-path (build-path staging "docs/reports/gsd-wave-evidence" artifact))
     (define review-path (build-path staging (string->path review-rel)))
     (define validation-path (build-path staging "docs/reports/gsd-wave-validation" artifact))
     (define evidence (read-binding-datum run evidence-path))
     (cond
       [(equal? (hash-ref output 'kind) "request-changes")
        (define rejected
          (hash 'stage
                "binding-review"
                'plan-id
                plan
                'wave
                (format "W~a" wave)
                'verdict
                "REQUEST_CHANGES"
                'reviewer
                (hash-ref output 'reviewer)
                'timestamp
                (iso-now)
                'reviewed-sha
                (hash-ref staged 'merge-sha)
                'head
                expected-head
                'branch
                (hash-ref staged 'branch)
                'findings
                (hash-ref output 'findings)
                'scope
                (hash-ref output 'scope)
                'report
                (hash-ref output 'report)))
        (write-rktd! run rejected (build-path staging "review-rejected.rktd"))
        (hash 'status
              "review-rejected"
              'stage
              "binding-review"
              'plan-id
              plan
              'wave
              wave
              'head
              expected-head
              'branch
              expected-branch
              'attempt-id
              (hash-ref receipt 'attempt-id)
              'attempt-fence
              (hash-ref receipt 'attempt-fence)
              'postmerge
              #t
              'findings
              (hash-ref output 'findings))]
       [else
        (define gates (owned-approval-gates (hash-ref output 'validation #f) authority))
        (define common
          (if authority
              (let ([base (hash 'review-mode owned-audit-mode
                                'owned-proof
                                (hash-ref authority 'reference)
                                'audit-binding
                                (hash-ref authority 'audit-binding))])
                (if (hash? (hash-ref authority 'red #f))
                    (hash-set base 'owned-red (dig authority 'red 'reference))
                    base))
              (hash)))
        (define finalized-evidence
          (hash-merge evidence (hash 'status "ready-for-merge" 'content-digest empty-sha)))
        (define evidence-final
          (for/fold ([acc finalized-evidence]) ([(key value) (in-hash common)])
            (hash-set acc key value)))
        (define review-datum
          (hash-merge common
                      (hash 'reviewer
                            (hash-ref output 'reviewer)
                            'verdict
                            "APPROVED"
                            'timestamp
                            (iso-now)
                            'reviewed-sha
                            (hash-ref staged 'merge-sha)
                            'content-digest
                            empty-sha
                            'scope
                            (hash-ref output 'scope)
                            'report
                            (hash-ref output 'report))))
        (define review-final
          (if authority
              (hash-set review-datum 'audit-ack (hash-ref output 'audit-ack))
              review-datum))
        (define validation
          (hash-merge common
                      (hash 'status
                            "current"
                            'planning-sync
                            "current"
                            'milestone
                            (hash-ref evidence 'milestone)
                            'wave
                            (hash-ref evidence 'wave)
                            'issue
                            (hash-ref evidence 'issue)
                            'branch
                            (hash-ref staged 'branch)
                            'implementation-sha
                            (hash-ref staged 'merge-sha)
                            'content-digest
                            empty-sha
                            'red-first
                            (hash-ref gates 'red-first)
                            'focused-tests
                            (hash-ref gates 'focused-tests)
                            'format-compile
                            (hash-ref gates 'format-compile)
                            'lint
                            (hash-ref gates 'lint))))
        (define validation-final
          (hash-merge validation
                      (hash 'fast
                            (hash-ref gates 'fast)
                            'review-artifact
                            review-rel
                            'remaining-items
                            (hash-ref gates 'remaining-items))))
        (for ([path (in-list (list evidence-path review-path validation-path))])
          (when (file-exists? path)
            (delete-file path)))
        (write-rktd! run evidence-final evidence-path)
        (write-rktd! run review-final review-path)
        (write-rktd! run validation-final validation-path)
        (when authority
          (owned-publication-files run staging authority))
        (define result
          (delivery-binding-review! repo
                                    plan
                                    wave
                                    staging
                                    #:campaign-root root
                                    #:expected-branch expected-branch
                                    #:context ctx))
        (ensure! (equal? (hash-ref result 'status #f) "reviewed")
                 (format "the finalized staged trio failed the strict binding gate: ~a"
                         (hash-ref result 'status #f)))
        (hash-set* result
                   'stage
                   "binding-review"
                   'head
                   expected-head
                   'branch
                   expected-branch
                   'postmerge
                   #t)])]))

(define (finalize! ctx repo plan wave stage expected-head expected-branch
                   campaign-root generation staging-root reviewer-output manual?)
  (if (equal? stage "binding-review")
      (finalize-binding! ctx
                         repo
                         plan
                         wave
                         expected-head
                         expected-branch
                         campaign-root
                         generation
                         staging-root
                         reviewer-output
                         manual?)
      (finalize-implementation! ctx
                                repo
                                plan
                                wave
                                expected-head
                                expected-branch
                                campaign-root
                                generation
                                staging-root
                                reviewer-output
                                manual?)))

;; ============================================================
;; The action
;; ============================================================

(define (auto-review! repo plan wave phase stage expected-head expected-branch
                      campaign-root generation staging-root
                      #:reviewer-output [reviewer-output #f]
                      #:manual-compatibility? [manual? #f])
  (ensure! (and (string? plan) (regexp-match? campaign-id-rx plan)) "invalid campaign id")
  (ensure! (and (exact-integer? wave) (>= wave 0)) "invalid wave index")
  (ensure! (member phase '("prepare" "finalize")) "invalid auto-review phase")
  (ensure! (member stage auto-review-stages) "invalid auto-review stage")
  (ensure! (full-sha? expected-head) "auto-review requires the durable receipt head")
  (ensure! (nonblank-string? expected-branch) "auto-review requires the durable receipt branch")
  (ensure! (and campaign-root (not (absent? campaign-root)))
           "auto-review requires --campaign-root")
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (ensure! (and staging-root (not (absent? staging-root))) "auto-review requires --staging-root")
  (define ctx (make-delivery-pr-context repo))
  (ensure! (= generation
               (journal-generation (delivery-pr-context-run ctx)
                                   (journal-root repo campaign-root)
                                   plan
                                   wave))
           "the review binding generation does not match the authoritative journal")
  (with-handlers ([exn:fail:review-infra?
                   (lambda (error)
                     (hash 'status "review-infra" 'reason (exn-message error)))])
    (cond
      [(equal? phase "prepare")
       (prepare! ctx
                 repo
                 plan
                 wave
                 stage
                 expected-head
                 expected-branch
                 campaign-root
                 generation
                 staging-root
                 manual?)]
      [else
       (ensure! (and reviewer-output (not (absent? reviewer-output)))
                "finalize requires --reviewer-output")
       (finalize! ctx
                  repo
                  plan
                  wave
                  stage
                  expected-head
                  expected-branch
                  campaign-root
                  generation
                  staging-root
                  reviewer-output
                  manual?)])))

;; ============================================================
;; CLI surface (same arguments and JSON as the Python action)
;; ============================================================

(define (flag-integer flags name)
  (define raw (hash-ref flags name #f))
  (and raw
       (let ([value (string->number raw)])
         (if (and value (exact-integer? value))
             value
             (refuse! (format "invalid integer for ~a: ~a" name raw))))))

(define (flag-true? flags name)
  (define raw (hash-ref flags name #f))
  (and raw (not (member raw '("0" "false" "#f")))))

(define (auto-review-argv! argv)
  (define action (and (pair? argv) (car argv)))
  (define flags (read-flag-value (if (pair? argv) (cdr argv) '())))
  (unless (equal? action "auto-review")
    (refuse! (format "unsupported action ~a; this module implements auto-review only"
                     (or action ""))))
  (define repo (hash-ref flags "--repo" #f))
  (unless repo
    (refuse! "auto-review requires --repo"))
  (define phase (hash-ref flags "--phase" #f))
  (define stage (hash-ref flags "--stage" #f))
  (define expected-head (hash-ref flags "--expected-head" #f))
  (define expected-branch (hash-ref flags "--expected-branch" #f))
  (define campaign-root (hash-ref flags "--campaign-root" #f))
  (define generation (flag-integer flags "--generation"))
  (define staging-root (hash-ref flags "--staging-root" #f))
  (unless (and phase
               stage
               expected-head
               expected-branch
               campaign-root
               generation
               staging-root)
    (refuse! (string-append "auto-review requires --phase, --stage, --expected-head, "
                            "--expected-branch, --campaign-root, --generation and "
                            "--staging-root")))
  (auto-review! repo
                (hash-ref flags "--plan" #f)
                (flag-integer flags "--wave")
                phase
                stage
                expected-head
                expected-branch
                campaign-root
                generation
                staging-root
                #:reviewer-output (hash-ref flags "--reviewer-output" #f)
                #:manual-compatibility? (flag-true? flags "--manual-review-compatibility")))

;; The CLI surface: success -> JSON on stdout, exit 0; a typed controller
;; refusal -> delivery-pending JSON, exit 2 (the exact shape the dispatcher's
;; typed-stop parser expects); a review-infra stop -> its typed JSON, exit 0.
(define (auto-review-run! argv)
  (with-handlers ([exn:fail:review-infra?
                   (lambda (error)
                     (hash 'status "review-infra" 'reason (exn-message error)))]
                  [(lambda (error) (exn:fail? error))
                   (lambda (error)
                     (hash 'status "delivery-pending" 'reason (exn-message error)))])
    (auto-review-argv! argv)))

;; The trusted-controller seam used by extensions/gsd/review-dispatch.rkt:
;; returns the payload as JSON, EXCEPT for infrastructure faults which are
;; re-raised with their typed cause so the dispatcher maps them to review-infra
;; (never to a controller refusal).
(define (auto-review-seam-run! argv)
  (with-handlers ([exn:fail:delivery-action?
                   (lambda (error)
                     (define message (exn-message error))
                     (if (infrastructure-refusal? message)
                         (raise (exn:fail:review-infra
                                 (redact-delivery-text message)
                                 (current-continuation-marks)))
                         (hash 'status "delivery-pending" 'reason message)))])
    (auto-review-argv! argv)))

(define (auto-review-main argv)
  (define result (auto-review-run! argv))
  (printf "~a\n" (jsexpr-string result))
  (exit (if (equal? (hash-ref result 'status #f) "delivery-pending") 2 0)))

(module+ main
  (auto-review-main (vector->list (current-command-line-arguments))))
