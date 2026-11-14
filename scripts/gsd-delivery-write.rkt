#lang racket/base

;; Phase B slice S5 — native GitHub WRITE seam plus the `open-pr` action of
;; scripts/gsd-delivery.py.
;;
;; Scope law: this module is the MUTATION seam (`gh_put`, `gh_post`) and the
;; ONE action that uses it so far (`open_pr`). It is NOT wired into
;; extensions/gsd/delivery-coordinator.rkt, it does not edit or replace
;; scripts/gsd-delivery.py, and the Python controller stays the production path
;; until an explicit later slice says otherwise. It reuses the S3 read layer
;; (scripts/gsd-delivery-api.rkt: the api session with its memo + `#:refresh?`
;; bypass), the S4 read-only ladder helpers (scripts/gsd-delivery-pr.rkt:
;; repository, refresh, resolve-existing-pr, validate-pr-identity,
;; require-receipt-tip, binding-path) and the shared bounded seam
;; (scripts/gsd-delivery-git.rkt). Nothing in those modules was modified.
;;
;; ONE-BUDGET LAW (B2), inherited: an action builds ONE
;; delivery-pr-context (make-delivery-pr-context), which owns ONE
;; (budget-seconds, start-ms) pair and builds the gh runner, the repo-git
;; command and this module's mutations from that SAME pair. Every `gh api`
;; mutation therefore draws down the SAME decreasing budget as the reads and the
;; fetches around it, exactly like Python's process-wide remaining_budget(). This
;; module never calls make-bounded-runner / make-repo-git-command: a second
;; runner would silently grant a fresh budget.
;;
;; NON-IDEMPOTENT-POST GUARD: a POST is not retried, ever. `gh-mutation` performs
;; EXACTLY ONE bounded attempt and either returns the parsed object or raises
;; the seam's typed refusal (budget-exhausted / delivery-timeout /
;; command-not-found / command-unavailable / output-truncated / the classified
;; command failure). There is no retry loop, no retry parameter and no
;; "retry once on transport error" path, because a repeated POST can open a
;; DUPLICATE pull request when the first request actually succeeded and only its
;; response was lost. Recovery is resolve-before-create on the next explicit
;; run: `open-pr` re-resolves an existing PR for the durable branch BEFORE any
;; creation, so an operator rerun after a failed POST reuses the PR if the
;; mutation really landed.
;;
;; Parity law (the Python functions are the contract; refusal strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   gh_put(slug, route, fields) / gh_post(slug, route, fields)
;;     `gh api --hostname github.com -X PUT|POST repos/<slug>/<route>
;;      -f k=v ...`, with the fields ordered by `sorted(fields)` exactly like
;;     Python and each `k=v` sent as ONE argv element (subprocess argv vector,
;;     never a shell). 60 s requested timeout (clamped by the shared runner to
;;     the remaining budget). The response is JSON-parsed and must be an OBJECT:
;;     any parse failure or non-object payload refuses with
;;     "malformed GitHub mutation response". A non-zero exit keeps the shared
;;     seam's typed cause; raw gh stderr is never surfaced.
;;
;;   open_pr(repo, plan, wave, expected_branch, expected_head=None) -> hash
;;     A non-blank durable branch is mandatory ("open-pr requires
;;     --expected-branch"). Order is load-bearing and preserved: resolve the
;;     repository identity, resolve at most one EXISTING OPEN PR for the branch
;;     (resolve-before-create), refresh origin/main, then force-refresh
;;     refs/remotes/origin/<branch> from refs/heads/<branch> and read the tip.
;;     The tip must be a full 40-hex SHA ("malformed fetched implementation
;;     branch tip") and must satisfy the receipt-tip rule (receipt head is an
;;     ancestor; only excluded evidence directories may have changed since).
;;     With no existing PR it creates one: title "W<wave> delivery: <branch>",
;;     the exact six-line body (Plan / Wave / Head / Branch / Binding evidence /
;;     Review), fields {base, body, head, title} sent in that sorted order. The
;;     creation response must be an object whose `number` is a positive integer
;;     ("GitHub pull-request creation response has no usable number").
;;     The PR is then re-read with a cache BYPASS (`#:refresh? #t`) — creation
;;     must be proven from a fresh read — and must be an object
;;     ("malformed pull-request response"), open ("pull request is not open"), a
;;     same-repository main-base PR, carry a positive integer number ("created
;;     pull request has no usable number"), a full head SHA ("created pull
;;     request has no full head SHA") equal to the fetched tip ("fetched
;;     implementation branch tip differs from pull-request head") on the
;;     expected branch ("pull request branch does not match expected branch").
;;     Returns {'status':'opened','pr':<n>,'branch':<branch>,'head':<sha>}; an
;;     existing PR takes the identical re-read/validation path (only the number
;;     source differs: "resolved pull request has no usable number") and is
;;     never duplicated.
;;
;; Documented deviation (fail closed, never open):
;;   * `gh_graphql` has NO counterpart in scripts/gsd-delivery.py: the
;;     controller contains no GraphQL call at all, so there is no Python argv or
;;     refusal string to be byte-identical to. It is provided because this slice
;;     owns the write seam, and its argv follows the project's only documented
;;     GraphQL shape (scripts/github/wave-finalizer-adapter.rkt:
;;     `gh api … graphql -X POST -f query=<text> …`). A blank query refuses
;;     ("graphql requires a non-empty query") and the response must be an object
;;     ("malformed GitHub mutation response"), matching the REST mutation seam.
;;     It is deliberately EXCLUDED from the differential parity table because
;;     Python has nothing to compare against.
;;   * open_pr keeps Python's defensive `require(isinstance(created, dict),
;;     'malformed GitHub pull-request creation response')`, but that guard is
;;     UNREACHABLE through this seam: gh_post itself refuses every non-object
;;     payload with "malformed GitHub mutation response" first, so a non-object
;;     creation response surfaces the seam's reason (the tests pin exactly
;;     that). The guard is kept so the Racket shape still mirrors the Python
;;     expression, never to relax a check.
;;   * an UNPARSEABLE mutation response (not JSON at all) refuses with the typed
;;     "malformed GitHub mutation response"; Python's gh_put/gh_post let
;;     `json.loads` raise and main() then reports the raw JSONDecodeError text.
;;     A payload that PARSES but is not an object is byte-identical on both
;;     sides.
;;   * a non-object candidate returned by the existing-PR query is a typed
;;     "resolved pull request has no usable number" refusal; Python would raise
;;     AttributeError on `.get` and report that raw text.
;;   * JSON integers: Python's `type(n) is int` and Racket's `exact-integer?`
;;     agree on every JSON number except that Racket has no separate bignum
;;     bound; `true`/`false`/`1.0` are refused by both.

(require json
         racket/list
         racket/string
         "gsd-delivery-git.rkt"
         "gsd-delivery-api.rkt"
         "gsd-delivery-pr.rkt")

;; write seam (mutations)
(provide gh-put
         gh-post
         gh-graphql
         ;; pure argv builders (exported so a test can pin the exact argument vector)
         mutation-argv
         graphql-argv
         ;; the action
         delivery-open-pr!
         open-pr-body
         ;; CLI surface (same JSON shapes as the Python action)
         delivery-write-action!
         delivery-write-main)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

;; Python's gh_put/gh_post pass `timeout=60`; the shared runner clamps it to the
;; remaining total process budget. Reads use the same 60 s request.
(define mutation-request-timeout 60.0)

;; ============================================================
;; Small helpers (same semantics as the S3/S4 modules)
;; ============================================================

;; Python `require(ok, reason)`.
(define (ensure! ok message)
  (unless ok
    (refuse! message)))

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

;; Python dig: any non-hash along the path yields None (#f). Local copy of the
;; read layer's private helper (those modules do not export it).
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

;; ============================================================
;; Mutation argv (byte-identical to the Python controller)
;; ============================================================

;; Python renders each field with an f-string and orders the keys with
;; `sorted(fields)`. Both are reproduced here so the argv a test logs is the
;; argv Python sends.
(define (normalize-field entry)
  (define key (car entry))
  (unless (and (string? key) (not (string=? key "")))
    (refuse! "invalid GitHub mutation field"))
  (cons key
        (if (string? (cdr entry))
            (cdr entry)
            (format "~a" (cdr entry)))))

(define (sorted-fields fields)
  (unless (and (list? fields)
               (for/and ([entry (in-list fields)])
                 (pair? entry)))
    (refuse! "invalid GitHub mutation fields"))
  (sort (map normalize-field fields) string<? #:key car))

;; `gh api --hostname github.com -X <method> repos/<slug>/<route> -f k=v ...`
;; gh is spawned with an ARGUMENT VECTOR, so a `k=v` containing spaces or
;; newlines (the PR body) is ONE literal argv element — never shell-split.
(define (mutation-argv method slug route fields)
  (append (list "api" "--hostname" "github.com" "-X" method (format "repos/~a/~a" slug route))
          (append* (for/list ([entry (in-list (sorted-fields fields))])
                     (list "-f" (string-append (car entry) "=" (cdr entry)))))))

;; `gh api --hostname github.com graphql -X POST -f query=<text> -f k=v ...`
;; (see the gh_graphql deviation note in the file header). The caller's query
;; text stays ONE argv element; variables are ordered by name for determinism.
(define (graphql-argv query variables)
  (unless (nonblank-string? query)
    (refuse! "graphql requires a non-empty query"))
  (append
   (list "api" "--hostname" "github.com" "graphql" "-X" "POST" "-f" (string-append "query=" query))
   (append* (for/list ([entry (in-list (sorted-fields variables))])
              (list "-f" (string-append (car entry) "=" (cdr entry)))))))

;; ============================================================
;; Mutation transport: EXACTLY ONE bounded attempt
;; ============================================================

;; Python: `json.loads(...)` failures and a non-dict payload both refuse with
;; "malformed GitHub mutation response". The command's own typed refusal is
;; raised BEFORE this function is reached and is never swallowed here.
(define (mutation-response text)
  (define data
    (with-handlers ([exn:fail? (lambda (_error) (refuse! "malformed GitHub mutation response"))])
      (string->jsexpr text)))
  (unless (hash? data)
    (refuse! "malformed GitHub mutation response"))
  data)

;; The ONE mutation attempt. `run` is the context's own bounded runner, so the
;; mutation shares the action's single budget. There is deliberately no retry:
;; see the NON-IDEMPOTENT-POST GUARD note at the top of this file.
(define (gh-mutation ctx method slug route fields)
  (define run (delivery-pr-context-run ctx))
  (mutation-response (run "gh" (mutation-argv method slug route fields) mutation-request-timeout)))

(define (gh-put ctx slug route fields)
  (gh-mutation ctx "PUT" slug route fields))

(define (gh-post ctx slug route fields)
  (gh-mutation ctx "POST" slug route fields))

(define (gh-graphql ctx query variables)
  (define run (delivery-pr-context-run ctx))
  (mutation-response (run "gh" (graphql-argv query variables) mutation-request-timeout)))

;; ============================================================
;; open-pr
;; ============================================================

;; The EXACT body Python builds (byte-identical, trailing newline included).
(define (open-pr-body plan wave tip expected-branch evidence)
  (string-append (format "Plan: ~a\n" plan)
                 (format "Wave: W~a\n" wave)
                 (format "Head: ~a\n" tip)
                 (format "Branch: ~a\n" expected-branch)
                 (format "Binding evidence: ~a\n" evidence)
                 (string-append "Review: genuine independent review required at the exact "
                                "implementation head.\n")))

;; Resolve-before-create an implementation PR from the fetched branch tip.
;;
;; The branch name is durable input, but the PR head is read from the remote
;; branch after a refresh; the current checkout is never used as the head.
;; Order is preserved from Python: existing-PR resolution happens BEFORE the
;; refresh/fetch, so a re-run after a lost creation response reuses the PR
;; instead of creating a duplicate.
(define (delivery-open-pr! repo
                           plan
                           wave
                           expected-branch
                           #:expected-head [expected-head #f]
                           #:context [context #f])
  (unless (nonblank-string? expected-branch)
    (refuse! "open-pr requires --expected-branch"))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define slug (repository git))
  ;; 1. Resolve-before-create: at most one OPEN PR may already target the
  ;;    durable branch; two fail closed inside resolve-existing-pr.
  (define existing (resolve-existing-pr api slug expected-branch))
  ;; 2. Refresh the default branch, then the wave branch itself.
  (refresh git)
  (define remote-ref (string-append "refs/remotes/origin/" expected-branch))
  ;; Forced refspec: a rewritten wave branch refreshes the remote-tracking ref.
  ;; The re-read PR identity below still pins the exact fetched tip.
  (git (list "fetch" "--no-tags" "origin" (format "+refs/heads/~a:~a" expected-branch remote-ref)))
  (define tip (string-trim (git (list "rev-parse" remote-ref))))
  (unless (full-sha? tip)
    (refuse! "malformed fetched implementation branch tip"))
  (require-receipt-tip git expected-head tip)
  ;; 3. Create only when no PR exists; otherwise reuse the resolved identity.
  (define pr
    (cond
      [(not existing)
       ;; Validate the campaign/wave identity BEFORE mutating anything.
       (define evidence (binding-path plan wave))
       (define title (format "W~a delivery: ~a" wave expected-branch))
       (define body (open-pr-body plan wave tip expected-branch evidence))
       (define created
         (gh-post ctx
                  slug
                  "pulls"
                  (list (cons "title" title)
                        (cons "body" body)
                        (cons "head" expected-branch)
                        (cons "base" "main"))))
       (ensure! (hash? created) "malformed GitHub pull-request creation response")
       (define created-number (hash-ref created 'number #f))
       (unless (and (exact-integer? created-number) (> created-number 0))
         (refuse! "GitHub pull-request creation response has no usable number"))
       (api slug (format "pulls/~a" created-number) #:refresh? #t)]
      [else
       (define existing-number (and (hash? existing) (hash-ref existing 'number #f)))
       (unless (and (exact-integer? existing-number) (> existing-number 0))
         (refuse! "resolved pull request has no usable number"))
       (api slug (format "pulls/~a" existing-number) #:refresh? #t)]))
  ;; 4. Prove the mutation from a FRESH read before exposing any identity.
  (ensure! (hash? pr) "malformed pull-request response")
  (unless (equal? (hash-ref pr 'state #f) "open")
    (refuse! "pull request is not open"))
  (validate-pr-identity pr slug)
  (define number (hash-ref pr 'number #f))
  (define head (dig pr 'head 'sha))
  (unless (and (exact-integer? number) (> number 0))
    (refuse! "created pull request has no usable number"))
  (unless (full-sha? head)
    (refuse! "created pull request has no full head SHA"))
  (unless (equal? head tip)
    (refuse! "fetched implementation branch tip differs from pull-request head"))
  (unless (equal? (dig pr 'head 'ref) expected-branch)
    (refuse! "pull request branch does not match expected branch"))
  (hasheq 'status "opened" 'pr number 'branch expected-branch 'head head))

;; ============================================================
;; CLI surface (same JSON shapes as the Python action)
;; ============================================================

(define (flag-integer flags name)
  (define raw (hash-ref flags name #f))
  (and raw
       (let ([value (string->number raw)])
         (if (and value (exact-integer? value))
             value
             (refuse! (format "invalid integer for ~a: ~a" name raw))))))

(define (delivery-write-action! argv)
  (define action (and (pair? argv) (car argv)))
  (define flags
    (read-flag-value (if (pair? argv)
                         (cdr argv)
                         '())))
  (define repo (hash-ref flags "--repo" #f))
  (unless repo
    (refuse! (format "~a requires --repo" (if action action ""))))
  (cond
    [(equal? action "open-pr")
     (delivery-open-pr! repo
                        (hash-ref flags "--plan" #f)
                        (flag-integer flags "--wave")
                        (hash-ref flags "--expected-branch" #f)
                        #:expected-head (hash-ref flags "--expected-head" #f))]
    [else
     (refuse! (format "unsupported action ~a; this module implements open-pr only" (or action "")))]))

;; Success -> JSON on stdout, exit 0. Refusal -> delivery-pending JSON, exit 2
;; (the exact shape delivery-coordinator.rkt's typed-stop parser expects).
(define (delivery-write-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (printf "~a\n" (jsexpr-string (delivery-write-action! argv)))
    (exit 0)))

(module+ main
  (delivery-write-main (vector->list (current-command-line-arguments))))
