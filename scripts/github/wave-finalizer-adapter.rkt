#lang racket/base

;; Protected-wave finalizer adapter (port job J2): the authenticated GitHub
;; adapter the wave finalizer controller is driven through.
;;
;; The Python controller (scripts/github/wave-finalizer.py) never invokes `gh`
;; itself: its `GitHubClient` protocol is supplied by a digest-verifying
;; launcher. This module is that adapter for the Racket port.
;; scripts/github/wave-finalizer.rkt owns the controller and the
;; exn:fail:finalizer taxonomy; this module REQUIRES it read-only for the error
;; struct and adds no controller logic of its own.
;;
;; Seam law: every request goes through ONE q delivery session
;; (scripts/gsd-delivery-api.rkt, `make-delivery-api-session`). The session owns
;; exactly one (budget-seconds, start-ms) window, credential isolation
;; (controller-environment re-adds GH_TOKEN/GITHUB_TOKEN to an otherwise
;; scrubbed environment for THAT ONE process), process-group cleanup and the
;; typed exn:fail:delivery-action refusal. Raw gh stderr is never echoed by the
;; seam, so a token a child prints cannot leak into a refusal.
;;
;; ONE-BUDGET LAW: one adapter = one session = one budget window. Reads use the
;; session's own closures (`api-call`, `checks`); the mutation/GraphQL runner is
;; built from the SESSION's (budget, start-ms) pair — never from a fresh
;; budget-seconds/now and never from a second session — so a read that spends
;; the budget makes every later mutation refuse with `budget-exhausted`. The
;; runner is a constructor seam (`#:make-runner`) so a caller can hand in the
;; session's own runner (the documented production wiring) and a test can prove
;; exactly ONE runner is created per adapter.
;;
;; Error taxonomy (exn:fail:finalizer; the kind is the Python class name):
;;   * a TRANSPORT failure raised by the seam is wrapped exactly like Python's
;;     `_api_call`: kind "ApiUncertainty", message
;;     "GitHub API uncertainty during <label>: <cause>". A finalizer error raised
;;     by the adapter itself is re-raised unchanged (Python re-raises
;;     FinalizerError), so no message is ever wrapped twice.
;;   * an adapter-boundary SHAPE refusal keeps the Python adapter-protocol
;;     message unwrapped: "<label> response is not an object",
;;     "<label>[<i>] is malformed", "malformed GitHub mutation response".
;;   * a missing launcher injection is kind "AttestationError"; a missing
;;     caller-supplied GraphQL text is kind "ConfigurationError".
;;
;; Call shapes mirror the Python protocol method for method:
;;   get_pull_request         gh api repos/<slug>/pulls/<n>            (+refresh)
;;   get_branch_protection    gh api repos/<slug>/branches/<ref>/protection
;;                            (a RAW api call: the delivery-api protection
;;                            helper hardcodes main and has different refusals,
;;                            so it is never reused here)
;;   list_workflow_runs       gh api repos/<slug>/actions/runs?head_sha=<sha>
;;                            &per_page=100&page=<n> — manual pages with
;;                            fail-closed completeness
;;   list_check_runs          the session's `checks` call
;;   merge_pull_request       gh api -X PUT repos/<slug>/pulls/<n>/merge
;;                            -f merge_method=<m> -f sha=<head> (sorted fields)
;;   get_issue                gh api repos/<slug>/issues/<n>
;;   close_issue              gh api -X PATCH repos/<slug>/issues/<n>
;;                            -f state=closed
;;   get/update_project_field gh api graphql -X POST -f query=<caller text> ...
;;   verify_attested_evidence the injected digest-verifying procedure
;;
;; GraphQL contract (no GraphQL text is invented here): the caller supplies the
;; exact query/mutation text and that text must project the field value at
;; `data.node.field.value`; the adapter returns exactly that jsexpr and refuses
;; a response that does not carry it. A configured value is passed through
;; unchanged as its JSON text.

(require json
         racket/list
         racket/string
         "../gsd-delivery-api.rkt"
         "../gsd-delivery-git.rkt"
         "wave-finalizer.rkt")

(provide make-wave-finalizer-adapter
         wave-finalizer-adapter?
         wave-finalizer-adapter-repository
         wave-finalizer-adapter-session
         wave-finalizer-adapter-runner
         wave-finalizer-adapter-budget
         wave-finalizer-adapter-deadline-ms
         wave-finalizer-adapter-get-pr
         wave-finalizer-adapter-get-protection
         wave-finalizer-adapter-list-workflow-runs
         wave-finalizer-adapter-list-check-runs
         wave-finalizer-adapter-merge-pr
         wave-finalizer-adapter-get-issue
         wave-finalizer-adapter-close-issue
         wave-finalizer-adapter-get-field
         wave-finalizer-adapter-update-field
         wave-finalizer-adapter-verify-evidence
         wave-finalizer-adapter-main
         missing-launcher-message
         workflow-runs-page-size)

;; ============================================================
;; Constants
;; ============================================================

;; Python's gh_put/gh_post request a 60 s timeout; the shared runner clamps it
;; to the remaining total process budget.
(define mutation-request-timeout 60.0)

;; GitHub's maximum page size; the manual loop below never truncates a list.
(define workflow-runs-page-size 100)

;; The exact Python argparse error text (exit 2): without an injected,
;; digest-verifying adapter there is no authenticated client at all.
(define missing-launcher-message
  "the digest-verifying launcher must inject an authenticated GitHub adapter")

;; ============================================================
;; Refusal taxonomy
;; ============================================================

(define (refuse kind message)
  (raise (exn:fail:finalizer message (current-continuation-marks) kind)))

(define (uncertain message)
  (refuse "ApiUncertainty" message))

;; Python `_api_call`: a FinalizerError is re-raised unchanged, every other
;; exception becomes an ApiUncertainty naming the operation. The seam's typed
;; refusal already names its CAUSE and never carries child output, so the
;; wrapped message stays free of credential material.
(define (seam-call label thunk)
  (with-handlers ([(lambda (error) (exn:fail:finalizer? error)) (lambda (error) (raise error))]
                  [exn:fail? (lambda (error)
                               (uncertain (format "GitHub API uncertainty during ~a: ~a"
                                                  label
                                                  (exn-message error))))])
    (thunk)))

;; ============================================================
;; Response-shape parity with the Python adapter protocol
;; ============================================================

;; Python `_api_mapping(value, label)`.
(define (object-response label value)
  (unless (hash? value)
    (uncertain (format "~a response is not an object" label)))
  value)

;; Python `_api_items(value, label)`: a bare sequence, or the explicit
;; {"items": [...], "complete": true} shape; anything else is uncertainty.
(define (items-response label value)
  (define items
    (cond
      [(hash? value)
       (unless (and (= 2 (hash-count value))
                    (hash-has-key? value 'items)
                    (hash-has-key? value 'complete)
                    (eq? (hash-ref value 'complete) #t))
         (uncertain (format "~a collection is not explicitly complete" label)))
       (hash-ref value 'items)]
      [else value]))
  (unless (list? items)
    (uncertain (format "~a is not a complete sequence" label)))
  (for ([item (in-list items)]
        [index (in-naturals)])
    (unless (hash? item)
      (uncertain (format "~a[~a] is malformed" label index))))
  items)

;; The jsexpr at `path`, with an explicit presence flag so a JSON null is a
;; present value and never mistaken for a missing field.
(define (path-lookup value path)
  (cond
    [(null? path) (values #t value)]
    [(hash? value)
     (define key (car path))
     (if (hash-has-key? value key)
         (path-lookup (hash-ref value key) (cdr path))
         (values #f #f))]
    [else (values #f #f)]))

;; The caller's GraphQL text projects the field value at data.node.field.value;
;; a response without it is uncertainty, never a guessed value.
(define (field-value label response)
  (define-values (present? value) (path-lookup response '(data node field value)))
  (unless present?
    (uncertain (format "~a response is missing data.node.field.value" label)))
  value)

;; ============================================================
;; Argument vectors (explicit, sorted, shell-free)
;; ============================================================

(define (field-args fields)
  (append* (for/list ([field (in-list fields)])
             (list "-f" (format "~a=~a" (car field) (cdr field))))))

;; Python `for key in sorted(fields)`: the argv is deterministic regardless of
;; how a caller orders the fields.
(define (rest-mutation-args method route fields)
  (append (list "api" "--hostname" "github.com" "-X" method route)
          (field-args (sort fields string<? #:key car))))

;; gh's graphql subcommand is a POST with the query text as ONE argv element.
(define (graphql-args query-text fields)
  (append (list "api" "graphql" "-X" "POST" "-f" (string-append "query=" query-text))
          (field-args fields)))

;; ============================================================
;; Session, runner and the adapter
;; ============================================================

(struct wave-finalizer-adapter
        (repository session api checks runner get-field-query update-field-mutation evidence-verifier)
  #:transparent)

(define (repository-slug? value)
  (and (string? value)
       (= 1 (for/sum ([char (in-string value)]) (if (char=? char #\/) 1 0)))
       (let ([parts (string-split value "/")])
         (and (= 2 (length parts))
              (for/and ([part (in-list parts)])
                (not (string=? part "")))))))

(define (make-wave-finalizer-adapter repository
                                     #:verify-evidence [verify-evidence #f]
                                     #:get-field-query [get-field-query #f]
                                     #:update-field-mutation [update-field-mutation #f]
                                     #:budget [budget #f]
                                     #:start-ms [start-ms #f]
                                     #:make-runner [make-runner make-bounded-runner])
  (unless (repository-slug? repository)
    (refuse "ConfigurationError" "repository must be owner/name"))
  (when (and verify-evidence (not (procedure? verify-evidence)))
    (refuse "ConfigurationError" "verify-evidence must be an injected procedure"))
  ;; THE one session of this adapter: it owns the single budget window every
  ;; read and every mutation draws down.
  (define session (make-delivery-api-session repository #:budget budget #:start-ms start-ms))
  ;; THE one bounded runner of this adapter, built from the session's OWN pair:
  ;; identical remaining-budget arithmetic, never a fresh budget or start time.
  (define run
    (make-runner (delivery-api-session-budget session) (delivery-api-session-start-ms session)))
  (wave-finalizer-adapter repository
                          session
                          (delivery-api-session-api-call session)
                          (delivery-api-session-checks session)
                          run
                          get-field-query
                          update-field-mutation
                          verify-evidence))

(define (wave-finalizer-adapter-budget adapter)
  (delivery-api-session-budget (wave-finalizer-adapter-session adapter)))

(define (wave-finalizer-adapter-deadline-ms adapter)
  (+ (delivery-api-session-start-ms (wave-finalizer-adapter-session adapter))
     (* 1000.0 (wave-finalizer-adapter-budget adapter))))

(define (adapter-slug adapter)
  (wave-finalizer-adapter-repository adapter))

(define (adapter-api adapter slug route #:refresh? [refresh? #f])
  ((wave-finalizer-adapter-api adapter) slug route #:refresh? refresh?))

;; ============================================================
;; Reads
;; ============================================================

(define (wave-finalizer-adapter-get-pr adapter number #:refresh? [refresh? #f])
  (object-response
   "pull request"
   (seam-call
    "get pull request"
    (lambda ()
      (adapter-api adapter (adapter-slug adapter) (format "pulls/~a" number) #:refresh? refresh?)))))

;; A RAW api call, never the delivery-api `protection` helper: that helper is
;; pinned to branches/main and refuses with a different reason chain.
(define (wave-finalizer-adapter-get-protection adapter ref)
  (object-response
   "branch protection"
   (seam-call "get branch protection"
              (lambda ()
                (adapter-api adapter (adapter-slug adapter) (format "branches/~a/protection" ref))))))

(define (workflow-runs-route sha page)
  (format "actions/runs?head_sha=~a&per_page=~a&page=~a" sha workflow-runs-page-size page))

;; Manual in-process pagination, mirroring `checks` in gsd-delivery-api.rkt: page
;; until the accumulated runs cover total_count, and refuse a short result
;; instead of returning a truncated (silently incomplete) run list. Every page
;; is read WITHOUT the session memo (`#:refresh? #t`): the controller compares
;; the pre-merge run ids between prepare and finalize, so a cached page could
;; hide a newly created attempt and turn that comparison into a no-op.
(define (wave-finalizer-adapter-list-workflow-runs adapter sha)
  (define slug (adapter-slug adapter))
  (define-values (runs total)
    (let loop ([page 1]
               [accumulated '()])
      (define data
        (seam-call "list workflow runs"
                   (lambda ()
                     (adapter-api adapter slug (workflow-runs-route sha page) #:refresh? #t))))
      (unless (and (hash? data) (list? (hash-ref data 'workflow_runs #f)))
        (uncertain "invalid workflow run result"))
      (define page-runs (hash-ref data 'workflow_runs))
      (unless (for/and ([run (in-list page-runs)])
                (hash? run))
        (uncertain "invalid workflow run entry"))
      ;; Same strictness as `malformed check-total`: a boolean/float/string
      ;; total_count fails closed instead of paginating on nonsense.
      (define page-total (hash-ref data 'total_count #f))
      (unless (and (exact-integer? page-total) (>= page-total 0))
        (uncertain "malformed workflow-run total"))
      (define next (append accumulated page-runs))
      (if (or (null? page-runs) (>= (length next) page-total))
          (values next page-total)
          (loop (add1 page) next))))
  ;; A short list is only acceptable when it already covers the advertised
  ;; total; otherwise the read is incomplete and fails closed.
  (unless (>= (length runs) total)
    (uncertain "workflow run pagination incomplete"))
  runs)

;; Check runs have no refresh path in the delivery api session (its `checks`
;; closure memoizes per (slug, sha)), so a fresh read pages the SAME route
;; through the session's api closure with `#:refresh? #t` -- the same rules as
;; `checks` in gsd-delivery-api.rkt (per-page total_count validation, short
;; result fails closed) and the same single budget window.
(define (check-runs-route sha page)
  (format "commits/~a/check-runs?per_page=100&page=~a" sha page))

(define (fresh-check-runs adapter sha)
  (define slug (adapter-slug adapter))
  (let loop ([page 1]
             [accumulated '()])
    (define data
      (seam-call "list check runs"
                 (lambda ()
                   (adapter-api adapter slug (check-runs-route sha page) #:refresh? #t))))
    (unless (and (hash? data) (list? (hash-ref data 'check_runs #f)))
      (uncertain "invalid check result"))
    (define page-runs (hash-ref data 'check_runs))
    (unless (for/and ([run (in-list page-runs)]) (hash? run))
      (uncertain "invalid check run entry"))
    (define page-total (hash-ref data 'total_count #f))
    (unless (and (exact-integer? page-total) (>= page-total 0))
      (uncertain "malformed check-total"))
    (define next (append accumulated page-runs))
    (if (or (null? page-runs) (>= (length next) page-total))
        next
        (loop (add1 page) next))))

(define (wave-finalizer-adapter-list-check-runs adapter sha #:refresh? [refresh? #f])
  (define runs
    (if refresh?
        (fresh-check-runs adapter sha)
        (seam-call "list check runs"
                   (lambda () ((wave-finalizer-adapter-checks adapter) (adapter-slug adapter) sha)))))
  (items-response "check runs" runs))

(define (wave-finalizer-adapter-get-issue adapter number)
  (object-response
   (format "issue ~a" number)
   (seam-call (format "get issue ~a" number)
              (lambda () (adapter-api adapter (adapter-slug adapter) (format "issues/~a" number))))))

;; ============================================================
;; Mutations
;; ============================================================

(define (run-gh adapter args)
  ((wave-finalizer-adapter-runner adapter) "gh" args mutation-request-timeout))

;; Python's mutation boundary: the response must be a JSON object, refused with
;; the exact protocol message otherwise. An unparseable payload reports the same
;; cause, wrapped like every other transport failure.
(define (mutation-response label text)
  (define value
    (with-handlers ([exn:fail?
                     (lambda (_error)
                       (uncertain
                        (format "GitHub API uncertainty during ~a: malformed GitHub mutation response"
                                label)))])
      (string->jsexpr text)))
  (unless (hash? value)
    (uncertain "malformed GitHub mutation response"))
  value)

(define (rest-mutation adapter method route label fields)
  (mutation-response
   label
   (seam-call label (lambda () (run-gh adapter (rest-mutation-args method route fields))))))

;; merge_method before sha: the field list is deliberately unordered here and
;; sorted by the argument builder.
(define (wave-finalizer-adapter-merge-pr adapter number #:sha sha #:merge-method merge-method)
  (rest-mutation adapter
                 "PUT"
                 (format "repos/~a/pulls/~a/merge" (adapter-slug adapter) number)
                 "merge pull request"
                 (list (cons "sha" sha) (cons "merge_method" merge-method))))

(define (wave-finalizer-adapter-close-issue adapter number)
  (rest-mutation adapter
                 "PATCH"
                 (format "repos/~a/issues/~a" (adapter-slug adapter) number)
                 (format "close issue ~a" number)
                 (list (cons "state" "closed"))))

;; ============================================================
;; GraphQL project field (caller-supplied text, unchanged value)
;; ============================================================

(define (required-graphql-text adapter get?)
  (define text
    (if get?
        (wave-finalizer-adapter-get-field-query adapter)
        (wave-finalizer-adapter-update-field-mutation adapter)))
  (unless (and (string? text) (not (string=? text "")))
    (refuse "ConfigurationError"
            (if get?
                "the caller must supply the GraphQL project field query text"
                "the caller must supply the GraphQL project field mutation text")))
  text)

(define (field-arguments project-id item-id field-id)
  (list (cons "projectId" project-id) (cons "itemId" item-id) (cons "fieldId" field-id)))

(define (graphql-request adapter text arguments label)
  (field-value label
               (mutation-response
                label
                (seam-call label (lambda () (run-gh adapter (graphql-args text arguments)))))))

(define (wave-finalizer-adapter-get-field adapter project-id item-id field-id)
  (graphql-request adapter
                   (required-graphql-text adapter #t)
                   (field-arguments project-id item-id field-id)
                   "read project field"))

;; The configured value is passed through UNCHANGED, as its JSON text.
(define (wave-finalizer-adapter-update-field adapter project-id item-id field-id value)
  (graphql-request adapter
                   (required-graphql-text adapter #f)
                   (append (field-arguments project-id item-id field-id)
                           (list (cons "value" (jsexpr-string value))))
                   "update project field"))

;; ============================================================
;; Injected evidence verification
;; ============================================================

(define (wave-finalizer-adapter-verify-evidence adapter repository reference)
  (define verify (wave-finalizer-adapter-evidence-verifier adapter))
  (unless (procedure? verify)
    (refuse "AttestationError" missing-launcher-message))
  (object-response "attested evidence"
                   (seam-call "verify GitHub-attested evidence"
                              (lambda () (verify repository reference)))))

;; ============================================================
;; CLI
;; ============================================================

;; Parity with the Python CLI: without an injected, digest-verifying adapter the
;; launcher cannot authenticate anything, so the process refuses exactly like
;; argparse (usage + message on stderr, exit 2). A successful run only reports
;; the adapter's budget window — this entry point performs no mutation.
(define (wave-finalizer-adapter-main argv
                                     #:verify-evidence [verify-evidence #f]
                                     #:exit [exit-procedure exit])
  (cond
    [(not (procedure? verify-evidence))
     (eprintf "usage: wave-finalizer-adapter --repo <owner/name>\n")
     (eprintf "wave-finalizer-adapter: error: ~a\n" missing-launcher-message)
     (exit-procedure 2)]
    [else
     (with-handlers ([exn:fail? (lambda (error)
                                  (eprintf "wave-finalizer-adapter: ~a\n" (exn-message error))
                                  (exit-procedure 1))])
       (define flags (read-flag-value argv))
       (define repository (hash-ref flags "--repo" #f))
       (unless repository
         (refuse "ConfigurationError" "wave-finalizer-adapter requires --repo"))
       (define adapter
         (make-wave-finalizer-adapter repository
                                      #:verify-evidence verify-evidence
                                      #:get-field-query (hash-ref flags "--get-field-query" #f)
                                      #:update-field-mutation
                                      (hash-ref flags "--update-field-mutation" #f)))
       (printf "~a\n"
               (jsexpr-string (hasheq 'status
                                      "ready"
                                      'repository
                                      (wave-finalizer-adapter-repository adapter)
                                      'budget-seconds
                                      (wave-finalizer-adapter-budget adapter)
                                      'deadline-ms
                                      (wave-finalizer-adapter-deadline-ms adapter))))
       (exit-procedure 0))]))

(module+ main
  (wave-finalizer-adapter-main (vector->list (current-command-line-arguments))))
