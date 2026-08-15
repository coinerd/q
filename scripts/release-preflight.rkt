#lang racket/base

;; scripts/release-preflight.rkt — Fail-fast release pre-flight (BUG-0007)
;;
;; Runs every cheap, structural release-surface invariant in seconds and exits
;; non-zero on the FIRST violation, so a malformed tag fails the Release
;; workflow (or a local check) before any expensive full-suite job starts.
;;
;; Invariants, in detection-cost order:
;;   1. tag-exists          — the named ref exists in this clone
;;   2. tag-object-type     — the tag object is ANNOTATED (cat-file -t == "tag"),
;;                            with the remediation command in the error message (S1)
;;   3. tag-version-consistency — version derived from the tag == q/info.rkt version
;;   4. manifest-dry-run    — q/scripts/gen-release-manifest.rkt --dry-run <tag>
;;                            renders the whole release surface without publishing (S2)
;;   5. release-readiness   — [--readiness] (BUG-0008) every fix required for this
;;                            release version (bug registry + GitHub milestone) has
;;                            landed in an ancestor of the tagged commit.
;;
;; Usage:
;;   racket scripts/release-preflight.rkt <tag>                 # 1-4 (structural)
;;   racket scripts/release-preflight.rkt <tag> --readiness     # 1-5 (full gate)
;;
;; Exit codes: 0 = all invariants hold; 1 = violated invariant(s) (reported);
;; 2 = usage error.

(require json
         racket/file
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/string
         racket/system)

;; ---------------------------------------------------------------------------
;; Exit protocol
;; ---------------------------------------------------------------------------

(define (usage!)
  (displayln "usage: racket scripts/release-preflight.rkt <tag> [--readiness]")
  (displayln "  <tag>        release tag to check, e.g. v1.00.00")
  (displayln "  --readiness  also gate: every fix required for this release version")
  (displayln "               (bug registry + milestone) is contained in the tagged commit")
  (exit 2))

(define (stage-ok fmt . args)
  (printf "  ok    ")
  (apply printf fmt args)
  (newline))

(define (stage-fail stage message)
  (eprintf "  FAIL  [~a] ~a\n" stage message)
  (eprintf "\nrelease preflight: refusing to proceed (fix the invariant above and re-run).\n")
  (exit 1))

;; ---------------------------------------------------------------------------
;; Environment
;; ---------------------------------------------------------------------------

;; Repo root = parent of the directory containing this script (scripts/).
;; Robust across invocation styles: when this module is *run as a script* the
;; run-file path gives us scripts/ directly; when it is merely *required*
;; (e.g. from tests), 'run-file carries no usable path, so we ask git.
(define (find-repo-root)
  (define here (path-only (find-system-path 'run-file)))
  (cond
    [(and here (relative-path? here)) (simplify-path (path->complete-path (build-path here 'up)))]
    [here (simplify-path (build-path here 'up))]
    [else
     (define out (open-output-string))
     (define code
       (parameterize ([current-output-port out])
         (system*/exit-code (find-executable-path "git") "rev-parse" "--show-toplevel")))
     (if (zero? code)
         (string->path (string-trim (get-output-string out)))
         (current-directory))]))

(define repo-root (find-repo-root))

;; ---------------------------------------------------------------------------
;; Git helpers
;; ---------------------------------------------------------------------------

;; Run `git <args...>` in repo-root; return list of lines of stdout.
;; Errors surface as a failed invariant at the call site.
(define (git-lines . args)
  (define args/strings (map ~a args))
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out])
      (apply system*/exit-code (find-executable-path "git") args/strings)))
  (if (zero? code)
      (string-split (string-trim (get-output-string out)) "\n")
      '()))

;; Run `git <args...>`; return (list exit-code stdout stderr) without dying.
(define (git-quiet . args)
  (define args/strings (map ~a args))
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") args/strings)))
  (list code (get-output-string out) (get-output-string err)))

;; ---------------------------------------------------------------------------
;; Invariant 1: tag exists
;; ---------------------------------------------------------------------------

(define (tag-exists? tag)
  (define code (car (git-quiet "rev-parse" "--verify" "--quiet" (string-append "refs/tags/" tag))))
  (zero? code))

(define (check-tag-exists! tag)
  (if (tag-exists? tag)
      (stage-ok "tag-exists: ~a resolves in this clone" tag)
      (stage-fail "tag-exists"
                  (format "tag ~a does not exist in this clone (fetch first, or check the tag name)"
                          tag))))

;; ---------------------------------------------------------------------------
;; Invariant 2: tag object type is annotated
;; ---------------------------------------------------------------------------

(define (git-tag-object-type tag)
  (define code+out (git-quiet "cat-file" "-t" tag))
  (match code+out
    [(list 0 s _) (string-trim s)]
    [_ ""]))

(define remediation-command
  ;; Single source of truth for how to repair a wrong tag object type.
  ;; Kept in sync with q/scripts/gen-release-manifest.rkt (S1).
  "git tag -fa ~a -m \"~a\" && git push origin ~a --force")

(define (check-tag-object-type! tag)
  (define type (git-tag-object-type tag))
  (cond
    [(string=? type "tag") (stage-ok "tag-object-type: ~a is an annotated tag object" tag)]
    [(string=? type "commit")
     (stage-fail "tag-object-type"
                 (string-append (format "~a is a LIGHTWEIGHT tag (points directly at a commit); " tag)
                                "release tags must be annotated so they carry the release message.\n"
                                "  fix: "
                                (format remediation-command tag tag tag)))]
    [else
     (stage-fail "tag-object-type"
                 (format "~a has unexpected object type \"~a\" (expected \"tag\")" tag type))]))

;; ---------------------------------------------------------------------------
;; Invariant 3: tag version == q/info.rkt version
;; ---------------------------------------------------------------------------

;; (define version "1.00.00") in q/info.rkt — same parse contract as
;; scripts/version-surface.rkt:parse-info-version-from-content.
(define (read-info-version)
  (define info-path (build-path repo-root "info.rkt"))
  (define content (file->string info-path))
  (define m
    (regexp-match #rx"\\(define version \"([0-9]+\\.[0-9]+\\.[0-9]+(?:-[A-Za-z0-9.-]+)?)\"" content))
  (and m (cadr m)))

(define (tag->version tag)
  (define m (regexp-match #rx"^v?([0-9]+\\.[0-9]+\\.[0-9]+(?:-[A-Za-z0-9.-]+)?)$" tag))
  (and m (cadr m)))

(define (check-tag-version-consistency! tag)
  (define tag-version (tag->version tag))
  (define info-version (read-info-version))
  (cond
    [(not tag-version)
     (stage-fail "tag-version-consistency"
                 (format "cannot parse a version out of tag name ~a (expected vMAJOR.MINOR.PATCH)"
                         tag))]
    [(not info-version)
     (stage-fail "tag-version-consistency" "cannot parse (define version ...) out of info.rkt")]
    [(string=? tag-version info-version)
     (stage-ok "tag-version-consistency: tag ~a == info.rkt version ~a" tag-version info-version)]
    [else
     (stage-fail
      "tag-version-consistency"
      (format "tag says ~a but info.rkt says ~a — bump info.rkt (and the version surface) or re-tag"
              tag-version
              info-version))]))

;; ---------------------------------------------------------------------------
;; Invariant 4: manifest dry-run renders the release surface
;; ---------------------------------------------------------------------------

(define (check-manifest-dry-run! tag)
  (define out (open-output-string))
  (define err (open-output-string))
  ;; Run from repo-root so the subprocess resolves util/version.rkt etc.
  (define code
    (parameterize ([current-directory repo-root]
                   [current-output-port out]
                   [current-error-port err])
      (system*/exit-code (find-executable-path "racket")
                         (path->string (build-path repo-root "scripts" "gen-release-manifest.rkt"))
                         "--dry-run"
                         tag)))
  (if (zero? code)
      (stage-ok "manifest dry-run: gen-release-manifest.rkt --dry-run ~a rendered the release surface"
                tag)
      (stage-fail "manifest-dry-run"
                  (format "gen-release-manifest.rkt --dry-run ~a failed (exit ~a): ~a"
                          tag
                          code
                          (get-output-string err)))))

;; ---------------------------------------------------------------------------
;; Invariant 5: release readiness (BUG-0008)
;;
;; "Which fixes must this release contain?" is derived from two cross-checked
;; sources — never hand-maintained per release:
;;
;;   * bug registry `.planning/bugs/INDEX.md`: rows whose "Fixed in" column
;;     targets this release version. The row's report file records the landing
;;     commit on a `Landing commit:` line (convention: .planning/bugs/README.md).
;;     The registry is a planning artifact (not part of the shipped repo), so it
;;     is used whenever present (repo root, parent, or $Q_BUG_REGISTRY).
;;
;;   * the GitHub milestone titled v<version>: every issue titled BUG-NNNN: ...
;;     (open or closed). Landing commit resolved from the issue's merged,
;;     cross-referenced PRs. This is the CI-side source.
;;
;; A fix is contained iff `git merge-base --is-ancestor <sha> <tagged-commit>`
;; succeeds. Missing/unknown/non-ancestor is a hard failure naming the BUG-ID,
;; the issue, and the commit that would satisfy the gate.
;; ---------------------------------------------------------------------------

(define (strip-v s) (regexp-replace "^v" s ""))

(define (release-targets? fixed-in version)
  (and (non-empty-string? fixed-in)
       (not (string=? fixed-in "—"))
       (string=? (strip-v (string-trim fixed-in)) (strip-v version))))

;; INDEX.md table row -> (list bug-id fixed-in report-file).
;; Row shape: | ID | reported | title | component | severity | status | fixed-in | [file](link) |
;; Backslash-free patterns only (transport-safe): char classes instead of escapes.
;; pregexp (not regexp): brace quantifiers {n}/{n,m} are literal braces under plain regexp.
(define registry-row-rx
  (pregexp "^[|] *(BUG-[0-9]{4}) *[|](?:[^|]*[|]){5} *([^|]*?) *[|]"))
(define file-link-rx (regexp "[]][(]([^)]+)"))

(define (parse-registry-rows index-path)
  (for/list ([line (in-list (file->lines index-path))]
             #:when (regexp-match registry-row-rx line))
    (define m (regexp-match registry-row-rx line))
    (define link (regexp-match file-link-rx line))
    (list (cadr m) (caddr m) (and link (cadr link)))))

;; `Landing commit:` line in a bug report holds the SHA of the squashed fix
;; that landed on main. Convention introduced with BUG-0008.
(define landing-commit-rx
  (pregexp (string-append "(?i:landing[ ]+commit)[^:]*:[ ]*["
                         (list->string (list #\` #\" #\'))
                         "]*([0-9a-f]{7,40})")))

(define (landing-sha-from-report registry-dir report-file)
  (cond
    [(not report-file) #f]
    [else
     (define p (build-path registry-dir (string->path report-file)))
     (and (file-exists? p)
          (cond [(regexp-match landing-commit-rx (file->string p)) => cadr]
                [else #f]))]))

(define (registry-index-path)
  (define candidates
    (append
     (cond [(getenv "Q_BUG_REGISTRY") => (lambda (p) (list (string->path p)))] [else '()])
     (list (build-path repo-root ".planning" "bugs" "INDEX.md")
           (build-path repo-root 'up ".planning" "bugs" "INDEX.md"))))
  (for/first ([p (in-list candidates)] #:when (file-exists? p)) p))

;; --- GitHub helpers (credential pattern shared with q/scripts/milestone-gate.rkt)

(define (gh-token)
  (or (getenv "GITHUB_TOKEN")
      (getenv "GH_TOKEN")
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (define p (build-path (find-system-path 'home-dir) "GH_PAT"))
        (and (file-exists? p) (string-trim (file->string p))))))

(define origin-url-rx (regexp "github[.]com[/:]([^/]+)/([^/]+?)(?:[.]git)?$"))

(define (repo-slug)
  (or (for/or ([url (in-list (git-lines "remote" "get-url" "origin"))])
        (cond [(regexp-match origin-url-rx url) => (lambda (m) (format "~a/~a" (cadr m) (caddr m)))]
              [else #f]))
      "coinerd/q"))

;; -> jsexpr or #f (no credentials, HTTP error, or bad JSON)
(define (gh-api-json path)
  (define token (gh-token))
  (and token
       (let ()
         (define out (open-output-string))
         (define code
           (parameterize ([current-output-port out])
             (system*/exit-code (find-executable-path "curl")
                                "-s" "-f" "--max-time" "30"
                                "-H" (format "Authorization: token ~a" token)
                                "-H" "Accept: application/vnd.github+json"
                                (format "https://api.github.com/repos/~a/~a" (repo-slug) path))))
         (and (zero? code)
              (with-handlers ([exn:fail:read? (lambda (_) #f)])
                (string->jsexpr (get-output-string out)))))))

(define bug-id-rx (pregexp "BUG-[0-9]{4}"))
(define bug-title-rx (pregexp "BUG-[0-9]{4}[: ]"))

;; BUG-NNNN issues on the v<version> milestone.
;; -> (values (list (list "BUG-NNNN" issue-number) ...) status)
;; status in {milestone, no-such-milestone, query-failed, no-credentials}
(define (milestone-required-fixes version)
  (cond
    [(not (gh-token)) (values '() 'no-credentials)]
    [else
     (define jml (gh-api-json "milestones?state=all&per_page=100"))
     (cond
       [(not (list? jml)) (values '() 'query-failed)]
       [else
        (define target (string-append "v" (strip-v version)))
        (define title-rx (regexp (string-append "^" (regexp-quote target) "($|[^0-9])")))
        (define found
          (for/first ([m (in-list jml)]
                      #:when (regexp-match? title-rx (hash-ref m 'title "")))
            m))
        (cond
          [(not found) (values '() 'no-such-milestone)]
          [else
           (define issues
             (gh-api-json (format "issues?milestone=~a&state=all&per_page=100"
                                  (hash-ref found 'number 0))))
           (cond
             [(not (list? issues)) (values '() 'query-failed)]
             [else
              (define rows
                (for/list ([i (in-list issues)]
                           #:when (regexp-match bug-title-rx (hash-ref i 'title "")))
                  (list (car (regexp-match bug-id-rx (hash-ref i 'title "")))
                        (hash-ref i 'number 0))))
              (values rows 'milestone)])])])]))

;; Landing SHA for a tracker issue: prefer a *merged* cross-referenced PR whose
;; title mentions the BUG-ID; otherwise the most recently merged one.
(define (issue-landing-sha bug-id issue-number)
  (define timeline
    (gh-api-json (format "issues/~a/timeline?per_page=100" issue-number)))
  (and (list? timeline)
       (let ()
         (define cross-refs '())
         (for ([e (in-list timeline)]
               #:when (equal? (hash-ref e 'event #f) "cross-referenced"))
           (define src (hash-ref e 'source (hasheq)))
           (define iss (hash-ref src 'issue (hasheq)))
           (when (hash-has-key? iss 'pull_request)
             (set! cross-refs (cons iss cross-refs))))
         (define merged-prs
           (filter values
                   (for/list ([iss (in-list (reverse cross-refs))])
                     (define p (gh-api-json (format "pulls/~a" (hash-ref iss 'number 0))))
                     (and (hash? p) (hash-ref p 'merged #f) p))))
         (cond
           [(null? merged-prs) #f]
           [else
            (define chosen
              (or (for/first ([p (in-list merged-prs)]
                              #:when (regexp-match? (regexp-quote bug-id) (hash-ref p 'title "")))
                    p)
                  (car (sort merged-prs string>=?
                             #:key (lambda (p) (hash-ref p 'merged_at ""))))))
            (hash-ref chosen 'merge_commit_sha #f)]))))

;; --- containment -----------------------------------------------------------

(define (tag-commit-sha tag)
  (define lines (git-lines "rev-list" "-1" tag))
  (and (pair? lines) (car lines)))

(define (commit-in-clone? sha)
  (zero? (car (git-quiet "cat-file" "-e" (string-append sha "^{commit}")))))

(define (commit-ancestor? sha of-commit)
  (zero? (car (git-quiet "merge-base" "--is-ancestor" sha of-commit))))

(define (short-sha sha)
  (if (and (string? sha) (>= (string-length sha) 8)) (substring sha 0 8) (or sha "?")))

(define (check-readiness! tag)
  (define version (tag->version tag))
  (unless version
    (stage-fail "release-readiness" (format "cannot derive a version from tag ~a" tag)))
  (define tagged (or (tag-commit-sha tag) "unknown"))
  ;; required fixes: registry rows whose Fixed-in targets this release version
  (define index-path (registry-index-path))
  (define registry-rows
    (if index-path
        (for/list ([row (in-list (parse-registry-rows index-path))]
                   #:when (release-targets? (cadr row) version))
          row)
        '()))
  (define registry-dir (and index-path (path-only index-path)))
  ;; cross-check with the GitHub milestone (CI-side source)
  (define-values (ms-entries ms-status) (milestone-required-fixes version))
  ;; union of both sources; registry-recorded SHA wins over tracker resolution
  (define required (make-hash)) ; bug-id -> (list issue-number landing-sha)
  (for ([row (in-list registry-rows)])
    (hash-set! required (car row)
               (list #f (landing-sha-from-report registry-dir (caddr row)))))
  (for ([e (in-list ms-entries)])
    (match-define (list bug-id issue-num) e)
    (define prev (hash-ref required bug-id (list #f #f)))
    (hash-set! required bug-id
               (list (or (car prev) issue-num)
                     (or (cadr prev) (issue-landing-sha bug-id issue-num)))))
  (printf "  --    [readiness] sources: registry ~a[~a row(s) target v~a] · milestone ~a\n"
          (if index-path (path->string index-path) "absent")
          (length registry-rows)
          version
          (match ms-status
            ['milestone (format "v~a: ~a issue(s)" version (length ms-entries))]
            ['no-such-milestone (format "v~a: none exists" version)]
            ['query-failed "UNAVAILABLE (query failed)"]
            ['no-credentials "UNAVAILABLE (no credentials)"]))
  (cond
    [(hash-empty? required)
     (cond
       [(eq? ms-status 'query-failed)
        (stage-fail "release-readiness"
                    "cannot prove readiness: no usable registry found AND the milestone query failed (need $GITHUB_TOKEN / $GH_TOKEN / ~/GH_PAT, or a reachable network)")]
       [else
        (stage-ok "release-readiness: no fixes recorded as required for v~a (registry + milestone agree there is nothing to gate)" version)])]
    [else
     (define results
       (for/list ([bug-id (in-list (sort (hash-keys required) string<?))])
         (match-define (list issue-num sha) (hash-ref required bug-id))
         (define ref (if issue-num (format "~a (#~a)" bug-id issue-num) bug-id))
         (cons bug-id
               (cond
                 [(not sha)
                  (list 'fail
                        (format "~a: no landing commit recorded — merge the fix PR and record its SHA on a `Landing commit:` line in the registry report (.planning/bugs/README.md)" ref))]
                 [(not (commit-in-clone? sha))
                  (list 'fail
                        (format "~a: landing commit ~a is not present in this clone — fetch it; commit ~a would satisfy the gate" ref (short-sha sha) (short-sha sha)))]
                 [(commit-ancestor? sha tagged)
                  (list 'ok
                        (format "~a: fix landed at ~a — contained in tagged ~a" ref (short-sha sha) (short-sha tagged)))]
                 [else
                  (list 'fail
                        (format "~a: required fix landed at ~a but tag ~a points at ~a, which does NOT contain it — re-tag on a commit containing ~a, e.g. on current main: git tag -fa ~a main -m \"~a\" && git push origin ~a --force"
                                ref (short-sha sha) tag (short-sha tagged) (short-sha sha) tag tag tag))]))))
     (for ([r (in-list results)])
       (if (eq? (cadr r) 'ok)
           (printf "  ok    [readiness] ~a\n" (caddr r))
           (eprintf "  FAIL  [readiness] ~a\n" (caddr r))))
     (define failed (filter (lambda (r) (not (eq? (cadr r) 'ok))) results))
     (cond
       [(pair? failed)
        (eprintf "\nrelease preflight: FAILED release-readiness — ~a required fix(es) missing from ~a:\n" (length failed) tag)
        (for ([r (in-list failed)]) (eprintf "  - ~a\n" (car r)))
        (exit 1)]
       [else
        (stage-ok "release-readiness: all ~a required fix(es) for v~a are contained in ~a" (length results) version tag)])]))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (preflight! tag)
  (printf "release preflight: ~a\n" tag)
  (check-tag-exists! tag)
  (check-tag-object-type! tag)
  (check-tag-version-consistency! tag)
  (check-manifest-dry-run! tag)
  (printf "release preflight: all invariants hold for ~a\n" tag))

(define (preflight-with-readiness! tag)
  (printf "release preflight: ~a\n" tag)
  (check-tag-exists! tag)
  (check-tag-object-type! tag)
  (check-tag-version-consistency! tag)
  (check-manifest-dry-run! tag)
  (check-readiness! tag)
  (printf "release preflight: all invariants hold for ~a\n" tag))

(module+ main
  (match (current-command-line-arguments)
    [(vector tag "--readiness") (preflight-with-readiness! tag)]
    [(vector "--readiness" tag) (preflight-with-readiness! tag)]
    [(vector tag) (preflight! tag)]
    [_ (usage!)]))

(module+ test
  (require rackunit)
  (test-case "tag->version parses plain and pre-release tags"
    (check-equal? (tag->version "v1.00.00") "1.00.00")
    (check-equal? (tag->version "1.00.00-PRE1") "1.00.00-PRE1")
    (check-false (tag->version "not-a-tag")))
  (test-case "read-info-version finds a version definition"
    (check-match (read-info-version) (? string? _))))
