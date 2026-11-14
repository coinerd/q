#lang racket

;; @speed fast
;; @suite testing
;; @covers scripts/github/project-setup.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-write.rkt
;;
;; Native port of the deleted scripts/setup-github-project.py
;; (scripts/github/project-setup.rkt) on top of the q delivery seams.
;;
;; Evidence law: a fake `gh` executable is installed FIRST on PATH. It logs
;; every argument vector it received (one argv element per line, each invocation
;; introduced by a --CALL-- marker) and then prints the fixture registered for
;; the request it parsed out of that vector. Every assertion is therefore made
;; from the child's own evidence: the exact argv (the GraphQL query text stays
;; inside ONE argument), the sorted `-f` variable order, the spawn count (which
;; proves create-only-when-absent, add-only-when-absent and no retry of a failed
;; mutation), and the typed refusal text.
;;
;; What the cases prove:
;;   (a) exact argv for the project read, each field creation, the paginated
;;       issue listing, the paginated item read and each addProjectV2ItemById;
;;   (b) idempotence: a complete board performs reads only, a partial board
;;       creates exactly the missing fields and adds exactly the missing issues,
;;       and a dry run mutates nothing;
;;   (c) typed refusals: repository-identity-mismatch (before any gh call), the
;;       non-GitHub origin refusal, project-not-found, status-field-missing,
;;       field-options-missing (Status is verified, never rewritten),
;;       field-data-type-mismatch, malformed issue/page/item shapes and
;;       project-items-pagination-incomplete;
;;   (d) no retry on a mutation: a failing add-item POST is attempted exactly
;;       once and the next issue is never attempted;
;;   (e) credential isolation: a shim that echoes GITHUB_TOKEN to stderr never
;;       leaks the token into the typed refusal;
;;   (f) the CLI: delivery-pending JSON with exit 2 on a refusal, a dry-run
;;       report with exit 0, and no gh call at all for a malformed argument.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         "../scripts/github/project-setup.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "helpers/w2-mini-git-repo.rkt")

(define slug "coinerd/q")
(define owner "coinerd")
(define project-number 1)
(define project-node-id "PVT_proj")
(define status-names (list "Inbox" "Ready" "In progress" "Blocked" "In review" "Done"))
(define area-names
  (list "Core"
        "Provider"
        "TUI"
        "CLI"
        "RPC/SDK"
        "Packages"
        "Extensions"
        "Docs"
        "Security"
        "CI"
        "Benchmarks"))

;; ============================================================
;; Fixture builders (the JSON the fake gh serves)
;; ============================================================

(define (option-json names)
  (for/list ([name (in-list names)])
    (hasheq 'id (string-append "option-" name) 'name name)))

(define (field-json name data-type [names '()])
  (hasheq 'id
          (string-append "field-" name)
          'name
          name
          'dataType
          data-type
          'options
          (option-json names)))

(define (status-field)
  (field-json "Status" "SINGLE_SELECT" status-names))

(define (complete-fields)
  (list (status-field)
        (field-json "Priority" "SINGLE_SELECT" (list "P0" "P1" "P2" "P3"))
        (field-json "Area" "SINGLE_SELECT" area-names)
        (field-json "Effort" "SINGLE_SELECT" (list "XS" "S" "M" "L" "XL"))
        (field-json "Risk" "SINGLE_SELECT" (list "Low" "Medium" "High"))
        (field-json "OwnerNote" "TEXT")))

(define (project-response fields #:item-count [item-count 1])
  (hasheq
   'data
   (hasheq
    'user
    (hasheq
     'projectV2
     (hasheq 'id
             project-node-id
             'number
             project-number
             'title
             "q Roadmap & Delivery"
             'url
             "https://github.com/users/coinerd/projects/1"
             'fields
             (hasheq 'nodes fields)
             'views
             (hasheq 'nodes
                     (list (hasheq 'id "view-1" 'name "Triage" 'number 1 'layout "TABLE_LAYOUT")))
             'items
             (hasheq 'totalCount item-count))))))

(define (missing-project-response)
  (hasheq 'data (hasheq 'user (hasheq 'projectV2 'null))))

(define (issue-json number)
  (hasheq 'number number 'node_id (format "I_~a" number) 'title (format "issue ~a" number)))

(define (pull-request-json number)
  (hash-set (issue-json number) 'pull_request (hasheq 'url "https://example.invalid/pr")))

(define (issues-pages page)
  (list page))

(define (item-node number)
  (hasheq 'id (format "PVTI_~a" number) 'content (hasheq 'number number)))

(define (items-response nodes has-next? end-cursor)
  (hasheq
   'data
   (hasheq
    'node
    (hasheq 'items
            (hasheq 'pageInfo (hasheq 'hasNextPage has-next? 'endCursor end-cursor) 'nodes nodes)))))

(define created-field-response
  (hasheq 'data
          (hasheq 'createProjectV2Field
                  (hasheq 'projectV2Field
                          (hasheq 'id "field-created" 'name "created" 'dataType "SINGLE_SELECT")))))

(define added-item-response
  (hasheq 'data (hasheq 'addProjectV2ItemById (hasheq 'item (hasheq 'id "PVTI_added")))))

;; ============================================================
;; The fake gh world
;; ============================================================

(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (put path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; One argv element per line, each invocation introduced by --CALL--, then the
;; fixture dispatch. `GH_SHIM_FAIL=add` makes ONLY the addProjectV2ItemById POST
;; fail, echoing GITHUB_TOKEN to stderr the way a real gh auth failure might.
(define shim-script
  (string-append "#!/bin/sh\n"
                 "printf '%s\\n' '--CALL--' >> \"$GH_SHIM_LOG\"\n"
                 "for argument in \"$@\"; do\n"
                 ;; Newlines inside ONE argv element (the GraphQL text) are recorded as
                 ;; RECORD-SEPARATOR (0x1e) so the log stays one argv element per line.
                 "  printf '%s' \"$argument\" | tr '\\n' '\\036' >> \"$GH_SHIM_LOG\"\n"
                 "  printf '\\n' >> \"$GH_SHIM_LOG\"\n"
                 "done\n"
                 "route=\nquery=\ncursor=\n"
                 "for argument in \"$@\"; do\n"
                 "  case \"$argument\" in\n"
                 "    repos/*) route=$argument ;;\n"
                 "    query=*) query=$argument ;;\n"
                 "    cursor=*) cursor=$argument ;;\n"
                 "  esac\n"
                 "done\n"
                 "if [ \"$GH_SHIM_FAIL\" = \"add\" ]; then\n"
                 "  case \"$query\" in\n"
                 "    *addProjectV2ItemById*)\n"
                 "      printf '%s\\n' \"GITHUB_TOKEN=$GITHUB_TOKEN\" >&2\n"
                 "      exit 1 ;;\n"
                 "  esac\n"
                 "fi\n"
                 "if [ -n \"$route\" ]; then\n"
                 "  /bin/cat \"$GH_SHIM_FIXTURES/issues.json\"\n"
                 "  exit 0\n"
                 "fi\n"
                 "case \"$query\" in\n"
                 "  *pageInfo*)\n"
                 "    if [ -n \"$cursor\" ]; then\n"
                 "      /bin/cat \"$GH_SHIM_FIXTURES/items-2.json\"\n"
                 "    else\n"
                 "      /bin/cat \"$GH_SHIM_FIXTURES/items-1.json\"\n"
                 "    fi ;;\n"
                 "  *createProjectV2Field*) /bin/cat \"$GH_SHIM_FIXTURES/created-field.json\" ;;\n"
                 "  *addProjectV2ItemById*) /bin/cat \"$GH_SHIM_FIXTURES/added-item.json\" ;;\n"
                 "  *projectV2*) /bin/cat \"$GH_SHIM_FIXTURES/project.json\" ;;\n"
                 "  *) printf 'no fixture for %s\\n' \"$query\" >&2; exit 66 ;;\n"
                 "esac\n"))

;; fixtures: list of (filename . jsexpr). proc receives (repo root), where root
;; holds gh.log and fixtures/.
(define (with-gh-world fixtures
                       proc
                       #:origin [origin "git@github.com:coinerd/q.git"]
                       #:fail [fail ""])
  (define root (make-temporary-file "q-project-setup-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define repo (build-path root "repo"))
                  (define bin (build-path root "bin"))
                  (define fixtures-dir (build-path root "fixtures"))
                  (make-directory* repo)
                  (make-directory* bin)
                  (make-directory* fixtures-dir)
                  (mini-git! repo "init" "-b" "main" ".")
                  (mini-git! repo "remote" "add" "origin" origin)
                  (for ([fixture (in-list fixtures)])
                    (put (build-path fixtures-dir (car fixture)) (jsexpr->string (cdr fixture))))
                  (define shim (build-path bin "gh"))
                  (put shim shim-script)
                  (file-or-directory-permissions shim #o700)
                  (define env (make-environment-variables))
                  (for ([pair (in-list `(("PATH" . ,(format "~a:/usr/bin:/bin" bin))
                                         ("HOME" . ,(path->string root))
                                         ("GIT_CONFIG_NOSYSTEM" . "1")
                                         ("GIT_TERMINAL_PROMPT" . "0")
                                         ("GH_SHIM_LOG" . ,(path->string (build-path root "gh.log")))
                                         ("GH_SHIM_FIXTURES" . ,(path->string fixtures-dir))
                                         ("GH_SHIM_FAIL" . ,fail)
                                         ("GITHUB_TOKEN" . "ghp_SENTINEL_SHOULD_NEVER_LEAK")))])
                    (environment-set! env (car pair) (cdr pair)))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "gh")
                                     "the fixture must resolve gh through PATH")
                    (proc repo root)))
                (lambda () (delete-directory/files root))))

;; Parse the argv log into a list of argument vectors, in call order. Each line
;; is ONE argv element; the RECORD-SEPARATOR encoding restores embedded
;; newlines inside the GraphQL text.
(define record-separator (string (integer->char 30)))

(define (decode-argument line)
  (string-replace line record-separator "\n"))

(define (read-calls root)
  (define log (build-path root "gh.log"))
  (define (finish current calls)
    (if (null? current)
        calls
        (cons (reverse (map decode-argument current)) calls)))
  (cond
    [(not (file-exists? log)) '()]
    [else
     (let loop ([lines (file->lines log)]
                [current '()]
                [calls '()])
       (cond
         [(null? lines) (reverse (finish current calls))]
         [(string=? (car lines) "--CALL--") (loop (cdr lines) '() (finish current calls))]
         [else (loop (cdr lines) (cons (car lines) current) calls)]))]))

;; The typed refusal message, or a failure if the call did not refuse.
(define (refusal-message thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed delivery refusal, but the call returned")))

(define (setup! repo #:expected-slug [expected-slug slug] #:dry-run? [dry-run? #f])
  (delivery-project-setup! repo
                           #:owner owner
                           #:project-number project-number
                           #:expected-slug expected-slug
                           #:dry-run? dry-run?))

;; The GraphQL argv every request shares: query text in ONE argument, then the
;; caller's variables in sorted order.
(define (graphql-call text variables)
  (append
   (list "api" "--hostname" "github.com" "graphql" "-X" "POST" "-f" (string-append "query=" text))
   (append* (for/list ([entry (in-list variables)])
              (list "-f" (format "~a=~a" (car entry) (cdr entry)))))))

(define (contains-all? text needles)
  (for/and ([needle (in-list needles)])
    (string-contains? text needle)))

(define (any-call-with? calls needle)
  (for/or ([call (in-list calls)])
    (string-contains? (string-append* call) needle)))

;; ============================================================
;; Scenario bodies (flat, so the test-case forms stay one line each)
;; ============================================================

(define (happy-path! repo root)
  (define report (setup! repo))
  (define calls (read-calls root))
  (check-equal? (hash-ref report 'status) "ready")
  (check-equal? (hash-ref report 'repository) slug)
  (check-equal? (hash-ref report 'createdFields) (list "Priority" "Area" "Effort" "Risk" "OwnerNote"))
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'added) (list 12 13))
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'alreadyOnBoard) 1)
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'openIssues) 3)
  ;; 1 read + 5 creations + 1 issue list + 1 item page + 2 adds.
  (check-equal? (length calls) 10)
  ;; The project read: caller variables sorted by name (number < owner).
  (check-equal? (list-ref calls 0)
                (graphql-call project-query
                              (list (cons "number" (number->string project-number))
                                    (cons "owner" owner))))
  ;; Priority carries the documented option table inline, in table order. The
  ;; option text is built here from literal rows (never from the module's own
  ;; table), so a table drift cannot agree with itself.
  (define (option-text name description color)
    (format "{name: ~s, description: ~s, color: ~a}" name description color))
  (define priority-options
    (string-join (list (option-text "P0" "Critical ship immediately" "RED")
                       (option-text "P1" "High ship this release" "ORANGE")
                       (option-text "P2" "Medium ship soon" "YELLOW")
                       (option-text "P3" "Low nice to have" "GRAY"))
                 ", "))
  (define priority-text
    (string-append "mutation($project: ID!, $name: String!) {\n"
                   "  createProjectV2Field(input: {projectId: $project, name: $name, "
                   "dataType: SINGLE_SELECT, singleSelectOptions: ["
                   priority-options
                   "]}) {\n"
                   "    projectV2Field { ... on ProjectV2SingleSelectField { "
                   "id name dataType options { id name } } }\n"
                   "  }\n"
                   "}\n"))
  (check-equal? (list-ref calls 1)
                (graphql-call priority-text
                              (list (cons "name" "Priority") (cons "project" project-node-id))))
  ;; Each remaining select field's own option table reaches its mutation.
  (check-true (string-contains?
               (string-append* (list-ref calls 2))
               "{name: \"Core\", description: \"Agent core loop types event bus\", color: PURPLE}"))
  (check-true (string-contains? (string-append* (list-ref calls 3))
                                "{name: \"XS\", description: \"Under 1 hour\", color: GREEN}"))
  (check-true (string-contains?
               (string-append* (list-ref calls 4))
               "{name: \"High\", description: \"Architectural or breaking change\", color: RED}"))
  ;; OwnerNote is the static TEXT mutation.
  (check-equal?
   (list-ref calls 5)
   (graphql-call
    (string-append
     "mutation($project: ID!, $name: String!) {\n"
     "  createProjectV2Field(input: {projectId: $project, name: $name, dataType: TEXT}) {\n"
     "    projectV2Field { ... on ProjectV2Field { id name dataType } }\n"
     "  }\n"
     "}\n")
    (list (cons "name" "OwnerNote") (cons "project" project-node-id))))
  ;; The Python REPO issues route, paginated and slurped by the api session.
  (check-equal? (list-ref calls 6)
                (list "api"
                      "--hostname"
                      "github.com"
                      (format "repos/~a/~a" slug issues-route)
                      "--paginate"
                      "--slurp"))
  ;; First item page: no cursor variable at all.
  (check-equal? (list-ref calls 7)
                (graphql-call project-items-query
                              (list (cons "first" "100") (cons "project" project-node-id))))
  ;; Only the missing issues are added, ascending, one POST each.
  (check-equal? (list-ref calls 8)
                (graphql-call add-item-mutation
                              (list (cons "content" "I_12") (cons "project" project-node-id))))
  (check-equal? (list-ref calls 9)
                (graphql-call add-item-mutation
                              (list (cons "content" "I_13") (cons "project" project-node-id)))))

(define (complete-board! repo root)
  (define report (setup! repo))
  (define calls (read-calls root))
  (check-equal? (hash-ref report 'createdFields) '())
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'added) '())
  (check-equal? (hash-ref report 'fields)
                (list "Status" "Priority" "Area" "Effort" "Risk" "OwnerNote"))
  (check-equal? (hash-ref report 'views) (list "Triage"))
  ;; Read, issue list, item page — and no mutation at all.
  (check-equal? (length calls) 3))

(define (dry-run! repo root)
  (define report (setup! repo #:dry-run? #t))
  (define calls (read-calls root))
  (check-equal? (hash-ref report 'status) "dry-run")
  (check-equal? (hash-ref report 'createdFields) (list "Priority" "Area" "Effort" "Risk" "OwnerNote"))
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'added) (list 12 13))
  (check-equal? (length calls) 3))

(define (two-item-pages! repo root)
  (define report (setup! repo))
  (define calls (read-calls root))
  (check-equal? (hash-ref (hash-ref report 'itemSync) 'added) '())
  (check-equal? (length calls) 4)
  (check-equal? (list-ref calls 3)
                (graphql-call project-items-query
                              (list (cons "cursor" "CURSOR_1")
                                    (cons "first" "100")
                                    (cons "project" project-node-id)))))

(define (board-tables!)
  (check-equal? (status-option-names) status-names)
  (check-equal? (map car select-field-specs) (list "Priority" "Area" "Effort" "Risk"))
  (check-equal? (map car (cadr (assoc "Area" select-field-specs))) area-names)
  (check-equal? text-field-names (list "OwnerNote"))
  (check-equal? (assoc "P0" (cadr (assoc "Priority" select-field-specs)))
                (list "P0" "Critical ship immediately" "RED")))

(define (identity-mismatch! repo root)
  (define message (refusal-message (lambda () (setup! repo))))
  (check-true (string-prefix? message "repository-identity-mismatch:"))
  (check-true (string-contains? message "other/repo"))
  (check-true (string-contains? message slug))
  ;; Nothing was read from or written to the API.
  (check-equal? (read-calls root) '()))

(define (non-github-origin! repo root)
  (check-equal? (refusal-message (lambda () (setup! repo #:expected-slug #f)))
                "origin must identify a GitHub repository (no arbitrary API host)")
  (check-equal? (read-calls root) '()))

(define (missing-project! repo root)
  (define message (refusal-message (lambda () (setup! repo))))
  (check-true (string-prefix? message "project-not-found:"))
  (check-true (string-contains? message "Projects UI"))
  (check-equal? (length (read-calls root)) 1))

(define (incomplete-status! repo root)
  ;; A Status field missing documented options refuses; the option set is never
  ;; rewritten in place (that rewrites option ids and detaches stored values).
  (define message (refusal-message (lambda () (setup! repo))))
  (check-true (string-prefix? message "field-options-missing:"))
  (check-true (string-contains? message "In progress"))
  (define calls (read-calls root))
  (check-equal? (length calls) 1)
  (check-false (any-call-with? calls "updateProjectV2Field")))

(define (missing-status! repo root)
  (check-true (string-prefix? (refusal-message (lambda () (setup! repo))) "status-field-missing:"))
  (check-equal? (length (read-calls root)) 1))

(define (wrong-field-type! repo root)
  (define message (refusal-message (lambda () (setup! repo))))
  (check-true (string-prefix? message "field-data-type-mismatch:"))
  (check-true (string-contains? message "Priority"))
  (check-equal? (length (read-calls root)) 1))

(define (malformed-issue-page! repo root)
  (check-true (string-prefix? (refusal-message (lambda () (setup! repo))) "malformed-issue-page:")))

(define (malformed-issue-entry! repo root)
  (define message (refusal-message (lambda () (setup! repo))))
  (check-true (string-prefix? message "malformed-issue-entry:"))
  (check-true (string-contains? message "node_id")))

(define (incomplete-item-page! repo root)
  (check-true (string-prefix? (refusal-message (lambda () (setup! repo)))
                              "project-items-pagination-incomplete:"))
  (check-equal? (length (read-calls root)) 3))

(define (add-fails! repo root)
  (define message (refusal-message (lambda () (setup! repo))))
  ;; The seam never echoes raw gh stderr (credential isolation).
  (check-false (string-contains? message "ghp_SENTINEL_SHOULD_NEVER_LEAK") message)
  (check-true (string-prefix? message "GitHub authentication failed"))
  (define calls (read-calls root))
  ;; Read, issue list, item page, then ONE add attempt for 12: the failed
  ;; mutation is not retried and 13 is never attempted.
  (check-equal? (length calls) 4)
  (check-true (string-contains? (string-append* (last calls)) "addProjectV2ItemById"))
  (check-true (any-call-with? calls "content=I_12"))
  (check-false (any-call-with? calls "content=I_13")))

(define (cli-refusal! repo root)
  (define output
    (with-output-to-string (lambda ()
                             (check-equal? (project-setup-main (list "--repo"
                                                                     (path->string repo)
                                                                     "--owner"
                                                                     owner
                                                                     "--project-number"
                                                                     "1"
                                                                     "--expected-slug"
                                                                     slug)
                                                               #:exit (lambda (code) code))
                                           2))))
  (define payload (string->jsexpr output))
  (check-equal? (hash-ref payload 'status) "delivery-pending")
  (check-true (string-prefix? (hash-ref payload 'reason) "project-not-found:")))

(define (cli-dry-run! repo root)
  (define output
    (with-output-to-string
     (lambda ()
       (check-equal?
        (project-setup-main
         (list "--repo" (path->string repo) "--owner" owner "--project-number" "1" "--dry-run")
         #:exit (lambda (code) code))
        0))))
  (define payload (string->jsexpr output))
  (check-equal? (hash-ref payload 'status) "dry-run")
  (check-equal? (hash-ref payload 'repository) slug)
  (check-equal? (hash-ref payload 'createdFields) '()))

(define (cli-malformed-number! repo root)
  (define output
    (with-output-to-string
     (lambda ()
       (check-equal? (project-setup-main
                      (list "--repo" (path->string repo) "--owner" owner "--project-number" "one")
                      #:exit (lambda (code) code))
                     2))))
  (define payload (string->jsexpr output))
  (check-equal? (hash-ref payload 'status) "delivery-pending")
  (check-true (string-prefix? (hash-ref payload 'reason) "malformed project number:"))
  (check-equal? (read-calls root) '()))

(define (graphql-escaping!)
  (check-equal? (graphql-string "plain") "\"plain\"")
  (check-equal? (graphql-string "a\"b") "\"a\\\"b\"")
  (check-equal? (graphql-string "a\\b") "\"a\\\\b\"")
  (check-equal? (graphql-string "a\nb") "\"a\\nb\""))

;; ============================================================
;; The fixture worlds
;; ============================================================

(define happy-path-fixtures
  (list
   (cons "project.json" (project-response (list (status-field))))
   (cons "issues.json"
         (issues-pages (list (issue-json 11) (issue-json 12) (issue-json 13) (pull-request-json 14))))
   (cons "items-1.json" (items-response (list (item-node 11)) #f 'null))
   (cons "created-field.json" created-field-response)
   (cons "added-item.json" added-item-response)))

(define complete-board-fixtures
  (list (cons "project.json" (project-response (complete-fields)))
        (cons "issues.json" (issues-pages (list (issue-json 11) (pull-request-json 12))))
        (cons "items-1.json" (items-response (list (item-node 11)) #f 'null))
        (cons "created-field.json" created-field-response)
        (cons "added-item.json" added-item-response)))

(define two-item-pages-fixtures
  (list (cons "project.json" (project-response (complete-fields)))
        (cons "issues.json" (issues-pages (list (issue-json 11) (issue-json 12) (issue-json 13))))
        (cons "items-1.json" (items-response (list (item-node 11)) #t "CURSOR_1"))
        (cons "items-2.json" (items-response (list (item-node 12) (item-node 13)) #f 'null))
        (cons "created-field.json" created-field-response)
        (cons "added-item.json" added-item-response)))

(define no-items-fixtures
  (list (cons "project.json" (project-response (complete-fields)))
        (cons "issues.json" (issues-pages (list (issue-json 12) (issue-json 13))))
        (cons "items-1.json" (items-response '() #f 'null))
        (cons "created-field.json" created-field-response)
        (cons "added-item.json" added-item-response)))

;; ============================================================
;; Cases
;; ============================================================

(module+ test
  (test-case "field creation and item sync issue the exact argv, once each"
    (with-gh-world happy-path-fixtures happy-path!))

  (test-case "a complete board performs reads only"
    (with-gh-world complete-board-fixtures complete-board!))

  (test-case "a dry run reports the plan and mutates nothing"
    (with-gh-world (list (cons "project.json" (project-response (list (status-field))))
                         (cons "issues.json" (issues-pages (list (issue-json 12) (issue-json 13))))
                         (cons "items-1.json" (items-response '() #f 'null))
                         (cons "created-field.json" created-field-response))
                   dry-run!))

  (test-case "project items paginate across two pages without duplicating adds"
    (with-gh-world two-item-pages-fixtures two-item-pages!))

  (test-case "the documented board tables are preserved"
    (board-tables!)))

(module+ test
  (test-case "repository identity is checked before any gh call"
    (with-gh-world complete-board-fixtures
                   identity-mismatch!
                   #:origin "https://github.com/other/repo.git"))

  (test-case "a non-GitHub origin is refused by the shared repository helper"
    (with-gh-world complete-board-fixtures
                   non-github-origin!
                   #:origin "https://gitlab.com/coinerd/q.git"))

  (test-case "a missing project refuses instead of inventing one"
    (with-gh-world (list (cons "project.json" (missing-project-response))) missing-project!))

  (test-case "Status is verified, never rewritten"
    (with-gh-world (list (cons "project.json"
                               (project-response (list (field-json "Status"
                                                                   "SINGLE_SELECT"
                                                                   (list "Inbox" "Ready" "Done"))))))
                   incomplete-status!))

  (test-case "a missing Status field refuses"
    (with-gh-world
     (list (cons "project.json"
                 (project-response (list (field-json "Priority" "SINGLE_SELECT" (list "P0"))))))
     missing-status!))

  (test-case "an existing field of the wrong data type refuses"
    (with-gh-world
     (list (cons "project.json"
                 (project-response
                  (list (status-field)
                        (field-json "Priority" "TEXT")
                        (field-json "Area" "SINGLE_SELECT" area-names)
                        (field-json "Effort" "SINGLE_SELECT" (list "XS" "S" "M" "L" "XL"))
                        (field-json "Risk" "SINGLE_SELECT" (list "Low" "Medium" "High"))
                        (field-json "OwnerNote" "TEXT")))))
     wrong-field-type!))

  (test-case "a non-list issues response refuses"
    (with-gh-world (list (cons "project.json" (project-response (complete-fields)))
                         (cons "issues.json" (hasheq 'message "not a page list"))
                         (cons "items-1.json" (items-response '() #f 'null)))
                   malformed-issue-page!))

  (test-case "an issue without a node_id refuses"
    (with-gh-world (list (cons "project.json" (project-response (complete-fields)))
                         (cons "issues.json"
                               (issues-pages (list (hasheq 'number 12 'title "no node id"))))
                         (cons "items-1.json" (items-response '() #f 'null)))
                   malformed-issue-entry!))

  (test-case "an incomplete item page refuses instead of adding duplicates"
    (with-gh-world (list (cons "project.json" (project-response (complete-fields)))
                         (cons "issues.json" (issues-pages (list (issue-json 12))))
                         (cons "items-1.json" (items-response (list (item-node 11)) #t 'null))
                         (cons "added-item.json" added-item-response))
                   incomplete-item-page!)))

(module+ test
  (test-case "a failing add-item POST is attempted exactly once and never leaks the token"
    (with-gh-world no-items-fixtures add-fails! #:fail "add")))

(module+ test
  (test-case "the CLI reports a typed refusal as delivery-pending with exit 2"
    (with-gh-world (list (cons "project.json" (missing-project-response))) cli-refusal!))

  (test-case "the CLI runs a dry-run setup and prints the report"
    (with-gh-world complete-board-fixtures cli-dry-run!))

  (test-case "the CLI refuses a malformed project number before any gh call"
    (with-gh-world '() cli-malformed-number!))

  (test-case "graphql-string escapes quotes, backslashes and newlines"
    (graphql-escaping!)))
