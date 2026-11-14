#lang racket/base

;; project-setup.rkt — native (Racket) bootstrap of the "q Roadmap & Delivery"
;; GitHub Projects V2 board.
;;
;; This module replaces the deleted one-shot Python script
;; `scripts/setup-github-project.py`. It keeps that script's OPERATIONS and
;; makes them safe to run more than once:
;;
;;   1. repository identity — the origin remote must identify the expected
;;      `owner/name` slug before any API call (typed
;;      `repository-identity-mismatch`), so the board of the wrong repository is
;;      never mutated;
;;   2. project read — the project is resolved by owner login and project
;;      number; a missing project is the typed `project-not-found` refusal
;;      (see DEVIATION 1: creating a user-owned Project V2 is not an API
;;      operation);
;;   3. field creation — Priority / Area / Effort / Risk (SINGLE_SELECT with the
;;      documented option tables) and OwnerNote (TEXT) are created ONLY when
;;      absent; an existing field of the wrong data type refuses
;;      `field-data-type-mismatch`, and an existing field missing a required
;;      option refuses `field-options-missing` (never silently rewritten);
;;   4. board item sync — every OPEN issue that is not already a project item is
;;      added with `addProjectV2ItemById`, and pull-request entries from the
;;      issues endpoint are skipped exactly like the Python filter did.
;;
;; Seam law: every request goes through ONE q delivery session
;; (`make-delivery-pr-context`, scripts/gsd-delivery-pr.rkt) — one
;; (budget-seconds, start-ms) window for the whole run. GraphQL goes through the
;; write seam `gh-graphql` (scripts/gsd-delivery-write.rkt) and the REST issue
;; listing through the session's own memoized `api-call`, so both draw down the
;; SAME budget. Credential isolation, process-group cleanup and refusal typing
;; are the shared bounded runner's (`controller-environment`): raw gh stderr is
;; never echoed, so an error message can never carry credential material.
;;
;; NON-IDEMPOTENT-POST GUARD: a mutation is never retried. `gh-graphql`
;; performs exactly ONE bounded attempt and either returns the parsed object or
;; raises the seam's typed refusal. Re-running the whole setup is safe because
;; every mutation is preceded by a read that proves it is still needed
;; (create-only-when-absent, add-only-when-not-an-item) — never because a failed
;; POST is repeated.
;;
;; DEVIATIONS from the Python script (all fail closed, none widen a mutation):
;;   1. `createProjectV2` does not exist for user-owned projects in GitHub's
;;      public GraphQL schema, so the Python script never created the project
;;      either (its docstring says "Project already created"). Project creation
;;      stays a web-UI operation; a missing project is a typed refusal naming
;;      that, not a guess.
;;   2. The Python `updateProjectV2Field` on Status replaced the WHOLE option
;;      set from a hardcoded list on every run, which rewrites option IDs and
;;      can detach the values already stored on items. This module VERIFIES the
;;      required Status options and refuses `status-options-missing` instead of
;;      rewriting them; adding an option is a human decision in the Projects UI.
;;   3. `createProjectV2View` is not part of the public GraphQL schema (the
;;      Python call always failed and was swallowed as "✗ Failed"), so saved
;;      views are READ and reported, never invented here.
;;   4. The Python issue backfill read ONE page of 100 open issues; this module
;;      paginates with `--paginate --slurp` and refuses a malformed page instead
;;      of silently truncating the board.
;;   5. The Python script printed "✓/✗" progress and always exited 0. This
;;      module reports one JSON document, refuses with
;;      {"status":"delivery-pending","reason":...} on stdout and exits 2 — the
;;      shape delivery-coordinator.rkt's typed-stop parser already expects.
;;
;; Testability: `delivery-project-setup!` takes an injected context, so a test
;; installs a fake `gh` (exact argv, spawn counts) and asserts both the argv and
;; the refusals. The GraphQL texts are pure functions exported so a test can pin
;; them byte-for-byte.

(require json
         racket/list
         racket/match
         racket/string
         "../gsd-delivery-git.rkt"
         "../gsd-delivery-api.rkt"
         "../gsd-delivery-pr.rkt"
         "../gsd-delivery-write.rkt")

(provide delivery-project-setup!
         project-setup-main
         repository-identity
         read-project
         ensure-fields!
         sync-project-items!
         ;; the documented board tables (exported so a test pins them)
         status-field-name
         status-option-names
         select-field-specs
         text-field-names
         ;; pure GraphQL texts / argv builders (exported for exact-argv tests)
         project-query
         project-items-query
         create-single-select-mutation
         create-text-mutation
         add-item-mutation
         issues-route
         graphql-string
         items-page-size)

;; ============================================================
;; The board tables (verbatim from scripts/setup-github-project.py)
;; ============================================================

(define status-field-name "Status")

;; (name description color) — the Python `status_options` table. Kept for
;; VERIFICATION only (DEVIATION 2): the option set is never rewritten in place.
(define status-options
  '(("Inbox" "New untriaged work" "GRAY") ("Ready" "Triaged and ready to pick up" "BLUE")
                                          ("In progress" "Currently being worked on" "YELLOW")
                                          ("Blocked" "Waiting on something" "RED")
                                          ("In review" "Under review" "PURPLE")
                                          ("Done" "Completed" "GREEN")))

(define (status-option-names)
  (map car status-options))

;; (field-name option-specs) — the Python `select_fields` table, in its order.
(define select-field-specs
  (list (list "Priority"
              '(("P0" "Critical ship immediately" "RED") ("P1" "High ship this release" "ORANGE")
                                                         ("P2" "Medium ship soon" "YELLOW")
                                                         ("P3" "Low nice to have" "GRAY")))
        (list "Area"
              '(("Core" "Agent core loop types event bus" "PURPLE")
                ("Provider" "LLM provider adapters" "BLUE")
                ("TUI" "Terminal UI" "GREEN")
                ("CLI" "Command-line interface" "ORANGE")
                ("RPC/SDK" "RPC mode and SDK bindings" "BLUE")
                ("Packages" "Racket package distribution" "YELLOW")
                ("Extensions" "Extension platform" "PINK")
                ("Docs" "Documentation" "GRAY")
                ("Security" "Security hardening" "RED")
                ("CI" "Continuous integration" "BLUE")
                ("Benchmarks" "Performance benchmarks" "GREEN")))
        (list "Effort"
              '(("XS" "Under 1 hour" "GREEN") ("S" "1 to 3 hours" "BLUE")
                                              ("M" "Half a day" "YELLOW")
                                              ("L" "Full day or more" "ORANGE")
                                              ("XL" "Multi-day effort" "RED")))
        (list "Risk"
              '(("Low" "Straightforward change" "GREEN")
                ("Medium" "Some complexity or dependency" "YELLOW")
                ("High" "Architectural or breaking change" "RED")))))

;; The Python script's OwnerNote TEXT field.
(define text-field-names '("OwnerNote"))

;; GitHub's maximum page size; the item loop never truncates a list.
(define items-page-size 100)

;; ============================================================
;; Small helpers
;; ============================================================

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

;; Python `dig`: any non-hash along the path yields #f.
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

(define (sorted-field-arguments variables)
  (sort variables string<? #:key car))

;; GraphQL string literal escaping: a table edit can never break the query text.
(define (graphql-string text)
  (define escaped
    (for/list ([char (in-string (if (string? text)
                                    text
                                    (format "~a" text)))])
      (case char
        [(#\\) "\\\\"]
        [(#\") "\\\""]
        [(#\newline) "\\n"]
        [(#\return) "\\r"]
        [(#\tab) "\\t"]
        [else (string char)])))
  (string-append "\"" (string-append* escaped) "\""))

(define (option-input-text options)
  (string-join (for/list ([option (in-list options)])
                 (match-let ([(list name description color) option])
                   (format "{name: ~a, description: ~a, color: ~a}"
                           (graphql-string name)
                           (graphql-string description)
                           color)))
               ", "))

;; ============================================================
;; Pure GraphQL texts and REST route
;; ============================================================

;; data.user.projectV2 — the fields/views/items shape the Python verification
;; query read, plus `id`/`url` (the script hardcoded the project node id).
(define project-query
  (string-append
   "query($owner: String!, $number: Int!) {\n"
   "  user(login: $owner) {\n"
   "    projectV2(number: $number) {\n"
   "      id\n"
   "      number\n"
   "      title\n"
   "      url\n"
   "      fields(first: 50) {\n"
   "        nodes {\n"
   "          ... on ProjectV2Field { id name dataType }\n"
   "          ... on ProjectV2SingleSelectField { id name dataType options { id name } }\n"
   "          ... on ProjectV2IterationField { id name dataType }\n"
   "        }\n"
   "      }\n"
   "      views(first: 50) { nodes { id name number layout } }\n"
   "      items(first: 1) { totalCount }\n"
   "    }\n"
   "  }\n"
   "}\n"))

;; One page of project items; `$cursor` is omitted on the first request.
(define project-items-query
  (string-append "query($project: ID!, $first: Int!, $cursor: String) {\n"
                 "  node(id: $project) {\n"
                 "    ... on ProjectV2 {\n"
                 "      items(first: $first, after: $cursor) {\n"
                 "        pageInfo { hasNextPage endCursor }\n"
                 "        nodes { id content { ... on Issue { number } } }\n"
                 "      }\n"
                 "    }\n"
                 "  }\n"
                 "}\n"))

;; `singleSelectOptions` is INLINE text: gh `-f` cannot send a nested input
;; object, and the Python script inlined it too.
(define (create-single-select-mutation options)
  (format
   (string-append
    "mutation($project: ID!, $name: String!) {\n"
    "  createProjectV2Field(input: {projectId: $project, name: $name, "
    "dataType: SINGLE_SELECT, singleSelectOptions: [~a]}) {\n"
    "    projectV2Field { ... on ProjectV2SingleSelectField { id name dataType options { id name } } }\n"
    "  }\n"
    "}\n")
   (option-input-text options)))

(define create-text-mutation
  (string-append
   "mutation($project: ID!, $name: String!) {\n"
   "  createProjectV2Field(input: {projectId: $project, name: $name, dataType: TEXT}) {\n"
   "    projectV2Field { ... on ProjectV2Field { id name dataType } }\n"
   "  }\n"
   "}\n"))

(define add-item-mutation
  (string-append
   "mutation($project: ID!, $content: ID!) {\n"
   "  addProjectV2ItemById(input: {projectId: $project, contentId: $content}) { item { id } }\n"
   "}\n"))

;; The Python `REPO` issues route, paginated and slurped by the api session.
(define issues-route "issues?state=open&per_page=100")

;; ============================================================
;; Repository identity
;; ============================================================

;; `repository` (scripts/gsd-delivery-pr.rkt) already refuses an origin that is
;; not a GitHub repository. This adds the identity CHECK the Python script only
;; hardcoded: the slug must be the one the board belongs to.
(define (repository-identity ctx #:expected-slug [expected-slug #f])
  (define git (delivery-pr-context-git ctx))
  (define slug (repository git))
  (when (and expected-slug (not (equal? slug expected-slug)))
    (refuse!
     (format
      "repository-identity-mismatch: origin identifies ~a, expected ~a; nothing was read or mutated"
      slug
      expected-slug)))
  slug)

;; ============================================================
;; Project read
;; ============================================================

;; One GraphQL read of the project; every shape refusal names the exact path.
(define (read-project ctx #:owner owner #:project-number number)
  (define response
    (gh-graphql ctx
                project-query
                (list (cons "number" (number->string number)) (cons "owner" owner))))
  (define project (dig response 'data 'user 'projectV2))
  (cond
    [(not (hash? project))
     (refuse! (format (string-append
                       "project-not-found: user ~a has no Projects V2 number ~a; create it in the "
                       "Projects UI (GraphQL cannot create a user-owned project) and add the "
                       "Start/Target/Iteration fields there")
                      owner
                      number))]
    [else
     (define id (hash-ref project 'id #f))
     (unless (nonblank-string? id)
       (refuse! "malformed-project-response: project id is not a non-empty string"))
     (define title (hash-ref project 'title #f))
     (unless (string? title)
       (refuse! "malformed-project-response: project title is missing"))
     (define field-nodes (dig project 'fields 'nodes))
     (unless (list? field-nodes)
       (refuse! "malformed-project-response: fields.nodes is not a list"))
     (project-summary project)]))

;; items.totalCount is the Python verification value; a missing or negative
;; count is a refusal, never a 0.
(define (project-summary project)
  (define total (dig project 'items 'totalCount))
  (unless (and (exact-integer? total) (>= total 0))
    (refuse! "malformed-project-response: items.totalCount is not a non-negative integer"))
  (define views (dig project 'views 'nodes))
  (unless (list? views)
    (refuse! "malformed-project-response: views.nodes is not a list"))
  (hasheq 'id
          (hash-ref project 'id)
          'number
          (dig project 'number)
          'title
          (hash-ref project 'title)
          'url
          (hash-ref project 'url #f)
          'fields
          (for/list ([node (in-list (dig project 'fields 'nodes))]
                     #:when (hash? node))
            (hasheq 'id
                    (hash-ref node 'id #f)
                    'name
                    (hash-ref node 'name #f)
                    'dataType
                    (hash-ref node 'dataType #f)
                    'options
                    (for/list ([option (in-list (or (dig node 'options) '()))]
                               #:when (hash? option))
                      (hash-ref option 'name #f))))
          'views
          (for/list ([view (in-list (dig project 'views 'nodes))]
                     #:when (hash? view))
            (hasheq 'name
                    (hash-ref view 'name #f)
                    'number
                    (hash-ref view 'number #f)
                    'layout
                    (hash-ref view 'layout #f)))
          'itemCount
          total))

;; ============================================================
;; Field verification / creation
;; ============================================================

(define (require-field-of-type! field expected-type)
  (define actual (hash-ref field 'dataType #f))
  (unless (equal? actual expected-type)
    (refuse!
     (format
      "field-data-type-mismatch: field ~a exists with dataType ~a, expected ~a; fix it in the Projects UI, nothing was mutated"
      (hash-ref field 'name)
      (or actual "none")
      expected-type))))

;; An existing single-select field must already carry every required option: a
;; missing option silently breaks status/priority automation, and rewriting the
;; set would detach stored values (DEVIATION 2).
(define (require-field-options! field required-options)
  (define present (hash-ref field 'options '()))
  (define missing
    (for/list ([name (in-list required-options)]
               #:unless (member name present))
      name))
  (unless (null? missing)
    (refuse!
     (format
      "field-options-missing: field ~a lacks options ~a; add them in the Projects UI, the option set is never rewritten in place"
      (hash-ref field 'name)
      (string-join missing ", ")))))

(define (ensure-fields! ctx project-id fields #:dry-run? [dry-run? #f])
  (define created '())
  (define (existing name)
    (for/first ([field (in-list fields)]
                #:when (equal? (hash-ref field 'name #f) name))
      field))
  ;; Status is VERIFIED (never rewritten): it must exist, be a single-select and
  ;; carry the documented option set.
  (define status (existing status-field-name))
  (unless status
    (refuse! (format "status-field-missing: the project has no ~a field; add it in the Projects UI"
                     status-field-name)))
  (require-field-of-type! status "SINGLE_SELECT")
  (require-field-options! status (status-option-names))
  ;; The four custom single-select tables, then the TEXT field.
  (for ([spec (in-list select-field-specs)])
    (match-let ([(list name options) spec])
      (define field (existing name))
      (cond
        [field
         (require-field-of-type! field "SINGLE_SELECT")
         (require-field-options! field (map car options))]
        [dry-run? (set! created (append created (list name)))]
        [else
         (define response
           (gh-graphql ctx
                       (create-single-select-mutation options)
                       (list (cons "name" name) (cons "project" project-id))))
         (define created-id (dig response 'data 'createProjectV2Field 'projectV2Field 'id))
         (unless (nonblank-string? created-id)
           (refuse! (format "malformed-project-response: createProjectV2Field ~a returned no id"
                            name)))
         (set! created (append created (list name)))])))
  (for ([name (in-list text-field-names)])
    (define field (existing name))
    (cond
      [field (require-field-of-type! field "TEXT")]
      [dry-run? (set! created (append created (list name)))]
      [else
       (define response
         (gh-graphql ctx create-text-mutation (list (cons "name" name) (cons "project" project-id))))
       (define created-id (dig response 'data 'createProjectV2Field 'projectV2Field 'id))
       (unless (nonblank-string? created-id)
         (refuse! (format "malformed-project-response: createProjectV2Field ~a returned no id" name)))
       (set! created (append created (list name)))]))
  created)

;; ============================================================
;; Board item sync
;; ============================================================

;; `--paginate --slurp` yields a list of pages; each page must be a list.
(define (flatten-issue-pages data)
  (unless (list? data)
    (refuse! "malformed-issue-page: the issues response is not a list"))
  (define pages
    (if (and (pair? data) (andmap list? data))
        data
        (list data)))
  (append* pages))

;; The REST issues endpoint also returns pull requests; the Python filter was
;; `[i for i in issues if "pull_request" not in i]` and is preserved.
(define (open-issue-entries ctx slug)
  (define api (delivery-pr-context-api ctx))
  (define data (api slug issues-route #:paginate? #t))
  (for/list ([entry (in-list (flatten-issue-pages data))]
             #:unless (and (hash? entry) (hash-has-key? entry 'pull_request)))
    (unless (hash? entry)
      (refuse! "malformed-issue-entry: an issues page entry is not an object"))
    (define number (hash-ref entry 'number #f))
    (unless (and (exact-integer? number) (> number 0))
      (refuse! "malformed-issue-entry: an open issue has no usable number"))
    (define node (hash-ref entry 'node_id #f))
    (unless (nonblank-string? node)
      (refuse! (format "malformed-issue-entry: issue ~a has no node_id" number)))
    (hasheq 'number number 'node_id node 'title (hash-ref entry 'title #f))))

;; Every project item already on the board, page by page, refusing an
;; incomplete pageInfo instead of stopping early.
(define (project-item-numbers ctx project-id)
  (let loop ([cursor #f]
             [seen '()])
    (define variables
      (append (list (cons "first" (number->string items-page-size)) (cons "project" project-id))
              (if cursor
                  (list (cons "cursor" cursor))
                  '())))
    (define response (gh-graphql ctx project-items-query variables))
    (define items (dig response 'data 'node 'items))
    (unless (hash? items)
      (refuse! "malformed-project-items-response: data.node.items is not an object"))
    (define nodes (hash-ref items 'nodes #f))
    (unless (list? nodes)
      (refuse! "malformed-project-items-response: items.nodes is not a list"))
    (define numbers
      (for/list ([node (in-list nodes)])
        (unless (hash? node)
          (refuse! "malformed-project-item: a project item is not an object"))
        (define content (hash-ref node 'content #f))
        ;; A draft item has no issue content; it can never collide with an issue.
        (cond
          [(not (hash? content)) #f]
          [else
           (define number (hash-ref content 'number #f))
           (cond
             [(not number) #f]
             [(and (exact-integer? number) (> number 0)) number]
             [else
              (refuse! (format "malformed-project-item: item ~a has an unusable issue number"
                               (hash-ref node 'id #f)))])])))
    (define all (append seen numbers))
    (define page-info (hash-ref items 'pageInfo #f))
    (unless (hash? page-info)
      (refuse! "malformed-project-items-response: items.pageInfo is not an object"))
    (cond
      [(not (eq? (hash-ref page-info 'hasNextPage #f) #t)) all]
      [else
       (define next (hash-ref page-info 'endCursor #f))
       (unless (nonblank-string? next)
         (refuse!
          "project-items-pagination-incomplete: hasNextPage is true but endCursor is empty; refusing a partial board read"))
       (loop next all)])))

;; Add only the issues whose number is not already an item. Issues are visited
;; in ascending number order so the argv sequence is deterministic.
(define (sync-project-items! ctx slug project-id #:dry-run? [dry-run? #f])
  (define issues (open-issue-entries ctx slug))
  (define existing (project-item-numbers ctx project-id))
  (define pending
    (sort (for/list ([issue (in-list issues)]
                     #:unless (member (hash-ref issue 'number) existing))
            issue)
          <
          #:key (lambda (issue) (hash-ref issue 'number))))
  (define added
    (if dry-run?
        (map (lambda (issue) (hash-ref issue 'number)) pending)
        (for/list ([issue (in-list pending)])
          (define response
            (gh-graphql ctx
                        add-item-mutation
                        (list (cons "content" (hash-ref issue 'node_id))
                              (cons "project" project-id))))
          (define item-id (dig response 'data 'addProjectV2ItemById 'item 'id))
          (unless (nonblank-string? item-id)
            (refuse!
             (format
              "malformed-add-item-response: addProjectV2ItemById for issue ~a returned no item id"
              (hash-ref issue 'number))))
          (hash-ref issue 'number))))
  (hasheq 'openIssues
          (length issues)
          'alreadyOnBoard
          (- (length issues) (length pending))
          'added
          added))

;; ============================================================
;; The setup operation
;; ============================================================

(define (delivery-project-setup! repo
                                 #:owner owner
                                 #:project-number number
                                 #:expected-slug [expected-slug #f]
                                 #:dry-run? [dry-run? #f]
                                 #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  (define slug (repository-identity ctx #:expected-slug expected-slug))
  (define project (read-project ctx #:owner owner #:project-number number))
  (define project-id (hash-ref project 'id))
  ;; The context carries the repository the identity was checked against, so
  ;; the item sync lists issues for exactly that slug.
  (define created (ensure-fields! ctx project-id (hash-ref project 'fields) #:dry-run? dry-run?))
  (define sync (sync-project-items! ctx slug project-id #:dry-run? dry-run?))
  (hasheq 'status
          (if dry-run? "dry-run" "ready")
          'repository
          slug
          'project
          (hasheq 'id
                  project-id
                  'number
                  (hash-ref project 'number)
                  'title
                  (hash-ref project 'title)
                  'url
                  (hash-ref project 'url))
          'fields
          (for/list ([field (in-list (hash-ref project 'fields))])
            (hash-ref field 'name))
          'views
          (for/list ([view (in-list (hash-ref project 'views))])
            (hash-ref view 'name))
          'itemCount
          (hash-ref project 'itemCount)
          'createdFields
          created
          'itemSync
          sync))

;; ============================================================
;; CLI
;; ============================================================

(define (require-flag flags name)
  (define value (hash-ref flags name #f))
  (unless (and (nonblank-string? value))
    (refuse! (format "project-setup requires ~a" name)))
  value)

(define (project-setup-main argv #:exit [exit-procedure exit])
  (with-handlers ([exn:fail:delivery-action?
                   (lambda (error)
                     ;; Same shape the coordinator's typed-stop parser expects.
                     (printf "~a\n" (delivery-pending-json (exn-message error)))
                     (exit-procedure 2))]
                  [exn:fail? (lambda (error)
                               (eprintf "project-setup: ~a\n" (exn-message error))
                               (exit-procedure 1))])
    (define dry-run? (and (member "--dry-run" argv) #t))
    (define flags (read-flag-value (remove "--dry-run" argv)))
    (define owner (require-flag flags "--owner"))
    (define number-text (require-flag flags "--project-number"))
    (define number (string->number number-text))
    (unless (and number (exact-integer? number) (> number 0))
      (refuse! (format "malformed project number: ~a" number-text)))
    (define report
      (delivery-project-setup! (or (hash-ref flags "--repo" #f) (current-directory))
                               #:owner owner
                               #:project-number number
                               #:expected-slug (hash-ref flags "--expected-slug" #f)
                               #:dry-run? dry-run?))
    (printf "~a\n" (jsexpr-string report))
    (exit-procedure 0)))

(module+ main
  (project-setup-main (vector->list (current-command-line-arguments))))
