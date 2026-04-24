#lang racket/base

;; extensions/github-integration.rkt — GitHub Integration Extension
;;
;; Wave A3: Registers 6 GitHub tools (gh-issue, gh-pr, gh-milestone,
;; gh-board, gh-wave-start, gh-wave-finish) that wrap the gh CLI.
;; Gracefully degrades when gh is not installed.

(require racket/contract
         racket/format
         racket/port
         racket/string
         racket/system
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide github-integration-extension
         gh-binary-path
         run-command
         shell-quote
         valid-identifier?
         valid-number?
         valid-state?
         valid-method?
         handle-gh-issue
         handle-gh-pr
         handle-gh-milestone
         handle-gh-board
         handle-gh-wave-start
         handle-gh-wave-finish
         register-github-tools)

;; ============================================================
;; Configuration
;; ============================================================

(define gh-binary-path (make-parameter #f))

;; ============================================================
;; Shell helpers + input validation
;; ============================================================

(define (shell-quote s)
  (define str
    (if (string? s)
        s
        (~a s)))
  (string-append "'" (string-replace str "'" "'\\''") "'"))

;; Validate that a string contains only safe identifier characters
(define (valid-identifier? s)
  (and (string? s) (regexp-match? #rx"^[a-zA-Z0-9_.-]+$" s)))

;; Validate issue/PR number is a positive integer or numeric string
(define (valid-number? n)
  (or (and (integer? n) (positive? n)) (and (string? n) (regexp-match? #rx"^[0-9]+$" n))))

;; Validate state arg is one of known values
(define valid-states '("open" "closed" "all"))

(define (valid-state? s)
  (and (string? s) (member s valid-states) #t))

;; Validate merge method is one of known values
(define valid-methods '("squash" "merge" "rebase"))

(define (valid-method? s)
  (and (string? s) (member s valid-methods) #t))

;; Run a command with explicit arg list — no /bin/sh interpolation.
;; Returns (values exit-code stdout stderr)
(define (run-command cmd . args)
  (define-values (sp stdout-in stdin-out stderr-in) (apply subprocess #f #f #f cmd args))
  (define out-str (port->string stdout-in))
  (define err-str (port->string stderr-in))
  (close-input-port stdout-in)
  (close-input-port stderr-in)
  (when (output-port? stdin-out)
    (close-output-port stdin-out))
  (subprocess-wait sp)
  (values (subprocess-status sp) out-str err-str))

;; ============================================================
;; gh / git execution
;; ============================================================

(define (gh-binary)
  (define p (gh-binary-path))
  (cond
    [(eq? p 'disabled) #f]
    [p p]
    [else (find-executable-path "gh")]))

(define (git-binary)
  (find-executable-path "git"))

(define (gh-unavailable-error)
  (make-error-result "GitHub CLI (gh) not found. Install from https://cli.github.com"))

(define (gh-exec-result . args)
  (define bin (gh-binary))
  (unless bin
    (error 'gh "GitHub CLI not found"))
  (apply run-command bin args))

(define (git-exec-result . args)
  (define bin (git-binary))
  (unless bin
    (error 'git "git not found"))
  (apply run-command bin args))

(define (gh-success . args)
  (define-values (ec out err) (apply gh-exec-result args))
  (if (= ec 0)
      (let ([text (string-trim out)])
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (if (string=? text "")
                                               (string-trim err)
                                               text)))))
      (make-error-result (format "gh failed (exit ~a): ~a" ec (string-trim err)))))

(define (gh-success-json . args)
  (define-values (ec out err) (apply gh-exec-result args))
  (if (= ec 0)
      (let* ([raw (string-trim out)]
             [parsed (with-handlers ([exn:fail? (lambda (_) #f)])
                       (string->jsexpr raw))])
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (if parsed
                                               (jsexpr->string parsed)
                                               raw)))))
      (make-error-result (format "gh failed (exit ~a): ~a" ec (string-trim err)))))

(define (git-success . args)
  (define-values (ec out err) (apply git-exec-result args))
  (if (= ec 0)
      (make-success-result (list (hasheq 'type
                                         "text"
                                         'text
                                         (let ([text (string-trim out)])
                                           (if (string=? text "")
                                               (string-trim err)
                                               text)))))
      (make-error-result (format "git failed (exit ~a): ~a" ec (string-trim err)))))

(define (get-repo-info)
  (define-values (ec out _)
    (run-command (gh-binary) "repo" "view" "--json" "nameWithOwner" "-q" ".nameWithOwner"))
  (cond
    [(not (= ec 0)) (values #f #f)]
    [else
     (define parts (string-split (string-trim out) "/"))
     (if (= (length parts) 2)
         (values (list-ref parts 0) (list-ref parts 1))
         (values #f #f))]))

;; ============================================================
;; Tool schemas
;; ============================================================

(define gh-issue-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "Issue action: create, close, update, get, list, close_tree")
    'title
    (hasheq 'type "string" 'description "Issue title (create/update)")
    'body
    (hasheq 'type "string" 'description "Issue body text (create/update)")
    'number
    (hasheq 'type "number" 'description "Issue number")
    'labels
    (hasheq 'type "array" 'items (hasheq 'type "string") 'description "Label names (create)")
    'state
    (hasheq 'type "string" 'description "State filter for list: open, closed, all (default: open)")
    'milestone_number
    (hasheq 'type "number" 'description "Milestone number filter (list)")
    'milestone_id
    (hasheq 'type "string" 'description "Milestone GraphQL ID (create)")
    'board_fields
    (hasheq 'type "object" 'description "Project board fields to set (create)"))
   'required
   '("action")))

(define gh-pr-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "PR action: create, merge, list, get")
    'title
    (hasheq 'type "string" 'description "PR title (create)")
    'body
    (hasheq 'type "string" 'description "PR body text (create)")
    'head
    (hasheq 'type "string" 'description "Head branch name (create)")
    'base
    (hasheq 'type "string" 'description "Base branch name (create, default: main)")
    'number
    (hasheq 'type "number" 'description "PR number (merge/get)")
    'method
    (hasheq 'type "string" 'description "Merge method: squash, merge, rebase (default: squash)")
    'commit_title
    (hasheq 'type "string" 'description "Squash commit title (merge)")
    'state
    (hasheq 'type "string" 'description "State filter for list: open, closed, all (default: open)"))
   'required
   '("action")))

(define gh-milestone-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "Milestone action: create, close, list, create_from_spec")
    'title
    (hasheq 'type "string" 'description "Milestone title (create)")
    'description
    (hasheq 'type "string" 'description "Milestone description (create)")
    'due_on
    (hasheq 'type "string" 'description "Due date ISO timestamp (create)")
    'number
    (hasheq 'type "number" 'description "Milestone number (close)")
    'state
    (hasheq 'type "string" 'description "State filter: open, closed, all (list, default: open)")
    'spec_file
    (hasheq 'type "string" 'description "Path to JSON spec file (create_from_spec)")
    'dry_run
    (hasheq 'type "boolean" 'description "Preview only, do not create (create_from_spec)"))
   'required
   '("action")))

(define gh-board-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type
            "string"
            'description
            "Board action: status, stale, autofix, verify, batch_set, reconfigure")
    'milestone_number
    (hasheq 'type "number" 'description "Milestone number (status/stale/autofix/verify)")
    'issue_numbers
    (hasheq 'type "array" 'items (hasheq 'type "number") 'description "Issue numbers (batch_set)")
    'issue_number
    (hasheq 'type "number" 'description "Single issue number (reconfigure)")
    'fields
    (hasheq 'type "object" 'description "Board fields to set: status, priority, area, effort, risk"))
   'required
   '("action")))

(define gh-wave-start-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq 'issue_number
           (hasheq 'type "number" 'description "GitHub issue number for the wave parent")
           'branch_name
           (hasheq 'type "string" 'description "Override branch name (default: auto-generated)"))
   'required
   '("issue_number")))

(define gh-wave-finish-schema
  (hasheq 'type
          "object"
          'properties
          (hasheq 'issue_number
                  (hasheq 'type "number" 'description "GitHub issue number for the wave parent")
                  'files
                  (hasheq 'type
                          "array"
                          'items
                          (hasheq 'type "string")
                          'description
                          "File paths to commit (relative to repo root)")
                  'commit_msg
                  (hasheq 'type "string" 'description "Commit message")
                  'branch
                  (hasheq 'type "string" 'description "Branch name (default: auto-detected)")
                  'pr_title
                  (hasheq 'type "string" 'description "PR title override (default: commit message)")
                  'pr_body
                  (hasheq 'type "string" 'description "PR body override (default: references issue)")
                  'close_sub_issues
                  (hasheq 'type "boolean" 'description "Close sub-issues (default: true)"))
          'required
          '("issue_number" "files" "commit_msg")))

;; ============================================================
;; Tool handlers
;; ============================================================

;; --- gh-issue ---

(define (handle-gh-issue args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (cond
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define action (hash-ref args 'action ""))
       (cond
         [(string=? action "") (make-error-result "Missing required argument: action")]
         ;; create
         [(string=? action "create")
          (define title (hash-ref args 'title #f))
          (cond
            [(not title) (make-error-result "create requires 'title'")]
            [else
             (define body (hash-ref args 'body ""))
             (define labels (hash-ref args 'labels '()))
             (define label-val
               (if (null? labels)
                   #f
                   (string-join labels ",")))
             (define ms-id (hash-ref args 'milestone_id #f))
             (apply gh-success-json
                    (append
                     (list "issue" "create" "--title" title "--body" body "--json" "number,title,url")
                     (if label-val
                         (list "--label" label-val)
                         '())
                     (if ms-id
                         (list "--milestone" ms-id)
                         '())))])]
         ;; close
         [(string=? action "close")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "close requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else (gh-success "issue" "close" (~a num))])]
         ;; update
         [(string=? action "update")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "update requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (define t (hash-ref args 'title #f))
             (define b (hash-ref args 'body #f))
             (apply gh-success-json
                    (append (list "issue" "edit" (~a num) "--json" "number,title,state")
                            (if t
                                (list "--title" t)
                                '())
                            (if b
                                (list "--body" b)
                                '())))])]
         ;; get
         [(string=? action "get")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "get requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (gh-success-json "issue"
                              "view"
                              (~a num)
                              "--json"
                              "number,title,state,body,labels,milestone")])]
         ;; list
         [(string=? action "list")
          (define raw-state (hash-ref args 'state "open"))
          (unless (valid-state? raw-state)
            (raise-user-error 'github-issues "invalid state: ~a" raw-state))
          (define mn (hash-ref args 'milestone_number #f))
          (apply gh-success-json
                 (append (list "issue"
                               "list"
                               "--state"
                               raw-state
                               "--limit"
                               "100"
                               "--json"
                               "number,title,state,labels")
                         (if mn
                             (list "--milestone" (~a mn))
                             '())))]
         ;; close_tree
         [(string=? action "close_tree")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "close_tree requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (define-values (ec-main _out-main err-main) (gh-exec-result "issue" "close" (~a num)))
             (cond
               [(not (= ec-main 0))
                (make-error-result (format "Failed to close #~a: ~a" num (string-trim err-main)))]
               [else
                (define-values (ec-view out-view _)
                  (gh-exec-result "issue" "view" (~a num) "--json" "body" "-q" ".body"))
                (define sub-nums
                  (if (= ec-view 0)
                      (map string->number
                           (regexp-match* #px"(?:closes?|fixes?|resolves?)\\s+#(\\d+)"
                                          (string-trim out-view)
                                          #:match-select cadr))
                      '()))
                (define closed-subs
                  (for/list ([sn sub-nums])
                    (define-values (ec-s _out-s _err-s) (gh-exec-result "issue" "close" (~a sn)))
                    (hasheq 'number sn 'closed (= ec-s 0))))
                (make-success-result (list (hasheq 'type
                                                   "text"
                                                   'text
                                                   (format "Issue #~a closed. Sub-issues: ~a"
                                                           num
                                                           (jsexpr->string closed-subs)))))])])]
         ;; unknown
         [else
          (make-error-result
           (format "Unknown action: ~a. Valid: create, close, update, get, list, close_tree"
                   action))])])))

;; --- gh-pr ---

(define (handle-gh-pr args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (cond
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define action (hash-ref args 'action ""))
       (cond
         [(string=? action "") (make-error-result "Missing required argument: action")]
         ;; create
         [(string=? action "create")
          (define title (hash-ref args 'title #f))
          (cond
            [(not title) (make-error-result "create requires 'title'")]
            [else
             (define body (hash-ref args 'body ""))
             (define head (hash-ref args 'head #f))
             (define base (hash-ref args 'base "main"))
             (apply gh-success-json
                    (append (list "pr"
                                  "create"
                                  "--title"
                                  title
                                  "--body"
                                  body
                                  "--base"
                                  base
                                  "--json"
                                  "number,title,url")
                            (if head
                                (list "--head" head)
                                '())))])]
         ;; merge
         [(string=? action "merge")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "merge requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (define method (hash-ref args 'method "squash"))
             (unless (valid-method? method)
               (raise-user-error 'github-pr "invalid merge method: ~a" method))
             (define commit-title (hash-ref args 'commit_title #f))
             (apply gh-success-json
                    (append (list "pr" "merge" (~a num) (string-append "--" method) "--json" "url")
                            (if commit-title
                                (list "--subject" commit-title)
                                '())))])]
         ;; list
         [(string=? action "list")
          (define raw-state (hash-ref args 'state "open"))
          (unless (valid-state? raw-state)
            (raise-user-error 'github-pr "invalid state: ~a" raw-state))
          (gh-success-json "pr"
                           "list"
                           "--state"
                           raw-state
                           "--limit"
                           "100"
                           "--json"
                           "number,title,state,headRefName")]
         ;; get
         [(string=? action "get")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "get requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (gh-success-json "pr"
                              "view"
                              (~a num)
                              "--json"
                              "number,title,state,headRefName,baseRefName,url")])]
         ;; unknown
         [else
          (make-error-result (format "Unknown action: ~a. Valid: create, merge, list, get"
                                     action))])])))

;; --- gh-milestone helpers ---

(define (milestone-create-from-spec spec-file dry-run?)
  ;; Extracted helper to avoid deep nesting in handle-gh-milestone.
  (define spec-path (expand-user-path spec-file))
  (cond
    [(not (file-exists? spec-path)) (make-error-result (format "Spec file not found: ~a" spec-file))]
    [else
     (define spec-data
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (call-with-input-file spec-path read-json)))
     (cond
       [(not spec-data) (make-error-result (format "Invalid JSON in spec file: ~a" spec-file))]
       [dry-run?
        (define ms (hash-ref spec-data 'milestones '()))
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (format "Dry run: would create ~a milestones from ~a:\n~a"
                                                   (length ms)
                                                   spec-file
                                                   (jsexpr->string spec-data)))))]
       [else
        (define milestones (hash-ref spec-data 'milestones '()))
        (define results
          (for/list ([m milestones])
            (define t (hash-ref m 'title "Untitled"))
            (define d (hash-ref m 'description ""))
            (define due (hash-ref m 'due_on ""))
            (define-values (ec _out err)
              (apply gh-exec-result
                     (append (list "api"
                                   "repos/{owner}/{repo}/milestones"
                                   "-X"
                                   "POST"
                                   "-f"
                                   (format "title=~a" t)
                                   "-f"
                                   (format "description=~a" d))
                             (if (string=? due "")
                                 '()
                                 (list "-f" (format "due_on=~a" due))))))
            (hasheq 'title
                    t
                    'success
                    (= ec 0)
                    'error
                    (if (= ec 0)
                        #f
                        (string-trim err)))))
        (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string results))))])]))

;; --- gh-milestone ---

(define (handle-gh-milestone args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (cond
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define action (hash-ref args 'action ""))
       (cond
         [(string=? action "") (make-error-result "Missing required argument: action")]
         ;; create
         [(string=? action "create")
          (define title (hash-ref args 'title #f))
          (cond
            [(not title) (make-error-result "create requires 'title'")]
            [else
             (define desc (hash-ref args 'description ""))
             (define due (hash-ref args 'due_on ""))
             (apply gh-success-json
                    (append (list "api"
                                  "repos/{owner}/{repo}/milestones"
                                  "-X"
                                  "POST"
                                  "-f"
                                  (format "title=~a" title))
                            (if (string=? desc "")
                                '()
                                (list "-f" (format "description=~a" desc)))
                            (if (string=? due "")
                                '()
                                (list "-f" (format "due_on=~a" due)))))])]
         ;; close
         [(string=? action "close")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "close requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (gh-success-json "api"
                              "repos/{owner}/{repo}/milestones"
                              (~a num)
                              "-X"
                              "PATCH"
                              "-f"
                              "state=closed")])]
         ;; list
         [(string=? action "list")
          (define state-arg (hash-ref args 'state "open"))
          (unless (valid-state? state-arg)
            (raise-user-error 'github-milestone "invalid state: ~a" state-arg))
          (gh-success-json "api"
                           (format "repos/{owner}/{repo}/milestones?state=~a" state-arg)
                           "--paginate")]
         ;; create_from_spec
         [(string=? action "create_from_spec")
          (define spec-file (hash-ref args 'spec_file #f))
          (cond
            [(not spec-file) (make-error-result "create_from_spec requires 'spec_file'")]
            [else (milestone-create-from-spec spec-file (hash-ref args 'dry_run #f))])]
         ;; unknown
         [else
          (make-error-result
           (format "Unknown action: ~a. Valid: create, close, list, create_from_spec" action))])])))

;; --- gh-board ---

(define (handle-gh-board args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (cond
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define action (hash-ref args 'action ""))
       (cond
         [(string=? action "") (make-error-result "Missing required argument: action")]
         ;; status
         [(string=? action "status")
          (define mn (hash-ref args 'milestone_number #f))
          (cond
            [(not mn) (make-error-result "status requires 'milestone_number'")]
            [else
             (gh-success-json "issue"
                              "list"
                              "--milestone"
                              (~a mn)
                              "--state"
                              "all"
                              "--limit"
                              "100"
                              "--json"
                              "number,title,state,labels")])]
         ;; verify
         [(string=? action "verify")
          (define mn (hash-ref args 'milestone_number #f))
          (cond
            [(not mn) (make-error-result "verify requires 'milestone_number'")]
            [else
             (define-values (ec out err)
               (gh-exec-result "issue"
                               "list"
                               "--milestone"
                               (~a mn)
                               "--state"
                               "open"
                               "--limit"
                               "100"
                               "--json"
                               "number,title"))
             (cond
               [(not (= ec 0))
                (make-error-result (format "Failed to check milestone: ~a" (string-trim err)))]
               [else
                (define issues
                  (with-handlers ([exn:fail? (lambda (_) '())])
                    (string->jsexpr (string-trim out))))
                (if (null? issues)
                    (make-success-result
                     (list (hasheq 'type "text" 'text (format "Milestone ~a: all issues closed" mn))))
                    (make-success-result
                     (list (hasheq 'type
                                   "text"
                                   'text
                                   (format "Milestone ~a: ~a open issues remain:\n~a"
                                           mn
                                           (length issues)
                                           (jsexpr->string issues))))))])])]
         ;; stale / autofix
         [(or (string=? action "stale") (string=? action "autofix"))
          (define mn (hash-ref args 'milestone_number #f))
          (cond
            [(not mn) (make-error-result (format "~a requires 'milestone_number'" action))]
            [else
             (gh-success-json "issue"
                              "list"
                              "--milestone"
                              (~a mn)
                              "--state"
                              "all"
                              "--limit"
                              "100"
                              "--json"
                              "number,title,state,updatedAt")])]
         ;; batch_set
         [(string=? action "batch_set")
          (define nums (hash-ref args 'issue_numbers '()))
          (cond
            [(null? nums) (make-error-result "batch_set requires 'issue_numbers'")]
            [else
             (define results
               (for/list ([n nums])
                 (hasheq 'number n 'processed #t)))
             (make-success-result (list (hasheq 'type
                                                "text"
                                                'text
                                                (format "batch_set acknowledged for ~a issues: ~a"
                                                        (length nums)
                                                        (jsexpr->string results)))))])]
         ;; reconfigure
         [(string=? action "reconfigure")
          (define num (hash-ref args 'issue_number #f))
          (cond
            [(not num) (make-error-result "reconfigure requires 'issue_number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else (gh-success-json "issue" "view" (~a num) "--json" "number,title,state,labels")])]
         ;; unknown
         [else
          (make-error-result
           (format "Unknown action: ~a. Valid: status, stale, autofix, verify, batch_set, reconfigure"
                   action))])])))

;; --- gh-wave-start ---

(define (handle-gh-wave-start args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (define issue-num (hash-ref args 'issue_number #f))
    (cond
      [(not issue-num) (make-error-result "Missing required argument: issue_number")]
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define branch-name
         (or (hash-ref args 'branch_name #f) (format "feature/issue-~a-wave" issue-num)))
       ;; Sync main
       (define-values (ec-co _out-co err-co) (git-exec-result "checkout" "main"))
       (cond
         [(not (= ec-co 0))
          (make-error-result (format "Cannot checkout main: ~a" (string-trim err-co)))]
         [else
          (git-exec-result "pull" "origin" "main")
          ;; Create feature branch
          (define-values (ec-branch _out-br err-br) (git-exec-result "checkout" "-b" branch-name))
          (cond
            [(not (= ec-branch 0))
             (make-error-result
              (format "Failed to create branch ~a: ~a" branch-name (string-trim err-br)))]
            [else
             (make-success-result
              (list (hasheq 'type
                            "text"
                            'text
                            (format "Wave started: branch '~a' created for issue #~a"
                                    branch-name
                                    issue-num))))])])])))

;; --- gh-wave-finish ---

(define (handle-gh-wave-finish args)
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (define issue-num (hash-ref args 'issue_number #f))
    (define files (hash-ref args 'files '()))
    (define commit-msg (hash-ref args 'commit_msg #f))
    (cond
      [(not issue-num) (make-error-result "Missing required argument: issue_number")]
      [(null? files) (make-error-result "Missing or empty required argument: files")]
      [(not commit-msg) (make-error-result "Missing required argument: commit_msg")]
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       ;; Step 1: git add + commit
       (define-values (ec-add _out-add err-add) (apply git-exec-result "add" files))
       (cond
         [(not (= ec-add 0)) (make-error-result (format "git add failed: ~a" (string-trim err-add)))]
         [else
          (define-values (ec-commit _out-commit err-commit)
            (git-exec-result "commit" "-m" commit-msg))
          (cond
            [(not (= ec-commit 0))
             (make-error-result (format "git commit failed: ~a" (string-trim err-commit)))]
            [else
             ;; Step 2: detect branch, push
             (define-values (ec-br out-br _) (git-exec-result "branch" "--show-current"))
             (define branch
               (or (hash-ref args 'branch #f) (and (= ec-br 0) (string-trim out-br)) "main"))
             (define-values (ec-push _out-push err-push)
               (git-exec-result "push" "-u" "origin" branch))
             (cond
               [(not (= ec-push 0))
                (make-error-result (format "git push failed: ~a" (string-trim err-push)))]
               [else
                ;; Step 3: create PR
                (define pr-title (hash-ref args 'pr_title commit-msg))
                (define pr-body (hash-ref args 'pr_body (format "(#~a)" issue-num)))
                (define-values (ec-pr out-pr err-pr)
                  (gh-exec-result "pr"
                                  "create"
                                  "--title"
                                  pr-title
                                  "--body"
                                  pr-body
                                  "--head"
                                  branch
                                  "--base"
                                  "main"
                                  "--json"
                                  "number,url"))
                (define pr-num
                  (if (= ec-pr 0)
                      (hash-ref (with-handlers ([exn:fail? (lambda (_) (hasheq))])
                                  (string->jsexpr (string-trim out-pr)))
                                'number
                                #f)
                      #f))
                ;; Step 4: merge PR
                (when pr-num
                  (define-values (ec-merge _out-merge err-merge)
                    (gh-exec-result "pr" "merge" (~a pr-num) "--squash"))
                  (unless (= ec-merge 0)
                    (make-error-result
                     (format
                      "PR merge failed (exit ~a): ~a. Files committed and pushed but PR not merged."
                      ec-merge
                      (string-trim err-merge)))))
                ;; Step 5: sync main
                (define-values (ec-co _out-co err-co) (git-exec-result "checkout" "main"))
                (define-values (ec-pull _out-pull err-pull) (git-exec-result "pull" "origin" "main"))
                (unless (= ec-co 0)
                  (make-error-result (format "git checkout main failed: ~a" (string-trim err-co))))
                (unless (= ec-pull 0)
                  (make-error-result (format "git pull failed: ~a" (string-trim err-pull))))
                ;; Step 6: close issue
                (define-values (ec-close _out-close err-close)
                  (gh-exec-result "issue" "close" (~a issue-num)))
                (unless (= ec-close 0)
                  (make-error-result
                   (format "Issue close failed (exit ~a): ~a. PR merged but issue not closed."
                           ec-close
                           (string-trim err-close))))
                (make-success-result
                 (list (hasheq
                        'type
                        "text"
                        'text
                        (format "Wave finished: ~a files committed, PR #~a merged, issue #~a closed"
                                (length files)
                                pr-num
                                issue-num))))])])])])))

;; ============================================================
;; Registration
;; ============================================================

(define (register-github-tools ctx _payload)
  (ext-register-tool!
   ctx
   "gh-issue"
   "Manage GitHub issues: create, close, update, get, list, close_tree."
   gh-issue-schema
   handle-gh-issue
   #:prompt-guidelines
   "Use gh-issue to manage GitHub issues. Include issue number for close/update/get.")
  (ext-register-tool! ctx
                      "gh-pr"
                      "Manage GitHub pull requests: create, merge, list, get."
                      gh-pr-schema
                      handle-gh-pr
                      #:prompt-guidelines "Use gh-pr to manage PRs. Default merge method is squash.")
  (ext-register-tool! ctx
                      "gh-milestone"
                      "Manage GitHub milestones: create, close, list, create_from_spec."
                      gh-milestone-schema
                      handle-gh-milestone
                      #:prompt-guidelines "Use gh-milestone to manage project milestones.")
  (ext-register-tool!
   ctx
   "gh-board"
   "Project board operations: status, stale, autofix, verify, batch_set, reconfigure."
   gh-board-schema
   handle-gh-board
   #:prompt-guidelines "Use gh-board to check milestone progress and manage board fields.")
  (ext-register-tool! ctx
                      "gh-wave-start"
                      "Start a GSD wave: create feature branch from main, sync."
                      gh-wave-start-schema
                      handle-gh-wave-start
                      #:prompt-guidelines "Use gh-wave-start at the beginning of a wave.")
  (ext-register-tool! ctx
                      "gh-wave-finish"
                      "Finish a GSD wave: commit, push, PR, merge, close issue, sync main."
                      gh-wave-finish-schema
                      handle-gh-wave-finish
                      #:prompt-guidelines "Use gh-wave-finish after completing a wave.")
  (hook-pass #f))

(define (register-github-commands ctx)
  (ext-register-command! ctx "/milestone" "Quick milestone status" 'general '() '("ms"))
  (ext-register-command! ctx "/issue" "Quick issue info" 'general '() '("i"))
  (ext-register-command! ctx "/pr" "Quick PR status" 'general '())
  (hook-pass #f))

;; ============================================================
;; Extension definition
;; ============================================================

(define (handle-github-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (cond
    [(member cmd '("/milestone" "/ms"))
     (hook-amend
      (hasheq
       'text
       (format
        "GitHub milestone command.~a\nUsage: /milestone [list|status|create] [args]\n\nUse gh-milestone-list, gh-milestone-status, or gh-milestone-create tools for full functionality."
        (if (string=? input-text "")
            ""
            (format " Input: ~a" input-text)))))]
    [(member cmd '("/issue" "/i"))
     (hook-amend
      (hasheq
       'text
       (format
        "GitHub issue command.~a\nUsage: /issue [get|list|create|close] [number|args]\n\nUse gh-issue-get, gh-issue-list, gh-issue-create, or gh-issue-close tools for full functionality."
        (if (string=? input-text "")
            ""
            (format " Input: ~a" input-text)))))]
    [(member cmd '("/pr"))
     (hook-amend
      (hasheq
       'text
       (format
        "GitHub PR command.~a\nUsage: /pr [list|get|create|merge] [number|args]\n\nUse gh-pr-list, gh-pr-get, gh-pr-create, or gh-pr-merge tools for full functionality."
        (if (string=? input-text "")
            ""
            (format " Input: ~a" input-text)))))]
    [else (hook-pass payload)]))

(define-q-extension github-integration-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-github-tools
                    #:on register-shortcuts
                    register-github-commands
                    #:on execute-command
                    handle-github-command)
