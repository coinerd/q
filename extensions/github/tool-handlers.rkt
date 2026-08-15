#lang racket/base

;; extensions/github/tool-handlers.rkt — GitHub tool handlers and registration
;;
;; Thin facade: imports handlers from github/handlers/*, keeps wave
;; start/finish and tool/command registration inline.

(require racket/format
         racket/string
         (only-in racket/file [file->string gh-file->string])
         json
         "../dynamic-tools.rkt"
         "../hooks.rkt"
         "../tool-api.rkt"
         "../ext-commands.rkt"
         (only-in "helpers.rkt"
                  gh-binary
                  gh-unavailable-error
                  gh-exec-result
                  git-exec-result
                  with-error-result)
         "tool-schemas.rkt"
         "handlers/issue-ops.rkt"
         "handlers/pr-ops.rkt"
         "handlers/milestone-ops.rkt")

(provide handle-gh-issue
         handle-gh-pr
         handle-gh-milestone
         handle-gh-board
         handle-gh-wave-start
         handle-gh-wave-finish
         register-github-tools
         register-github-commands)

;; with-error-result imported from helpers.rkt

;; ============================================================
;; Wave handlers
;; ============================================================

(define (handle-gh-wave-start args [exec-ctx #f])
  (with-error-result
   "github operation"
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

(define (wave-state-path)
  (let-values ([(_ec out _e) (git-exec-result "rev-parse" "--show-toplevel")])
    (define root (string-trim out))
    (build-path (if (non-empty-string? root) root ".") ".planning" "STATE.md")))

(define (handle-gh-wave-finish args [exec-ctx #f])
  (with-error-result
   "github operation"
   ;; W6 (BUG-0011): durable checkpoints + idempotent milestone actions.
   ;; wave-id/step/state-path drive the resume checklist in STATE.md;
   ;; files drives the tree/content already-committed check.
   (define wave-id (hash-ref args 'wave_id "W0"))
   (define step (hash-ref args 'step "wave-finish"))
   (define state-path (hash-ref args 'state_path ".planning/STATE.md"))
   (define summary (hash-ref args 'summary "Wave complete"))
   (define issue-num (hash-ref args 'issue_number #f))
   (define files (hash-ref args 'files '()))
   (define state-content (read-state-content state-path))
   (cond
     [(not (gh-binary)) (gh-unavailable-error)]
     [(wave-step-completed? state-content wave-id step)
      (make-success-result
       (list (hasheq
              'type
              "text"
              'text
              (format "Wave ~a step '~a' already recorded in ~a - skipping (idempotent resume)."
                      wave-id
                      step
                      state-path))))]
     [(wave-already-committed? files)
      (write-wave-checkpoint! state-path wave-id step)
      (make-success-result
       (list (hasheq
              'type
              "text"
              'text
              (format "Wave ~a step '~a': change already committed (tree/content check) - no-op."
                      wave-id
                      step))))]
     [else
      (unless (pair? files)
        (raise-user-error 'gh-wave-finish "step '~a': 'files' required" step))
      (define-values (ec-add _out-add err-add) (git-exec-result "add" "-A"))
      (unless (= ec-add 0)
        (raise-user-error 'gh-wave-finish "Failed to stage: ~a" (string-trim err-add)))
      (define commit-msg
        (or (hash-ref args 'commit_message #f) (format "wave: ~a (issue #~a)" summary issue-num)))
      (define-values (ec-commit _out-c err-commit) (git-exec-result "commit" "-m" commit-msg))
      (unless (= ec-commit 0)
        (raise-user-error 'gh-wave-finish "Commit failed: ~a" (string-trim err-commit)))
      (define-values (ec-hb out-hb _err-hb) (git-exec-result "rev-parse" "--abbrev-ref" "HEAD"))
      (define head-branch (string-trim out-hb))
      (define-values (ec-push _out-p err-push) (git-exec-result "push" "origin" head-branch))
      (unless (= ec-push 0)
        (raise-user-error 'gh-wave-finish "Push failed: ~a" (string-trim err-push)))
      ;; Idempotent PR create (BUG-0011): lookup-first - reuse the existing
      ;; open PR for the head branch instead of double-creating.
      (define existing-pr (find-open-pr-for-head head-branch))
      (define pr-num
        (or (and existing-pr (hash-ref existing-pr 'number #f))
            (let ()
              (define-values (ec-pr out-pr err-pr)
                (gh-exec-result "pr"
                                "create"
                                "--title"
                                (format "Wave ~a" summary)
                                "--body"
                                (or (hash-ref args 'pr_body #f) (format "Wave summary: ~a" summary))
                                "--head"
                                head-branch
                                "--base"
                                "main"
                                "--json"
                                "number,title,url"))
              (unless (= ec-pr 0)
                (raise-user-error 'gh-wave-finish "PR creation failed: ~a" (string-trim err-pr)))
              (define created (open-pr-from-lookup out-pr))
              (and created (hash-ref created 'number #f)))))
      (write-wave-checkpoint! state-path wave-id step)
      (make-success-result
       (list (hasheq 'type
                     "text"
                     'text
                     (format "Wave finished: ~a (issue #~a) - PR #~a" summary issue-num pr-num))))])))

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
