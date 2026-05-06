#lang racket/base

;; extensions/github/tool-handlers.rkt — GitHub tool handlers and registration
;;
;; Thin facade: imports handlers from github/handlers/*, keeps wave
;; start/finish and tool/command registration inline.

(require racket/format
         racket/string
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

(define (handle-gh-wave-finish args [exec-ctx #f])
  (with-error-result
   "github operation"
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
         (define-values (ec-commit _out-commit err-commit) (git-exec-result "commit" "-m" commit-msg))
         (cond
           [(not (= ec-commit 0))
            (make-error-result (format "git commit failed: ~a" (string-trim err-commit)))]
           [else
            ;; Step 2: detect branch, push
            (define-values (ec-br out-br _) (git-exec-result "branch" "--show-current"))
            (define branch
              (or (hash-ref args 'branch #f) (and (= ec-br 0) (string-trim out-br)) "main"))
            (define-values (ec-push _out-push err-push) (git-exec-result "push" "-u" "origin" branch))
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
