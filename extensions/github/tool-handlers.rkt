#lang racket/base

;; extensions/github/tool-handlers.rkt — GitHub tool handlers and registration
;;
;; Thin facade: imports handlers from github/handlers/*, keeps wave
;; start/finish and tool/command registration inline.

(require racket/format
         racket/list
         racket/path
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
                  resolve-git-root
                  resolve-planning-root
                  valid-number?
                  with-error-result)
         "tool-schemas.rkt"
         (rename-in "handlers/issue-ops.rkt" [handle-gh-issue raw-handle-gh-issue])
         (rename-in "handlers/pr-ops.rkt" [handle-gh-pr raw-handle-gh-pr])
         (rename-in "handlers/milestone-ops.rkt"
                    [handle-gh-milestone raw-handle-gh-milestone]
                    [handle-gh-board raw-handle-gh-board]))

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

(define (execution-cwd exec-ctx)
  (or (and exec-ctx (exec-context? exec-ctx) (exec-context-working-directory exec-ctx))
      (current-directory)))

(define (resolved-repo-root exec-ctx)
  (or (resolve-git-root (execution-cwd exec-ctx))
      (raise-user-error 'github-wave "Cannot resolve a Git repository from execution cwd")))

(define (non-empty-text? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (positive-integer? value)
  (and (integer? value) (positive? value)))

(define (call-in-resolved-root handler args exec-ctx)
  (with-error-result "github operation"
                     (parameterize ([current-directory (resolved-repo-root exec-ctx)])
                       (handler args exec-ctx))))

(define (handle-gh-issue args [exec-ctx #f])
  (call-in-resolved-root raw-handle-gh-issue args exec-ctx))

(define (handle-gh-pr args [exec-ctx #f])
  (call-in-resolved-root raw-handle-gh-pr args exec-ctx))

(define (handle-gh-milestone args [exec-ctx #f])
  (call-in-resolved-root raw-handle-gh-milestone args exec-ctx))

(define (handle-gh-board args [exec-ctx #f])
  (call-in-resolved-root raw-handle-gh-board args exec-ctx))

(define (handle-gh-wave-start args [exec-ctx #f])
  (with-error-result
   "github operation"
   (define issue-num (hash-ref args 'issue_number #f))
   (define requested-branch (hash-ref args 'branch_name #f))
   (cond
     [(not (valid-number? issue-num)) (make-error-result "Invalid required argument: issue_number")]
     [(and requested-branch (not (non-empty-text? requested-branch)))
      (make-error-result "Invalid argument: branch_name")]
     [(not (gh-binary)) (gh-unavailable-error)]
     [else
      (define repo-root (resolved-repo-root exec-ctx))
      (define branch-name (or requested-branch (format "feature/issue-~a-wave" issue-num)))
      (parameterize ([current-directory repo-root])
        (define-values (ec-co _out-co err-co) (git-exec-result "checkout" "main"))
        (unless (= ec-co 0)
          (raise-user-error 'gh-wave-start "Cannot checkout main: ~a" (string-trim err-co)))
        (define-values (ec-pull _out-pull err-pull) (git-exec-result "pull" "origin" "main"))
        (unless (= ec-pull 0)
          (raise-user-error 'gh-wave-start "Cannot pull origin/main: ~a" (string-trim err-pull)))
        (define-values (ec-branch _out-br err-br) (git-exec-result "checkout" "-b" branch-name))
        (unless (= ec-branch 0)
          (raise-user-error 'gh-wave-start
                            "Failed to create branch ~a: ~a"
                            branch-name
                            (string-trim err-br)))
        (make-success-result (list (hasheq 'type
                                           "text"
                                           'text
                                           (format "Wave started: branch '~a' created for issue #~a"
                                                   branch-name
                                                   issue-num)))))])))

(define (safe-wave-file? value)
  (and (non-empty-text? value)
       (not (string-contains? value "\n"))
       (not (string-contains? value "\r"))
       (not (string-contains? value "\u0000"))
       (let ([path (string->path value)])
         (and (not (absolute-path? path))
              (for/and ([component (in-list (explode-path path))])
                (not (memq component '(up same))))))))

(define (valid-wave-files? files)
  (and (list? files)
       (pair? files)
       (andmap safe-wave-file? files)
       (= (length files) (length (remove-duplicates files string=?)))))

(define (explicit-state-path raw cwd)
  (parameterize ([current-directory cwd])
    (simplify-path (path->complete-path raw))))

(define (created-pr-number stdout)
  (define parsed
    (with-handlers ([exn:fail? (lambda (_)
                                 (raise-user-error 'gh-wave-finish
                                                   "PR creation returned invalid JSON"))])
      (string->jsexpr (string-trim stdout))))
  (define number (and (hash? parsed) (hash-ref parsed 'number #f)))
  (unless (valid-number? number)
    (raise-user-error 'gh-wave-finish "PR creation response did not contain a valid PR number"))
  number)

(define (handle-gh-wave-finish args [exec-ctx #f])
  ;; Quarantined by policy: in-product finalization cannot safely prove
  ;; protected-branch merge, issue closure, and planning synchronization.
  ;; Validate the public contract, then fail before any filesystem, git, or
  ;; GitHub mutation. The external authenticated PR workflow remains the sole
  ;; finalization authority.
  (define allowed-keys '(issue_number files commit_msg))
  (define issue-num (hash-ref args 'issue_number #f))
  (define files (hash-ref args 'files #f))
  (define commit-msg (hash-ref args 'commit_msg #f))
  (cond
    [(not (for/and ([key (in-hash-keys args)])
            (memq key allowed-keys)))
     (make-error-result "Invalid argument: unknown property for quarantined gh-wave-finish")]
    [(not (positive-integer? issue-num))
     (make-error-result "Invalid required argument: issue_number must be a positive integer")]
    [(not (valid-wave-files? files))
     (make-error-result
      "Invalid required argument: files must be a non-empty list of safe relative paths")]
    [(not (non-empty-text? commit-msg)) (make-error-result "Invalid required argument: commit_msg")]
    [else
     (make-error-result
      "gh-wave-finish is quarantined: use the external authenticated PR workflow; no repository or GitHub mutation was performed")]))

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
  (ext-register-tool!
   ctx
   "gh-wave-finish"
   (string-append "Quarantined: validates required issue_number, files, and commit_msg, "
                  "then always fails before mutation. Use the external authenticated PR workflow.")
   gh-wave-finish-schema
   handle-gh-wave-finish
   #:prompt-guidelines
   "gh-wave-finish is quarantined and always fails before mutation after validating required arguments; use the external authenticated PR workflow.")
  (hook-pass #f))

(define (register-github-commands ctx)
  (ext-register-command! ctx "/milestone" "Quick milestone status" 'general '() '("ms"))
  (ext-register-command! ctx "/issue" "Quick issue info" 'general '() '("i"))
  (ext-register-command! ctx "/pr" "Quick PR status" 'general '())
  (hook-pass #f))
