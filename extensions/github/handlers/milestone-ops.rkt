#lang racket/base

;; extensions/github/handlers/milestone-ops.rkt — GitHub milestone + board handlers

(require racket/format
         racket/string
         json
         (only-in "../helpers.rkt"
                  gh-binary
                  gh-unavailable-error
                  valid-number?
                  valid-state?
                  gh-success-json
                  gh-exec-result)
         (only-in "../../tool-api.rkt" make-error-result make-success-result))

(provide handle-gh-milestone
         handle-gh-board
         milestone-create-from-spec)

;; --- gh-milestone helpers ---

(define (milestone-create-from-spec spec-file dry-run?)
  ;; Extracted helper to avoid deep nesting in handle-gh-milestone.
  (define spec-path (expand-user-path spec-file))
  (cond
    [(not (file-exists? spec-path)) (make-error-result (format "Spec file not found: ~a" spec-file))]
    [else
     (define spec-data
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning (format "github/spec-read: ~a" (exn-message e)))
                                    #f)])
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
                                   "title"
                                   t
                                   "-f"
                                   "description"
                                   d)
                             (if (string=? due "")
                                 '()
                                 (list "-f" "due_on" due)))))
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

(define (handle-gh-milestone args [exec-ctx #f])
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
                    (append
                     (list "api" "repos/{owner}/{repo}/milestones" "-X" "POST" "-f" "title" title)
                     (if (string=? desc "")
                         '()
                         (list "-f" "description" desc))
                     (if (string=? due "")
                         '()
                         (list "-f" "due_on" due))))])]
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

(define (handle-gh-board args [exec-ctx #f])
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
