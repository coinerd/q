#lang racket/base

;; extensions/github/handlers/pr-ops.rkt — GitHub PR tool handler
;;
;; Extracted from tool-handlers.rkt. Pure handler logic for gh-pr actions.

(require racket/format
         racket/string
         (only-in "../helpers.rkt"
                  gh-binary
                  gh-unavailable-error
                  valid-number?
                  valid-method?
                  valid-state?
                  gh-success-json)
         (only-in "../../tool-api.rkt" make-error-result))

(provide handle-gh-pr)

(define (handle-gh-pr args [exec-ctx #f])
  (with-handlers ([exn:fail? (lambda (e) (make-error-result (exn-message e)))])
    (cond
      [(not (gh-binary)) (gh-unavailable-error)]
      [else
       (define action (hash-ref args 'action ""))
       (cond
         [(string=? action "") (make-error-result "Missing required argument: action")]
         [(string=? action "create")
          (define title (hash-ref args 'title #f))
          (cond
            [(not title) (make-error-result "create requires 'title'")]
            [else
             (define body (hash-ref args 'body ""))
             (define head (hash-ref args 'head #f))
             (define base (hash-ref args 'base "main"))
             (apply gh-success-json
                    (append (list "pr" "create" "--title" title "--body" body "--base" base "--json" "number,title,url")
                            (if head (list "--head" head) '())))])]
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
                            (if commit-title (list "--subject" commit-title) '())))])]
         [(string=? action "list")
          (define raw-state (hash-ref args 'state "open"))
          (unless (valid-state? raw-state)
            (raise-user-error 'github-pr "invalid state: ~a" raw-state))
          (gh-success-json "pr" "list" "--state" raw-state "--limit" "100"
                           "--json" "number,title,state,headRefName")]
         [(string=? action "get")
          (define num (hash-ref args 'number #f))
          (cond
            [(not num) (make-error-result "get requires 'number'")]
            [(not (valid-number? num)) (make-error-result (format "invalid number: ~a" num))]
            [else
             (gh-success-json "pr" "view" (~a num) "--json" "number,title,state,headRefName,baseRefName,url")])]
         [else
          (make-error-result (format "Unknown action: ~a. Valid: create, merge, list, get" action))])])))
