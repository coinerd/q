#lang racket/base

;; extensions/github/handlers/issue-ops.rkt — GitHub issue tool handler

(require racket/format
         racket/string
         json
         (only-in "../helpers.rkt"
                  with-error-result
                  gh-binary
                  gh-unavailable-error
                  valid-number?
                  valid-state?
                  gh-success
                  gh-success-json
                  gh-exec-result)
         (only-in "../../tool-api.rkt" make-error-result make-success-result))

(provide handle-gh-issue)

(define (handle-gh-issue args [exec-ctx #f])
  (with-error-result
   "github operation"
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
