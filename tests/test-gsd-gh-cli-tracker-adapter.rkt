#lang racket/base
;; @covers extensions/gsd/gh-cli-tracker-adapter.rkt
;; @speed fast
;; @suite gsd
(require rackunit
         racket/list
         racket/string
         "../extensions/gsd/gh-cli-tracker-adapter.rkt"
         "../extensions/gsd/github-port.rkt")

(test-case "the Racket adapter is inert without explicit live authorization"
  (check-exn exn:fail? (lambda () (make-gh-cli-tracker-adapter #:repository "coinerd/q"))))

(test-case "repository identity and issue number are validated before invoking gh"
  (define calls '())
  (define (runner args)
    (set! calls (cons args calls))
    (values 0 "{}" ""))
  (check-exn exn:fail?
             (lambda ()
               (make-gh-cli-tracker-adapter #:live? #t #:repository "../other" #:runner runner)))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  (check-exn exn:fail? (lambda () ((github-adapter-close-issue! adapter) -1)))
  (check-equal? calls '()))

(test-case "closing an issue uses gh api with an argument vector and checks its result"
  (define calls '())
  (define (runner args)
    (set! calls (cons args calls))
    (values 0 "{\"number\":9763,\"state\":\"closed\"}" ""))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  ((github-adapter-close-issue! adapter) 9763)
  (check-equal? calls
                '(("api" "repos/coinerd/q/issues/9763" "--method" "PATCH" "-f" "state=closed"))))

(test-case "HTTP failure, missing response, and wrong issue fail closed"
  (for ([response (in-list (list (list 1 "" "forbidden")
                                 (list 0 "" "")
                                 (list 0 "{\"number\":99,\"state\":\"closed\"}" "")))])
    (define (runner _args)
      (apply values response))
    (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
    (check-exn exn:fail? (lambda () ((github-adapter-close-issue! adapter) 9763)))))

(test-case "board mutation refuses absent project item and option IDs"
  (define calls 0)
  (define (runner _args)
    (set! calls (add1 calls))
    (values 0 "{}" ""))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  (check-exn exn:fail?
             (lambda ()
               ((github-adapter-set-board-field! adapter)
                (hasheq 'issue-number 9763 'field "Status" 'value "Done"))))
  (check-equal? calls 0))

(define board-params
  (hasheq 'issue-number
          9763
          'field
          "Status"
          'value
          "Done"
          'project-item-id
          "PVTI_item"
          'project-id
          "PVT_project"
          'field-id
          "PVTSSF_status"
          'option-id
          "done123"))
(define (item-proof number option)
  (format (string-append "{\"data\":{\"node\":{\"id\":\"PVTI_item\","
                         "\"project\":{\"id\":\"PVT_project\"},"
                         "\"content\":{\"number\":~a,"
                         "\"repository\":{\"nameWithOwner\":\"coinerd/q\"}},"
                         "\"fieldValueByName\":{\"optionId\":\"~a\","
                         "\"field\":{\"id\":\"PVTSSF_status\"}}}}}")
          number
          option))

(test-case "board update checks issue/project identity before mutation and option afterward"
  (define calls '())
  (define (runner args)
    (set! calls (append calls (list args)))
    (cond
      [(equal? (take args 2) '("api" "graphql"))
       (values 0 (item-proof 9763 (if (= (length calls) 1) "inbox" "done123")) "")]
      [else (values 0 "{\"id\":\"PVTI_item\"}" "")]))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  ((github-adapter-set-board-field! adapter) board-params)
  (check-equal? (map (lambda (args) (take args 2)) calls)
                '(("api" "graphql") ("project" "item-edit") ("api" "graphql"))))

(test-case "wrong project item issue, repo, or project blocks before mutation"
  (for ([proof (in-list (list (item-proof 9764 "inbox")
                              (string-replace (item-proof 9763 "inbox") "coinerd/q" "other/repo")
                              (string-replace (item-proof 9763 "inbox") "PVT_project" "PVT_wrong")))])
    (define calls '())
    (define (runner args)
      (set! calls (append calls (list args)))
      (values 0 proof ""))
    (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
    (check-exn exn:fail? (lambda () ((github-adapter-set-board-field! adapter) board-params)))
    (check-equal? (length calls) 1)))

(test-case "wrong post-update option fails closed"
  (define calls 0)
  (define (runner args)
    (set! calls (add1 calls))
    (values 0
            (if (equal? (car args) "project")
                "{\"id\":\"PVTI_item\"}"
                (item-proof 9763 "inbox"))
            ""))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  (check-exn exn:fail? (lambda () ((github-adapter-set-board-field! adapter) board-params)))
  (check-equal? calls 3))
