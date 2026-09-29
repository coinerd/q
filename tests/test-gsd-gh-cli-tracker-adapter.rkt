#lang racket/base
;; @covers extensions/gsd/gh-cli-tracker-adapter.rkt
;; @speed fast
;; @suite gsd
(require rackunit
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

(test-case "project item edit requires a matching authenticated response"
  (define params
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
  (define seen #f)
  (define (runner args)
    (set! seen args)
    (values 0 "{\"id\":\"PVTI_item\"}" ""))
  (define adapter (make-gh-cli-tracker-adapter #:live? #t #:repository "coinerd/q" #:runner runner))
  ((github-adapter-set-board-field! adapter) params)
  (check-equal? seen
                '("project" "item-edit"
                            "--id"
                            "PVTI_item"
                            "--project-id"
                            "PVT_project"
                            "--field-id"
                            "PVTSSF_status"
                            "--single-select-option-id"
                            "done123"
                            "--format"
                            "json"))
  (define bad
    (make-gh-cli-tracker-adapter #:live? #t
                                 #:repository "coinerd/q"
                                 #:runner (lambda (_) (values 0 "{\"id\":\"other\"}" ""))))
  (check-exn exn:fail? (lambda () ((github-adapter-set-board-field! bad) params))))
