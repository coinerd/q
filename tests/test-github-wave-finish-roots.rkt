#lang racket

;; @speed fast
;; @suite default

(require rackunit
         racket/file
         racket/path
         racket/string
         "../extensions/github-integration.rkt"
         "../extensions/github/helpers.rkt"
         "../extensions/tool-api.rkt")

(define (result-text result)
  (string-join (for/list ([item (in-list (tool-result-content result))]
                          #:when (hash? item))
                 (hash-ref item 'text ""))))

(define (with-split-layout proc)
  (define outer (make-temporary-file "github-wave-roots-~a" 'directory))
  (define repo (build-path outer "q"))
  (define canonical-planning (build-path outer ".planning"))
  (define tracked-planning (build-path repo ".planning"))
  (make-directory* (build-path repo ".git"))
  (make-directory* canonical-planning)
  (make-directory* tracked-planning)
  (display-to-file "# Canonical state\n" (build-path canonical-planning "STATE.md"))
  (display-to-file "# Tracked state\n" (build-path tracked-planning "STATE.md"))
  (dynamic-wind void
                (lambda () (proc outer repo canonical-planning tracked-planning))
                (lambda () (delete-directory/files outer))))

(test-case "wave finish is quarantined before git, GitHub, or planning mutation"
  (with-split-layout
   (lambda (outer _repo canonical-planning tracked-planning)
     (define git-calls (box '()))
     (define gh-calls (box '()))
     (parameterize ([gh-binary-path (string->path "/bin/true")]
                    [current-git-exec-result (lambda args
                                               (set-box! git-calls (cons args (unbox git-calls)))
                                               (values 0 "" ""))]
                    [current-gh-exec-result (lambda args
                                              (set-box! gh-calls (cons args (unbox gh-calls)))
                                              (values 0 "" ""))])
       (define result
         (handle-gh-wave-finish
          (hasheq 'issue_number 42 'files '("allowed.rkt") 'commit_msg "Safe commit")
          (make-exec-context #:working-directory outer)))
       (check-true (tool-result-is-error? result))
       (check-true (string-contains? (result-text result) "quarantined")))
     (check-equal? (unbox git-calls) '())
     (check-equal? (unbox gh-calls) '())
     (check-equal? (file->string (build-path canonical-planning "STATE.md")) "# Canonical state\n")
     (check-equal? (file->string (build-path tracked-planning "STATE.md")) "# Tracked state\n"))))

(test-case "wave start fails closed when pull fails and runs from resolved repo"
  (with-split-layout
   (lambda (outer repo _canonical-planning _tracked-planning)
     (define calls (box '()))
     (define (fake-git . args)
       (set-box! calls (cons (cons (simplify-path (current-directory)) args) (unbox calls)))
       (if (equal? args '("pull" "origin" "main"))
           (values 1 "" "network unavailable")
           (values 0 "" "")))
     (parameterize ([gh-binary-path (string->path "/bin/true")]
                    [current-git-exec-result fake-git])
       (define result
         (handle-gh-wave-start (hasheq 'issue_number 42)
                               (make-exec-context #:working-directory outer)))
       (check-true (tool-result-is-error? result))
       (check-true (string-contains? (result-text result) "pull")))
     (check-false (member '("checkout" "-b" "feature/issue-42-wave") (map cdr (unbox calls))))
     (check-true (for/and ([call (in-list (unbox calls))])
                   (equal? (explode-path (car call)) (explode-path (simplify-path repo))))))))
