#lang racket/base
;; In-repository Racket GitHub adapter. `gh` is the authenticated transport;
;; no Python helper, token handling or shell interpolation is involved.
;; Construction requires explicit live authority; production callers must
;; supply the exact repository and project identifiers from trusted bindings.
(require json
         racket/string
         "github-port.rkt"
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-exit-code
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-timed-out?
                  subprocess-result-truncated?))
(provide make-gh-cli-tracker-adapter)

(define repo-rx #px"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
(define id-rx #px"^[A-Za-z0-9_=-]+$")
(define (positive-issue! n)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'gh-cli-tracker-adapter "exact-positive-integer?" n)))
(define (safe-id! v key)
  (unless (and (string? v) (regexp-match? id-rx v))
    (raise-arguments-error 'gh-cli-tracker-adapter "missing/invalid project identifier" key v))
  v)
(define (default-runner args)
  (define result (run-subprocess "gh" #:args args #:timeout 30))
  (values (if (or (subprocess-result-timed-out? result) (subprocess-result-truncated? result))
              -1
              (subprocess-result-exit-code result))
          (subprocess-result-stdout result)
          (subprocess-result-stderr result)))
(define (checked-call runner args)
  (define-values (code output _stderr) (runner args))
  (unless (and (exact-integer? code) (zero? code))
    (error 'gh-cli-tracker-adapter "GitHub command failed or timed out"))
  output)
(define (json-response output)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (string->jsexpr output)))

;; Resolve the item through GitHub's GraphQL API rather than trusting the
;; operator-supplied opaque ID alone. This is a READ before any write and a
;; second read after the edit, bound to the exact issue, project, field and
;; selected option. The same read also returns the bound field's option
;; list, so a stale or mistyped single-select option id can be refused
;; before any mutation. `gh` credentials stay in its own credential store.
(define item-query
  (string-append "query($id:ID!){node(id:$id){... on ProjectV2Item{"
                 "id project{id} content{... on Issue{number repository{nameWithOwner}}}"
                 " fieldValueByName(name:\"Status\"){... on ProjectV2ItemFieldSingleSelectValue{"
                 "optionId field{... on ProjectV2SingleSelectField{id options{id}}}}}}}"))
(define (nested-ref h keys)
  (for/fold ([value h]) ([key (in-list keys)])
    (and (hash? value) (hash-ref value key #f))))
;; The option ids carried by the bound Status single-select field in a
;; readback, or #f when the response does not expose them.
(define (field-option-ids result)
  (define options (nested-ref result '(data node fieldValueByName field options)))
  (and (list? options)
       (for/list ([option (in-list options)]
                  #:when (hash? option))
         (hash-ref option 'id #f))))
;; A readback proves the item, project, repository, issue number and field
;; identity. `expected-option` additionally proves the *resulting* option
;; after a mutation; `required-option` proves that the operator-supplied
;; option id is a member of the field's option list *before* any mutation,
;; failing closed (including when the option list cannot be read).
(define (verify-item! runner item project field repo issue [expected-option #f] [required-option #f])
  (define result
    (json-response (checked-call runner
                                 (list "api"
                                       "graphql"
                                       "-f"
                                       (string-append "query=" item-query)
                                       "-F"
                                       (string-append "id=" item)))))
  (unless (and (hash? result)
               (not (hash-ref result 'errors #f))
               (equal? (nested-ref result '(data node id)) item)
               (equal? (nested-ref result '(data node project id)) project)
               (equal? (nested-ref result '(data node content number)) issue)
               (equal? (nested-ref result '(data node content repository nameWithOwner)) repo)
               (equal? (nested-ref result '(data node fieldValueByName field id)) field)
               (or (not expected-option)
                   (equal? (nested-ref result '(data node fieldValueByName optionId))
                           expected-option)))
    (error 'gh-cli-tracker-adapter "project item identity/status readback not verified"))
  (when required-option
    (define option-ids (field-option-ids result))
    (unless (and option-ids (member required-option option-ids))
      (error 'gh-cli-tracker-adapter
             "configured option-id is not a member of the bound field's option list"))))

(define (make-gh-cli-tracker-adapter #:live? [live? #f]
                                     #:repository repository
                                     #:runner [runner default-runner])
  (unless (eq? live? #t)
    (raise-arguments-error 'make-gh-cli-tracker-adapter
                           "explicit live authorization required"
                           "live?"
                           live?))
  (unless (and (string? repository)
               (regexp-match? repo-rx repository)
               (not (regexp-match? #px"\\.\\." repository)))
    (raise-argument-error 'make-gh-cli-tracker-adapter "owner/repository" repository))
  (unless (procedure? runner)
    (raise-argument-error 'make-gh-cli-tracker-adapter "procedure?" runner))
  (define (unsupported _)
    (error 'gh-cli-tracker-adapter "unsupported GitHub operation"))
  (github-adapter
   unsupported
   (lambda (n)
     (positive-issue! n)
     (define response
       (json-response (checked-call runner
                                    (list "api"
                                          (format "repos/~a/issues/~a" repository n)
                                          "--method"
                                          "PATCH"
                                          "-f"
                                          "state=closed"))))
     (unless (and (hash? response)
                  (equal? (hash-ref response 'number #f) n)
                  (equal? (hash-ref response 'state #f) "closed"))
       (error 'gh-cli-tracker-adapter "issue close response not verified")))
   (lambda (params)
     (unless (hash? params)
       (raise-argument-error 'gh-cli-tracker-adapter "hash?" params))
     (positive-issue! (hash-ref params 'issue-number #f))
     ;; These opaque IDs must come from an authenticated project binding, not
     ;; inferred from a label such as "Status" or a milestone number.
     (define item (safe-id! (hash-ref params 'project-item-id #f) "project-item-id"))
     (define project (safe-id! (hash-ref params 'project-id #f) "project-id"))
     (define field (safe-id! (hash-ref params 'field-id #f) "field-id"))
     (define option (safe-id! (hash-ref params 'option-id #f) "option-id"))
     (unless (and (equal? (hash-ref params 'field #f) "Status")
                  (equal? (hash-ref params 'value #f) "Done"))
       (error 'gh-cli-tracker-adapter "only Status=Done is supported"))
     ;; The option id is proven to belong to the bound field on this same
     ;; read, so a stale or mistyped id is refused before any write.
     (verify-item! runner item project field repository (hash-ref params 'issue-number) #f option)
     (define response
       (json-response (checked-call runner
                                    (list "project"
                                          "item-edit"
                                          "--id"
                                          item
                                          "--project-id"
                                          project
                                          "--field-id"
                                          field
                                          "--single-select-option-id"
                                          option
                                          "--format"
                                          "json"))))
     (unless (and (hash? response) (equal? (hash-ref response 'id #f) item))
       (error 'gh-cli-tracker-adapter "project item update response not verified"))
     (verify-item! runner item project field repository (hash-ref params 'issue-number) option))
   unsupported
   unsupported
   (lambda (_) #f)
   (lambda (_) #f)
   (lambda (_) #f)
   (lambda (_) #f)))
