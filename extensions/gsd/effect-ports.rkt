#lang racket/base
;; STABILITY: internal

;; Cohesive external-domain contracts for the GSD extension.
;;
;; This is intentionally not a generic effect map. Each value names one real
;; external domain and exposes only operations currently required by GSD.
;; GitHub (v0.99.90 W4) is modeled as idempotent correlated commands: every
;; command carries a correlation ID and the port keeps a journal of
;; correlation IDs already executed, so retries never create a duplicate
;; external effect (issue/merge/release). The port defaults to dry-run and
;; the adapter is injected; live GitHub is reachable only through an explicit
;; approved smoke (never from standard tests).
;; Events retain the existing `(symbol? hash? -> void?)` session-sink shape
;; rather than gaining a redundant one-method struct.
;;
;; Neutral-boundary rule: this module imports contracts only. Concrete Racket,
;; sandbox, process, filesystem, and event-bus adapters belong in
;; system-adapters.rkt; GitHub idempotency/redaction logic belongs in
;; github-port.rkt.

(require racket/contract)

(provide gsd-external-domains
         gsd-port-domain-counts
         gsd-github-command
         gsd-github-command?
         gsd-github-command-kind
         gsd-github-command-correlation-id
         gsd-github-command-params
         gsd-github-command-expected-sha
         gsd-github-command-result
         gsd-github-command-result?
         gsd-github-command-result-correlation-id
         gsd-github-command-result-kind
         gsd-github-command-result-external-id
         gsd-github-command-result-dry-run?
         gsd-github-command-result-already-done?
         gsd-github-command-result-note
         (contract-out
          (struct gsd-process-result ((exit-code exact-integer?) (stdout bytes?) (stderr bytes?)))
          (struct gsd-filesystem-port
                  ((kind (-> path-string? (or/c #f 'file 'directory 'link)))
                   (read-bytes (-> path-string? bytes?))
                   (write-bytes! (-> path-string? bytes? void?))
                   (rename! (-> path-string? path-string? void?))
                   (delete! (-> path-string? void?))
                   (mkdir! (-> path-string? void?))
                   (list (-> path-string? (listof path?)))
                   (acquire-lock (-> path-string? any/c))
                   (release-lock! (-> path-string? any/c void?))))
          (struct gsd-git-port
                  ((find-root (-> path-string? (or/c path-string? #f)))
                   (head-summary (-> path-string? (listof string?) string?))))
          (struct gsd-clock-port ((seconds (-> exact-integer?)) (milliseconds (-> real?))))
          (struct gsd-process-port
                  ((run (-> string? (listof string?) path-string? gsd-process-result?))
                   (stop-worker! (-> void?))))
          (struct gsd-github-port
                  ((execute (-> gsd-github-command? gsd-github-command-result?))
                   (dry-run? (-> boolean?))
                   (journal (-> (listof string?)))))
          (struct gsd-effect-ports
                  ((filesystem gsd-filesystem-port?) (git gsd-git-port?)
                                                     (clock gsd-clock-port?)
                                                     (process gsd-process-port?)
                                                     (github gsd-github-port?)
                                                     (event-sink (-> symbol? hash? void?))))))

(define gsd-external-domains '(filesystem git github clock process event))

(define gsd-github-command-kinds '(issue-create issue-close board-set-field pr-merge release-create))

(struct gsd-github-command (kind correlation-id params expected-sha)
  #:transparent
  #:guard
  (lambda (kind correlation-id params expected-sha name)
    (unless (memq kind gsd-github-command-kinds)
      (raise-arguments-error
       name
       "invalid github command kind (expected issue-create|issue-close|board-set-field|pr-merge|release-create)"
       "kind"
       kind))
    (unless (and (string? correlation-id) (positive? (string-length correlation-id)))
      (raise-arguments-error name
                             "correlation-id must be a nonempty string"
                             "correlation-id"
                             correlation-id))
    (unless (hash? params)
      (raise-arguments-error name "params must be a hash" "params" params))
    (unless (or (not expected-sha) (string? expected-sha))
      (raise-arguments-error name "expected-sha must be a string or #f" "expected-sha" expected-sha))
    (values kind correlation-id params expected-sha)))

(struct gsd-github-command-result (correlation-id kind external-id dry-run? already-done? note)
  #:transparent)

;; At most one cohesive port per external domain. GitHub (W4) counts as its
;; one boundary (gsd-github-port). Event uses the existing sink callback
;; shape and counts as its one boundary without another wrapper abstraction.
(define gsd-port-domain-counts
  #hasheq((filesystem . 1) (git . 1) (github . 1) (clock . 1) (process . 1) (event . 1)))

(struct gsd-process-result (exit-code stdout stderr) #:transparent)
(struct gsd-filesystem-port
        (kind read-bytes write-bytes! rename! delete! mkdir! list acquire-lock release-lock!)
  #:transparent)
(struct gsd-git-port (find-root head-summary) #:transparent)
(struct gsd-clock-port (seconds milliseconds) #:transparent)
(struct gsd-process-port (run stop-worker!) #:transparent)
(struct gsd-github-port (execute dry-run? journal) #:transparent)
(struct gsd-effect-ports (filesystem git clock process github event-sink) #:transparent)
