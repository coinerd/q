#lang racket/base
;; STABILITY: internal

;; Cohesive external-domain contracts for the GSD extension.
;;
;; This is intentionally not a generic effect map. Each value names one real
;; external domain and exposes only operations currently required by GSD.
;; GitHub has no current in-process GSD owner, so W0 inventories it with count
;; zero; W4 will add its adapter from actual correlated command semantics.
;; Events retain the existing `(symbol? hash? -> void?)` session-sink shape
;; rather than gaining a redundant one-method struct.
;;
;; Neutral-boundary rule: this module imports contracts only. Concrete Racket,
;; sandbox, process, filesystem, and event-bus adapters belong in
;; system-adapters.rkt.

(require racket/contract)

(provide gsd-external-domains
         gsd-port-domain-counts
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
          (struct gsd-effect-ports
                  ((filesystem gsd-filesystem-port?) (git gsd-git-port?)
                                                     (clock gsd-clock-port?)
                                                     (process gsd-process-port?)
                                                     (event-sink (-> symbol? hash? void?))))))

(define gsd-external-domains '(filesystem git github clock process event))

;; At most one cohesive port per external domain. GitHub is intentionally zero
;; until W4. Event uses the existing sink callback shape and counts as its one
;; boundary without introducing another wrapper abstraction.
(define gsd-port-domain-counts
  #hasheq((filesystem . 1) (git . 1) (github . 0) (clock . 1) (process . 1) (event . 1)))

(struct gsd-process-result (exit-code stdout stderr) #:transparent)
(struct gsd-filesystem-port
        (kind read-bytes write-bytes! rename! delete! mkdir! list acquire-lock release-lock!)
  #:transparent)
(struct gsd-git-port (find-root head-summary) #:transparent)
(struct gsd-clock-port (seconds milliseconds) #:transparent)
(struct gsd-process-port (run stop-worker!) #:transparent)
(struct gsd-effect-ports (filesystem git clock process event-sink) #:transparent)
