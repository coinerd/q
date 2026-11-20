#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-architecture-graph-helpers.rkt — CL0 graph analysis tests (#9977)
;;
;; Covers scripts/architecture-graph-helpers.rkt and the
;; scripts/architecture-graph-analysis.rkt CLI over a synthetic snapshot:
;;   1. Test modules are excluded from the production graph
;;   2. Tarjan SCC detects a two-module cycle and a self-loop
;;   3. Transitive closure excludes the node itself and is deterministic
;;   4. Cross-subsystem edges aggregate by first path segment with direction
;;   5. Ranked listings are total-ordered (closure desc, direct desc, path asc)
;;   6. Markdown rendering is byte-identical under input reordering
;;   7. CLI end-to-end on a temp snapshot (raw + markdown outputs)

(require rackunit
         racket/file
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/architecture-graph-helpers.rkt")

(define-runtime-path graph-cli "../scripts/architecture-graph-analysis.rkt")

(define (entry-ref entry key)
  (define found (assoc key entry))
  (if found
      (second found)
      #f))

;; Synthetic production modules: agent/a <-> agent/b form a cycle,
;; agent/a -> agent/c -> util/d crosses the agent/util subsystem boundary;
;; util/self has a self-loop. One test module is wired to agent/a and must be
;; excluded from the production graph.
(define (module-record path deps)
  `((path ,path) (loc 10)
                 (provide-specs 1)
                 (dependency-fan-in 0)
                 (dependency-fan-out ,(length deps))
                 (dependencies ,deps)
                 (changed-commits 0)
                 (hotspot-score 0)))

(define snapshot
  `((revision "test-revision") (modules ,(list (module-record "agent/a.rkt"
                                                              '("agent/b.rkt" "agent/c.rkt"))
                                               (module-record "agent/b.rkt" '("agent/a.rkt"))
                                               (module-record "agent/c.rkt" '("util/d.rkt"))
                                               (module-record "util/d.rkt" '())
                                               (module-record "util/self.rkt" '("util/self.rkt"))
                                               (module-record "tests/test-a.rkt" '("agent/a.rkt"))))))

(define analysis (analyze-module-graph snapshot #:top 5))
(define graph (module-deps (production-modules (snapshot-modules snapshot))))

(module+ test
  (test-case "test modules are excluded from the production graph"
    (check-equal? (entry-ref analysis 'production-module-count) 5)
    (check-false (hash-has-key? graph "tests/test-a.rkt"))
    (check-equal? (hash-count graph) 5))

  (test-case "Tarjan SCC finds the cycle and the self-loop"
    (check-equal? (entry-ref analysis 'cycle-count) 2)
    (check-equal? (entry-ref analysis 'cycles) '(("agent/a.rkt" "agent/b.rkt") ("util/self.rkt"))))

  (test-case "SCC pop stops at the root when an ancestor is still on the stack"
    ;; p -> r -> s: s pops alone, then r pops alone, then p pops alone. The
    ;; component pop must never drain nodes below its own root (regression for
    ;; a whole-stack drain bug that merged p and r into one component).
    (define nested (hash "p.rkt" '("r.rkt") "r.rkt" '("s.rkt") "s.rkt" '()))
    (check-equal? (tarjan-scc nested) '(("p.rkt") ("r.rkt") ("s.rkt"))))

  (test-case "transitive closure excludes the node itself and reaches descendants"
    (check-equal? (hash-ref (transitive-closure graph) "agent/a.rkt")
                  '("agent/b.rkt" "agent/c.rkt" "util/d.rkt"))
    ;; self-loop node: the closure excludes itself once but keeps nothing beyond
    (check-equal? (hash-ref (transitive-closure graph) "util/self.rkt") '())
    (check-equal? (hash-ref (transitive-closure graph) "agent/c.rkt") '("util/d.rkt")))

  (test-case "cross-subsystem edges aggregate with direction and counts"
    (check-equal? (cross-subsystem-edges graph) '(("agent" "util" 1))))

  (test-case "subsystem summary counts internal and cross edges"
    (check-equal? (subsystem-summary graph) '(("agent" 3 3 1 0) ("util" 2 1 0 1))))

  (test-case "closure dependents rank by transitive count then direct then path"
    (define dependents (entry-ref analysis 'closure-dependents))
    (check-equal? (first dependents) '("util/d.rkt" 3 1))
    (for ([left dependents]
          [right (rest dependents)])
      (define acceptable
        (or (> (second left) (second right))
            (and (= (second left) (second right))
                 (or (> (third left) (third right))
                     (and (= (third left) (third right)) (string<? (first left) (first right)))))))
      (check-true acceptable (format "order violated: ~a before ~a" left right))))

  (test-case "markdown rendering is byte-identical under input reordering"
    (define reordered (list (first snapshot) (list 'modules (reverse (second (second snapshot))))))
    (check-equal? (graph-analysis->markdown analysis)
                  (graph-analysis->markdown (analyze-module-graph reordered #:top 5)))
    (check-true (string-contains? (graph-analysis->markdown analysis) "agent/b.rkt")))

  (test-case "CLI end-to-end writes raw and markdown evidence"
    (define racket-path (find-executable-path "racket"))
    (check-not-false racket-path)
    (define tmp-dir (make-temporary-file "cl0-graph-~a" 'directory))
    (dynamic-wind (lambda () (void))
                  (lambda ()
                    (define snapshot-path (build-path tmp-dir "snapshot.rktd"))
                    (define raw-path (build-path tmp-dir "graph.rktd"))
                    (define markdown-path (build-path tmp-dir "graph.md"))
                    (call-with-output-file snapshot-path
                                           (lambda (out) (writeln snapshot out))
                                           #:exists 'truncate/replace)
                    (define stdout (open-output-string))
                    (define stderr (open-output-string))
                    (define ok?
                      (parameterize ([current-output-port stdout]
                                     [current-error-port stderr])
                        (system* racket-path
                                 (path->string graph-cli)
                                 "--snapshot"
                                 (path->string snapshot-path)
                                 "--raw"
                                 (path->string raw-path)
                                 "--markdown"
                                 (path->string markdown-path))))
                    (unless ok?
                      (error 'test-graph-cli "command failed: ~a" (get-output-string stderr)))
                    (define raw (call-with-input-file raw-path read))
                    (check-equal? (entry-ref raw 'production-module-count) 5)
                    (check-equal? (entry-ref raw 'revision) "test-revision")
                    (check-true (file-exists? markdown-path))
                    (check-true (non-empty-string? (file->string markdown-path))))
                  (lambda () (delete-directory/files tmp-dir)))))
