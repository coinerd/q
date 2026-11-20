#lang racket/base

;; scripts/architecture-graph-helpers.rkt — pure graph analysis over an
;; architecture-baseline snapshot (the raw .rktd emitted by
;; scripts/architecture-baseline.rkt).
;;
;; CL0 (#9977): adds the graph views the baseline snapshot does not compute —
;; strongly connected components (Tarjan), transitive dependency closure,
;; closure-weighted hubs/dependents and cross-subsystem edge aggregation.
;; This module is a pure CONSUMER of the snapshot datum: it never reads Git,
;; source files or the dependency policy, so the canonical baseline contract
;; is unchanged. The subsystem of a module is its first path segment
;; ("root" for top-level files); CL1 canonical subsystem maps supersede this
;; coarse partition without changing the numbers recorded here.
;;
;; Snapshot edges are STATIC internal requires only (see the baseline
;; helpers): dynamic requires and macro-expanded bindings are unresolved and
;; must be reported as such, never inferred.
;;
;; All outputs are deterministically ordered: sorts are total (keys descend
;; numerically, then paths ascend) and hash iteration never leaks into
;; rendering.

(require racket/list
         racket/string)

(provide snapshot-modules
         production-modules
         module-deps
         subsystem-of
         tarjan-scc
         transitive-closure
         cross-subsystem-edges
         subsystem-summary
         analyze-module-graph
         graph-analysis->markdown)

;; ---- snapshot access ---------------------------------------------------------

(define (entry-ref entry key)
  (define found (assoc key entry))
  (if found
      (second found)
      #f))

;; snapshot datum -> list of records (path deps loc changed-commits provide-specs)
(define (snapshot-modules snapshot)
  (for/list ([module (in-list (entry-ref snapshot 'modules))])
    (list (entry-ref module 'path)
          (entry-ref module 'dependencies)
          (entry-ref module 'loc)
          (entry-ref module 'changed-commits)
          (entry-ref module 'provide-specs))))

;; module records -> production (non-test) records, sorted by path
(define (production-modules modules)
  (sort (for/list ([record (in-list modules)]
                   #:unless (test-module-path? (first record)))
          record)
        string<?
        #:key first))

(define (test-module-path? path)
  (or (string-prefix? path "tests/") (string-contains? path "/tests/")))

;; module records -> immutable hash path -> (sorted dependency paths)
(define (module-deps modules)
  (for/hash ([record (in-list modules)])
    (values (first record) (second record))))

;; path -> subsystem label (first path segment; "root" for top-level files)
(define (subsystem-of path)
  (define parts (string-split path "/"))
  (if (= (length parts) 1)
      "root"
      (first parts)))

;; ---- graph algorithms ----------------------------------------------------------

;; graph (hash path -> deps) -> list of SCCs, each a sorted list of paths,
;; ordered by first member path. Singleton components without a self-loop are
;; included so callers can reason about component totals deterministically.
(define (tarjan-scc graph)
  (define index (make-hash))
  (define lowlink (make-hash))
  (define on-stack (make-hash))
  (define stack '())
  (define components '())
  (define counter (box 0))
  (define (strongconnect node)
    (hash-set! index node (unbox counter))
    (hash-set! lowlink node (unbox counter))
    (set-box! counter (add1 (unbox counter)))
    (set! stack (cons node stack))
    (hash-set! on-stack node #t)
    (for ([successor (in-list (hash-ref graph node '()))])
      (cond
        [(not (hash-has-key? index successor))
         (strongconnect successor)
         (hash-set! lowlink node (min (hash-ref lowlink node) (hash-ref lowlink successor)))]
        [(hash-ref on-stack successor)
         (hash-set! lowlink node (min (hash-ref lowlink node) (hash-ref index successor)))]))
    (when (= (hash-ref lowlink node) (hash-ref index node))
      ;; Pop exactly the component: stop at the root, never drain the whole
      ;; stack (nodes below the root belong to an ancestor's pending SCC).
      (define component
        (let loop ([members '()])
          (define member (car stack))
          (set! stack (cdr stack))
          (hash-set! on-stack member #f)
          (define members* (cons member members))
          (if (equal? member node)
              members*
              (loop members*))))
      (set! components (cons (sort component string<?) components))))
  (for ([node (in-list (sort (hash-keys graph) string<?))]
        #:unless (hash-has-key? index node))
    (strongconnect node))
  (sort (reverse components) string<? #:key first))

;; graph -> immutable hash path -> sorted transitively reachable paths
;; (excluding the node itself unless it participates in a cycle).
(define (transitive-closure graph)
  (define cache (make-hash))
  (define (reachable node)
    (cond
      [(hash-has-key? cache node) (hash-ref cache node)]
      [else
       (define seen (make-hash))
       (let walk ([current node])
         (for ([successor (in-list (hash-ref graph current '()))])
           (unless (or (hash-has-key? seen successor) (string=? successor node))
             (hash-set! seen successor #t)
             (walk successor))))
       (define sorted (sort (hash-keys seen) string<?))
       (hash-set! cache node sorted)
       sorted]))
  (for/hash ([node (in-list (sort (hash-keys graph) string<?))])
    (values node (reachable node))))

;; ---- cross-subsystem aggregation -------------------------------------------------

;; graph -> sorted directed cross-subsystem edges (from-subsystem to-subsystem count)
(define (cross-subsystem-edges graph)
  (define pairs (make-hash))
  (for ([from (in-list (sort (hash-keys graph) string<?))])
    (define from-subsys (subsystem-of from))
    (for ([to (in-list (hash-ref graph from '()))])
      (define to-subsys (subsystem-of to))
      (unless (string=? from-subsys to-subsys)
        (define key (cons from-subsys to-subsys))
        (hash-set! pairs key (add1 (hash-ref pairs key 0))))))
  (sort (for/list ([(key count) (in-hash pairs)])
          (list (car key) (cdr key) count))
        string<?
        #:key (lambda (item) (string-append (first item) "/" (second item)))))

;; graph -> sorted records (subsystem modules internal-edges outbound inbound)
(define (subsystem-summary graph)
  (define members (make-hash))
  (for ([node (in-list (hash-keys graph))])
    (hash-set! members (subsystem-of node) (add1 (hash-ref members (subsystem-of node) 0))))
  (define internal (make-hash))
  (define outbound (make-hash))
  (define inbound (make-hash))
  (for ([from (in-list (hash-keys graph))])
    (define from-subsys (subsystem-of from))
    (for ([to (in-list (hash-ref graph from '()))])
      (define to-subsys (subsystem-of to))
      (if (string=? from-subsys to-subsys)
          (hash-set! internal from-subsys (add1 (hash-ref internal from-subsys 0)))
          (begin
            (hash-set! outbound from-subsys (add1 (hash-ref outbound from-subsys 0)))
            (hash-set! inbound to-subsys (add1 (hash-ref inbound to-subsys 0)))))))
  (sort (for/list ([(subsys count) (in-hash members)])
          (list subsys
                count
                (hash-ref internal subsys 0)
                (hash-ref outbound subsys 0)
                (hash-ref inbound subsys 0)))
        string<?
        #:key first))

;; ---- hub ranking ------------------------------------------------------------------

;; records are (path closure-count direct-count); order: closure desc,
;; direct desc, path asc — a total order independent of hash iteration.
(define (rank-records records)
  (sort records
        (lambda (a b)
          (or (> (second a) (second b))
              (and (= (second a) (second b))
                   (or (> (third a) (third b))
                       (and (= (third a) (third b)) (string<? (first a) (first b)))))))))

(define (closure-hub-records graph closure)
  (for/list ([node (in-list (sort (hash-keys graph) string<?))])
    (list node (length (hash-ref closure node '())) (length (hash-ref graph node '())))))

(define (closure-dependent-records graph closure)
  (define direct (make-hash))
  (for ([from (in-list (hash-keys graph))])
    (for ([to (in-list (hash-ref graph from '()))])
      (hash-set! direct to (add1 (hash-ref direct to 0)))))
  (define transitive (make-hash))
  (for ([(node reachable) (in-hash closure)])
    (for ([target (in-list reachable)])
      (hash-set! transitive target (add1 (hash-ref transitive target 0)))))
  (for/list ([target (in-list (sort (hash-keys transitive) string<?))])
    (list target (hash-ref transitive target 0) (hash-ref direct target 0))))

(define (take-top records top)
  (take records (min top (length records))))

;; ---- full analysis -----------------------------------------------------------------

;; snapshot #:top -> analysis datum (all lists deterministically ordered):
;; ((revision ...) (production-module-count n) (production-edge-count n)
;;  (cycle-count n) (cycles (...)) (closure-hubs (...))
;;  (closure-dependents (...)) (cross-subsystem-edges (...))
;;  (subsystem-summary (...)))
(define (analyze-module-graph snapshot #:top [top 25])
  (define modules (production-modules (snapshot-modules snapshot)))
  (define graph (module-deps modules))
  (define closure (transitive-closure graph))
  (define cycles
    (filter (lambda (component)
              (or (> (length component) 1)
                  (member (first component) (hash-ref graph (first component) '()))))
            (tarjan-scc graph)))
  (define edge-count
    (for/sum ([node (in-list (hash-keys graph))]) (length (hash-ref graph node '()))))
  `((revision ,(entry-ref snapshot 'revision))
    (production-module-count ,(length modules))
    (production-edge-count ,edge-count)
    (cycle-count ,(length cycles))
    (cycles ,cycles)
    (closure-hubs ,(take-top (rank-records (closure-hub-records graph closure)) top))
    (closure-dependents ,(take-top (rank-records (closure-dependent-records graph closure)) top))
    (cross-subsystem-edges
     ,(take-top (sort (cross-subsystem-edges graph) (lambda (a b) (> (third a) (third b)))) top))
    (subsystem-summary ,(subsystem-summary graph))))

;; ---- markdown rendering -------------------------------------------------------------

(define (format-table-row cells)
  (string-append "| " (string-join (map (lambda (cell) (format "~a" cell)) cells) " | ") " |"))

(define (markdown-table headers rows)
  (append (list (format-table-row headers)
                (format-table-row (map (lambda (header) (make-string (string-length header) #\-))
                                       headers)))
          (map format-table-row rows)))

(define (graph-analysis->markdown analysis)
  (define revision (entry-ref analysis 'revision))
  (define lines
    (append (list "# Architecture Graph Analysis"
                  ""
                  (format "Pinned revision: `~a`" revision)
                  ""
                  "Derived from the architecture-baseline raw snapshot; static internal"
                  "requires only — dynamic requires and macro-expanded bindings are"
                  "unresolved by construction and are reported separately, never inferred."
                  ""
                  "## Summary"
                  ""
                  (format "- production-modules: ~a" (entry-ref analysis 'production-module-count))
                  (format "- production-edges: ~a" (entry-ref analysis 'production-edge-count))
                  (format "- dependency-cycles: ~a" (entry-ref analysis 'cycle-count))
                  ""
                  "## Dependency cycles"
                  "")
            (if (zero? (entry-ref analysis 'cycle-count))
                (list "None detected in static requires." "")
                (append (markdown-table '("Size" "Members")
                                        (for/list ([cycle (in-list (entry-ref analysis 'cycles))])
                                          (list (length cycle) (string-join cycle ", "))))
                        (list "")))
            (list "## Closure hubs (largest transitive fan-out)" "")
            (markdown-table '("Path" "Transitive reach" "Direct fan-out")
                            (for/list ([hub (in-list (entry-ref analysis 'closure-hubs))])
                              (list (format "`~a`" (first hub)) (second hub) (third hub))))
            (list "" "## Closure dependents (largest transitive fan-in)" "")
            (markdown-table '("Path" "Transitive dependents" "Direct fan-in")
                            (for/list ([dep (in-list (entry-ref analysis 'closure-dependents))])
                              (list (format "`~a`" (first dep)) (second dep) (third dep))))
            (list "" "## Top cross-subsystem edges" "")
            (markdown-table '("From subsystem" "To subsystem" "Edges")
                            (for/list ([edge (in-list (entry-ref analysis 'cross-subsystem-edges))])
                              (list (first edge) (second edge) (third edge))))
            (list "" "## Subsystem summary" "")
            (markdown-table '("Subsystem" "Modules" "Internal edges" "Outbound" "Inbound")
                            (for/list ([row (in-list (entry-ref analysis 'subsystem-summary))])
                              (list (first row) (second row) (third row) (fourth row) (fifth row))))
            (list "")))
  (string-join (map (lambda (line) (string-append line "\n")) lines) ""))
