#lang racket/base

;; q/scripts/run-tests/shard-plan.rkt — duration-aware CI shard planning
;;
;; Static round-robin sharding (three `fast` shards, two workflow shards)
;; ignores measured per-file duration, so shard wall clocks are unbalanced and
;; the slowest shard sets CI latency. This module is a deterministic
;; longest-processing-time-first (LPT) bin-packing planner:
;;
;;   - Input:  the post-selection test-file inventory plus per-file durations
;;             from retained CI JSON artifacts (W0 schema: a hash with
;;             `files: [{path, duration_seconds, ...}]`).
;;   - Missing durations use a conservative default (p95 of the known suite
;;             durations) and every substitution is recorded in plan metadata.
;;   - Objective: minimize the predicted slowest shard; stable tie-break by
;;             path makes plans reproducible from identical inputs.
;;   - Constraints: every file stays atomic, mutation-sensitive files remain
;;             whole units (each shard serializes them ahead — those execution
;;             semantics live in the runner and are unchanged here), optional
;;             co-location/anti-co-location constraints are honored when
;;             declared; unsatisfiable constraints are recorded, never fatal.
;;   - Fallback: any planner error yields a round-robin plan with a logged
;;             reason — the planner can always fall back.
;;
;; Execution semantics are NOT changed by this module: it only decides WHICH
;; files each shard runs (the runner's subprocess execution and serial-ahead
;; behavior are untouched).
;;
;; SCOPE (§11.3): the `report`/`measure` experiment modes — the starvation/tail
;; checks, cohort-anchored plan regeneration, plan JSON/rendering and the
;; `--ordering` (fifo/lpt) experiment — are deleted. What remains is the
;; deterministic plan the CI lanes consume via `--shard-plan active`.
;; STABILITY: internal — consumed by run-tests/runner.rkt.

(require racket/list
         racket/file
         racket/match
         racket/math
         racket/path
         racket/string
         json
         (only-in "classify.rkt" shard-files))

(provide shard-plan?
         shard-plan-shards
         shard-plan-predicted
         shard-plan-mode
         shard-plan-reason
         shard-plan-substituted
         shard-plan-known-count
         shard-plan-default-seconds
         shard-plan-total-files
         build-shard-plan
         build-shard-plan/safe
         plan-shard-files
         plan-predicted-max
         inventory-preserved?
         load-duration-snapshot
         artifact-json-files)

;; ---------------------------------------------------------------------------
;; Plan representation
;; ---------------------------------------------------------------------------

(struct shard-plan
        (shards ; (listof (listof path-string)), index 0 .. total-1
         predicted ; (listof seconds), one per shard
         mode ; 'duration-aware | 'round-robin-fallback
         reason ; #f | string (fallback / constraint-violation note)
         duration-source ; #f | string (as passed via --durations)
         known-count ; files with a measured duration
         substituted ; sorted list of files that used the default duration
         default-seconds ; conservative default (p95 of known durations)
         total-files ; inventory size (inventory preservation check)
         inventory ; sorted full inventory (round-robin comparison)
         weights)) ; hash file -> effective seconds (measured or default)

;; ---------------------------------------------------------------------------
;; Minimal set helpers (avoid racket/set dependency surface)
;; ---------------------------------------------------------------------------

(define (list->set lst)
  (for/hash ([x (in-list lst)])
    (values x #t)))
(define (set-member? st k)
  (hash-ref st k #f))

;; ---------------------------------------------------------------------------
;; Duration snapshots (W0 schema artifacts)
;; ---------------------------------------------------------------------------

;; A retained CI JSON artifact is a hash with a `files` array whose entries
;; carry `path` (or `file`) and `duration_seconds`. Accepts a single file or a
;; directory of *.json artifacts; when several artifacts report the same file
;; the MAXIMUM observed duration wins (conservative — a slower re-run must not
;; under-estimate a shard).
(define (artifact-json-files path-string)
  (define src
    (if (path? path-string)
        (path->string path-string)
        path-string))
  (cond
    [(not src) '()]
    [(directory-exists? src)
     (map path->string
          (sort (filter (lambda (p) (and (file-exists? p) (equal? (path-get-extension p) #".json")))
                        (directory-list (string->path src) #:build? #t))
                string<?
                #:key path->string))]
    [(file-exists? src) (list (simple-form-path (string->path src)))]
    [else '()]))

(define (entry-duration entry)
  (define d (hash-ref entry 'duration_seconds #f))
  (and (real? d) (>= d 0) (exact->inexact d)))

;; A retained fast-runtime census artifact (schema
;; `q.census.fast-runtime/1`, v1.00.28 W0/W7 measurement waves) carries
;; `records` with `path` + `median_ms` instead of `files` with
;; `duration_seconds`. Map each record to duration_seconds = median_ms/1000
;; so the census — the LATEST retained per-file duration evidence feeding the
;; v1.00.28-final cohort decision — can drive the duration model directly.
;; Returns #f when the artifact is not a census.
(define (census-file-entries parsed)
  (define records (hash-ref parsed 'records #f))
  (and (list? records)
       (for/list ([e (in-list records)]
                  #:when (hash? e))
         (define ms (hash-ref e 'median_ms #f))
         (define f (hash-ref e 'path #f))
         (and (string? f)
              (real? ms)
              (>= ms 0)
              (hash 'path f 'duration_seconds (exact->inexact (/ ms 1000.0)))))))

(define (load-duration-snapshot source)
  ;; → (values durations status)
  ;; durations: hash path-string -> seconds (max across artifacts)
  ;; status: 'disabled (no source) | 'missing (source has no artifacts) |
  ;;         'corrupt (some artifact unreadable/bad shape) | 'ok
  (define files (artifact-json-files source))
  (cond
    [(null? files) (values (hash) (if source 'missing 'disabled))]
    [else
     (define corrupt? (box #f))
     (define acc
       (for/fold ([acc (hash)]) ([p (in-list files)])
         (define parsed
           (with-handlers ([exn:fail? (lambda (_)
                                        (set-box! corrupt? #t)
                                        #f)])
             (call-with-input-file p read-json)))
         (cond
           [(not (hash? parsed))
            (set-box! corrupt? #t)
            acc]
           [else
            (define entries (hash-ref parsed 'files #f))
            (cond
              [(list? entries)
               (for/fold ([acc* acc])
                         ([e (in-list entries)]
                          #:when (hash? e))
                 (define f (or (hash-ref e 'path #f) (hash-ref e 'file #f)))
                 (define d (entry-duration e))
                 (if (and (string? f) d)
                     (hash-set acc* f (max d (hash-ref acc* f 0.0)))
                     acc*))]
              ;; census schema fallback (records/path/median_ms)
              [else
               (define census-entries (census-file-entries parsed))
               (cond
                 [(list? census-entries)
                  (for/fold ([acc* acc])
                            ([e (in-list census-entries)]
                             #:when (hash? e))
                    (define f (hash-ref e 'path #f))
                    (define d (entry-duration e))
                    (if (and (string? f) d)
                        (hash-set acc* f (max d (hash-ref acc* f 0.0)))
                        acc*))]
                 [else
                  (set-box! corrupt? #t)
                  acc])])])))
     (values acc
             (cond
               [(unbox corrupt?) 'corrupt]
               [(hash-empty? acc) 'missing]
               [else 'ok]))]))

;; p95 of the known durations (conservative default for unmeasured files).
;; With no measurements at all, use a positive constant: a 0.0 default would
;; make every unit weightless, and equal weights must still spread evenly
;; across shards (LPT tie-breaks on unit count) rather than stacking on
;; shard 0 — the degenerate all-unmeasured case is exactly round-robin.
(define DEFAULT-UNMEASURED-SECONDS 1.0)
(define (snapshot-default-seconds durations)
  (define known (sort (hash-values durations) <))
  (cond
    [(null? known) DEFAULT-UNMEASURED-SECONDS]
    [else
     (define n (length known))
     (define idx (min (sub1 n) (exact-floor (* 0.95 (sub1 n)))))
     (exact->inexact (list-ref known idx))]))

;; ---------------------------------------------------------------------------
;; Packing units (files stay atomic; co-location groups fuse)
;; ---------------------------------------------------------------------------

;; A unit is one scheduling atom: a singleton file or a declared co-location
;; group (restricted to files present in the inventory). Files are never
;; split, so mutation-sensitive files always stay whole within one shard (the
;; runner serializes them ahead of that shard's parallel batches).
(struct unit (files weight) #:transparent)

(define (unit<? a b)
  ;; Total order for stable LPT: heavier first, then lexicographically
  ;; smallest representative path — deterministic from identical inputs.
  (cond
    [(> (unit-weight a) (unit-weight b)) #t]
    [(< (unit-weight a) (unit-weight b)) #f]
    [else (string<? (car (unit-files a)) (car (unit-files b)))]))

;; ---------------------------------------------------------------------------
;; Planner
;; ---------------------------------------------------------------------------

(define (validate-shard-total! who shard-total)
  (unless (and (integer? shard-total) (> shard-total 0))
    (raise-argument-error who "positive integer" shard-total)))

(define (round-robin-plan files
                          shard-total
                          #:durations [durations #f]
                          #:profile-skips? [profile-skips? (lambda (f) #f)]
                          #:duration-source [duration-source #f]
                          #:reason [reason #f])
  (validate-shard-total! 'build-shard-plan shard-total)
  (define sorted-files (sort (remove-duplicates files) string<?))
  (define-values (dur known-count)
    (if (hash? durations)
        (values durations (hash-count durations))
        (values (hash) 0)))
  (define default-seconds (snapshot-default-seconds dur))
  (define weights
    (for/hash ([f (in-list sorted-files)])
      (values f
              (if (profile-skips? f)
                  0.0
                  (hash-ref dur f default-seconds)))))
  (define shards
    (for/list ([i (in-range shard-total)])
      (shard-files sorted-files i shard-total)))
  (define predicted
    (for/list ([shard (in-list shards)])
      (for/sum ([f (in-list shard)]) (hash-ref weights f))))
  (shard-plan shards
              predicted
              'round-robin-fallback
              reason
              duration-source
              known-count
              (for/list ([f (in-list sorted-files)]
                         #:unless (hash-ref dur f #f))
                f)
              default-seconds
              (length sorted-files)
              sorted-files
              weights))

(define (build-shard-plan files
                          shard-total
                          #:durations [durations #f]
                          #:profile-skips? [profile-skips? (lambda (f) #f)]
                          #:co-locate [co-locate '()]
                          #:separate [separate '()]
                          #:duration-source [duration-source #f])
  ;; Pure function of (files, shard-total, durations, constraints): identical
  ;; inputs → identical plan (determinism guard covered by unit tests).
  (validate-shard-total! 'build-shard-plan shard-total)
  (define sorted-files (sort (remove-duplicates files) string<?))
  (define-values (dur known-count)
    (if (hash? durations)
        (values durations (hash-count durations))
        (values (hash) 0)))
  (define default-seconds (snapshot-default-seconds dur))
  (define substituted
    (for/list ([f (in-list sorted-files)]
               #:unless (hash-ref dur f #f))
      f))
  (define (effective-weight f)
    (if (profile-skips? f)
        0.0
        (hash-ref dur f default-seconds)))
  (define weights
    (for/hash ([f (in-list sorted-files)])
      (values f (effective-weight f))))
  ;; Co-location groups: fuse present members into one unit.
  (define present (list->set sorted-files))
  (define (in-inventory? group)
    (filter (lambda (f) (set-member? present f)) group))
  (define grouped-set (list->set (apply append (map in-inventory? co-locate))))
  (define (mk-unit fs)
    (unit (sort (remove-duplicates fs) string<?) (for/sum ([f (in-list fs)]) (effective-weight f))))
  (define units
    (sort (append (for/list ([g (in-list co-locate)]
                             #:when (pair? (in-inventory? g)))
                    (mk-unit (in-inventory? g)))
                  (for/list ([f (in-list sorted-files)]
                             #:unless (set-member? grouped-set f))
                    (mk-unit (list f))))
          unit<?))
  ;; Anti-co-location: file -> set of files that must not share its shard.
  (define forbidden
    (for/fold ([acc (hash)]) ([pair (in-list separate)])
      (match pair
        [(list a b)
         (hash-set (hash-set acc a (hash-set (hash-ref acc a (hash)) b #t))
                   b
                   (hash-set (hash-ref acc b (hash)) a #t))]
        [_ acc])))
  (define loads (make-vector shard-total 0.0))
  (define counts (make-vector shard-total 0))
  (define contents (make-vector shard-total '()))
  (define violations '())
  (for ([u (in-list units)])
    (define ufiles (unit-files u))
    ;; Candidate shards ordered by (load, unit-count, index): LPT picks the
    ;; least-loaded shard; when loads are equal (e.g. all-unmeasured files
    ;; sharing the default duration) the unit count breaks the tie so units
    ;; spread evenly instead of stacking on shard 0; index is the final
    ;; stable tie-break, keeping plans reproducible.
    (define candidate-order
      (sort (for/list ([i (in-range shard-total)])
              i)
            (lambda (i j)
              (or (< (vector-ref loads i) (vector-ref loads j))
                  (and (= (vector-ref loads i) (vector-ref loads j))
                       (or (< (vector-ref counts i) (vector-ref counts j))
                           (and (= (vector-ref counts i) (vector-ref counts j)) (< i j))))))))
    (define (permitted? i)
      (for/and ([f (in-list ufiles)])
        (for/and ([g (in-list (hash-keys (hash-ref forbidden f (hash))))])
          (not (member g (vector-ref contents i))))))
    (define choice
      (or (for/first ([i (in-list candidate-order)]
                      #:when (permitted? i))
            i)
          ;; unsatisfiable: force the least-loaded shard (lowest index on
          ;; ties) and record the violation — constraints never fail the run.
          (let ([forced (for/fold ([best 0]) ([i (in-range 1 shard-total)])
                          (if (< (vector-ref loads i) (vector-ref loads best)) i best))])
            (set! violations
                  (cons (format "separate-constraint unsatisfiable for ~a" (string-join ufiles ", "))
                        violations))
            forced)))
    (vector-set! contents choice (append ufiles (vector-ref contents choice)))
    (vector-set! counts choice (+ (vector-ref counts choice) 1))
    (vector-set! loads choice (+ (vector-ref loads choice) (unit-weight u))))
  (define shards
    (for/list ([i (in-range shard-total)])
      (sort (vector-ref contents i) string<?)))
  (define predicted
    (for/list ([shard (in-list shards)])
      (for/sum ([f (in-list shard)]) (hash-ref weights f))))
  (shard-plan shards
              predicted
              'duration-aware
              (and (pair? violations) (string-join (reverse violations) "; "))
              duration-source
              known-count
              substituted
              default-seconds
              (length sorted-files)
              sorted-files
              weights))

;; The planner can always fall back: any error during duration-aware planning
;; degrades to a round-robin plan with the reason logged (stderr) and recorded
;; in the plan metadata.
(define (build-shard-plan/safe files
                               shard-total
                               #:durations [durations #f]
                               #:profile-skips? [profile-skips? (lambda (f) #f)]
                               #:co-locate [co-locate '()]
                               #:separate [separate '()]
                               #:duration-source [duration-source #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf ";; run-tests: shard-plan fallback → round-robin: ~a~n"
                                        (exn-message e))
                               (round-robin-plan
                                files
                                (if (and (integer? shard-total) (> shard-total 0)) shard-total 1)
                                #:durations durations
                                #:profile-skips? profile-skips?
                                #:duration-source duration-source
                                #:reason (exn-message e)))])
    (build-shard-plan files
                      shard-total
                      #:durations durations
                      #:profile-skips? profile-skips?
                      #:co-locate co-locate
                      #:separate separate
                      #:duration-source duration-source)))

;; ---------------------------------------------------------------------------
;; Derived values used by the runner's `--shard-plan active` path
;; ---------------------------------------------------------------------------

(define (plan-shard-files plan index)
  (list-ref (shard-plan-shards plan) index))

(define (plan-predicted-max plan)
  (apply max (cons 0.0 (shard-plan-predicted plan))))

(define (inventory-preserved? plan)
  (and (= (shard-plan-total-files plan) (apply + (map length (shard-plan-shards plan))))
       ;; every inventory file appears exactly once across shards
       (equal? (shard-plan-inventory plan) (sort (apply append (shard-plan-shards plan)) string<?))))
