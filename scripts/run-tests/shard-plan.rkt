#lang racket/base

;; q/scripts/run-tests/shard-plan.rkt — duration-aware CI shard planning (W7)
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
;; files each shard runs (the runner's subprocess/grouped and serial-ahead
;; behavior is untouched).
;; STABILITY: internal — consumed by run-tests/runner.rkt.

(require racket/list
         racket/file
         racket/format
         racket/function
         racket/hash
         racket/match
         racket/math
         racket/path
         racket/string
         openssl
         (only-in file/sha1 bytes->hex-string)
         json
         (only-in "classify.rkt" shard-files))

(provide shard-plan?
         shard-plan-shards
         shard-plan-predicted
         shard-plan-mode
         shard-plan-substituted
         shard-plan-total-files
         build-shard-plan
         build-shard-plan/safe
         plan->jsexpr
         plan->assignments
         plan-shard-files
         plan-predicted-max
         round-robin-predicted-max
         print-shard-plan-report
         write-plan-json!
         load-duration-snapshot
         artifact-json-files
         inventory-preserved?
         activation-recommendation
         ;; W10 (v1.00.29): starvation/tail checks + cohort-anchored plan
         ;; regeneration + the --shard-plan measure duration model.
         plan-mean-shard-seconds
         starvation-check
         tail-straddle-check
         quantile-interpolated
         load-cohort-wall-anchors
         regenerate-plan-report
         print-starvation-tail-report
         starvation-max-ratio
         tail-straddle-limit-ratio
         ;; Phase C fold: deterministic file ordering (formerly
         ;; scheduler-order.rkt; v1.00.23 W3).
         ordering-record
         ordering-record-mode
         ordering-record-requested
         ordering-record-fallback-reason
         ordering-record-snapshot-checksum
         ordering-record-snapshot-status
         ordering-record-snapshot-files
         ordering-record-freshness-decision
         ordering-record-used-durations
         default-ordering
         default-max-age-seconds
         current-max-age-seconds
         known-orderings
         prepare-ordering
         order-files
         ordering-record->jsexpr)

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

;; W10: a retained fast-runtime census artifact (schema
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
              ;; W10: census schema fallback (records/path/median_ms)
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
;; Accessors / derived values
;; ---------------------------------------------------------------------------

(define (plan->assignments plan)
  (shard-plan-shards plan))

(define (plan-shard-files plan index)
  (list-ref (shard-plan-shards plan) index))

(define (plan-predicted-max plan)
  (apply max (cons 0.0 (shard-plan-predicted plan))))

(define (round-robin-predicted-max plan)
  (define total (length (shard-plan-shards plan)))
  (define inv (shard-plan-inventory plan))
  (define w (shard-plan-weights plan))
  (apply max
         (cons 0.0
               (for/list ([i (in-range total)])
                 (for/sum ([f (in-list (shard-files inv i total))]) (hash-ref w f))))))

(define (inventory-preserved? plan)
  (and (= (shard-plan-total-files plan) (apply + (map length (shard-plan-shards plan))))
       ;; every inventory file appears exactly once across shards
       (equal? (shard-plan-inventory plan) (sort (apply append (shard-plan-shards plan)) string<?))))

(define (activation-recommendation plan)
  (cond
    [(not (inventory-preserved? plan)) (cons "hold" "inventory not preserved")]
    [(eq? (shard-plan-mode plan) 'round-robin-fallback)
     (cons "hold" "planner fell back to round-robin")]
    [(<= (round-robin-predicted-max plan) (plan-predicted-max plan))
     (cons "hold" "no predicted improvement over round-robin")]
    [else (cons "activate" "predicted max-shard duration improves")]))

(define (plan->jsexpr plan)
  (hasheq 'schema
          "shard-plan/1"
          'mode
          (symbol->string (shard-plan-mode plan))
          'reason
          (or (shard-plan-reason plan) 'null)
          'shard_total
          (length (shard-plan-shards plan))
          'file_count
          (shard-plan-total-files plan)
          'durations
          (hasheq 'source
                  (or (shard-plan-duration-source plan) 'null)
                  'known
                  (shard-plan-known-count plan)
                  'substituted_count
                  (length (shard-plan-substituted plan))
                  'substituted
                  (shard-plan-substituted plan)
                  'default_seconds
                  (shard-plan-default-seconds plan))
          'predicted
          (hasheq 'per_shard_seconds
                  (shard-plan-predicted plan)
                  'max_seconds
                  (plan-predicted-max plan)
                  'round_robin_max_seconds
                  (round-robin-predicted-max plan)
                  'inventory_preserved
                  (inventory-preserved? plan))
          'activation
          (let ([rec (activation-recommendation plan)])
            (hasheq 'recommendation
                    (if (symbol? (car rec))
                        (symbol->string (car rec))
                        (car rec))
                    'why
                    (cdr rec)))
          'shards
          (for/list ([shard (in-list (shard-plan-shards plan))]
                     [i (in-naturals)])
            (hasheq 'index
                    i
                    'file_count
                    (length shard)
                    'predicted_seconds
                    (list-ref (shard-plan-predicted plan) i)
                    'files
                    shard))))

(define (write-plan-json! plan path)
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (out)
                           (write-json (plan->jsexpr plan) out)
                           (newline out))))

;; ---------------------------------------------------------------------------
;; Report (report-only mode changes nothing)
;; ---------------------------------------------------------------------------

(define (print-shard-plan-report plan [port (current-output-port)])
  (define total (length (shard-plan-shards plan)))
  (fprintf port
           ";; run-tests: shard plan (~a) shards=~a files=~a~n"
           (shard-plan-mode plan)
           total
           (shard-plan-total-files plan))
  (fprintf port
           ";; durations: source=~a known=~a substituted=~a default=~as~n"
           (or (shard-plan-duration-source plan) "<none>")
           (shard-plan-known-count plan)
           (length (shard-plan-substituted plan))
           (~r (shard-plan-default-seconds plan) #:precision '(= 2)))
  (when (shard-plan-reason plan)
    (fprintf port ";; reason: ~a~n" (shard-plan-reason plan)))
  (for ([shard (in-list (shard-plan-shards plan))]
        [i (in-naturals)])
    (fprintf port
             "shard ~a/~a: ~a file~a, predicted ~as~n"
             i
             total
             (length shard)
             (if (= (length shard) 1) "" "s")
             (~r (list-ref (shard-plan-predicted plan) i) #:precision '(= 1)))
    (for ([f (in-list shard)])
      (fprintf port "  ~a~n" f)))
  (define pmax (plan-predicted-max plan))
  (define rmax (round-robin-predicted-max plan))
  (define delta (- rmax pmax))
  (fprintf port
           ";; predicted max shard: ~as (~a) vs ~as (round-robin) → ~a~as (~a%)~n"
           (~r pmax #:precision '(= 1))
           (shard-plan-mode plan)
           (~r rmax #:precision '(= 1))
           (if (>= delta 0) "-" "+")
           (~r (abs delta) #:precision '(= 1))
           (if (> rmax 0)
               (~r (* 100 (/ delta rmax)) #:precision '(= 1))
               "0.0"))
  (define rec (activation-recommendation plan))
  (fprintf port
           ";; activation: ~a — ~a (inventory preserved: ~a)~n"
           (car rec)
           (cdr rec)
           (inventory-preserved? plan)))

;; ---------------------------------------------------------------------------
;; W10 (v1.00.29): starvation/tail checks + cohort-anchored regeneration
;; ---------------------------------------------------------------------------
;; The wave contract (PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md §6 W8: "tune
;; shard count only if inventory is preserved ... and no starvation/tail
;; regression appears") is operationalized here with two pre-registered,
;; mechanical checks; the definitions below are FROZEN for the v1.00.29 W10
;; regeneration and travel with every generated plan report:
;;
;; starvation — no shard's predicted duration may exceed
;;   `starvation-max-ratio` (1.35) times the MEAN shard duration of the
;;   plan. A shard heavier than that starves its own files relative to the
;;   plan cadence and alone dominates the CI wall.
;;
;; tail-straddle — a file is predicted to straddle a shard-cadence boundary
;;   when its effective weight exceeds the mean shard duration (it cannot
;;   complete within one mean shard-length window). It straddles a boundary
;;   TWICE when its weight exceeds `tail-straddle-limit-ratio` (2.0) times
;;   the mean shard duration: such a file is file-bound — no rebalance of
;;   shard counts can remove the tail it creates — and is recorded as a
;;   violation. Files straddling exactly once are recorded informationally.

(define STARVATION-MAX-RATIO 1.35)
(define TAIL-STRADDLE-LIMIT-RATIO 2.0)

(define (starvation-max-ratio)
  STARVATION-MAX-RATIO)
(define (tail-straddle-limit-ratio)
  TAIL-STRADDLE-LIMIT-RATIO)

;; Mean of the plan's predicted shard durations (inexact; 0.0 for a
;; degenerate plan with no shards — never divides by zero).
(define (plan-mean-shard-seconds plan)
  (define preds (shard-plan-predicted plan))
  (if (null? preds)
      0.0
      (exact->inexact (/ (for/sum ([p (in-list preds)]) p) (length preds)))))

;; Starvation gate: every shard's predicted duration must be within the
;; ratio limit of the mean. Returns a jsexpr-able hasheq; `ok` is #f when at
;; least one shard offends.
(define (starvation-check plan #:max-ratio [max-ratio STARVATION-MAX-RATIO])
  (define mean (plan-mean-shard-seconds plan))
  (define per-shard
    (for/list ([p (in-list (shard-plan-predicted plan))]
               [i (in-naturals)])
      (hasheq 'index
              i
              'predicted_seconds
              (exact->inexact p)
              'ratio
              (if (> mean 0.0)
                  (exact->inexact (/ p mean))
                  0.0))))
  (define offending (filter (lambda (row) (> (hash-ref row 'ratio) max-ratio)) per-shard))
  (hasheq 'ok
          (null? offending)
          'max_ratio_limit
          (exact->inexact max-ratio)
          'mean_shard_seconds
          mean
          'per_shard
          per-shard
          'offending
          offending))

;; Tail/straddle gate per the frozen definition above. `violations` lists
;; files predicted to straddle a boundary twice (weight > 2×mean);
;; `straddling_once` lists files straddling exactly once (informational).
(define (tail-straddle-check plan #:limit-ratio [limit-ratio TAIL-STRADDLE-LIMIT-RATIO])
  (define mean (plan-mean-shard-seconds plan))
  (define weights (shard-plan-weights plan))
  (define shard-of
    (for/hash ([shard (in-list (shard-plan-shards plan))]
               [i (in-naturals)]
               #:when #t
               [f (in-list shard)])
      (values f i)))
  (define (classify threshold)
    (for/list ([f (in-list (sort (hash-keys weights) string<?))]
               #:when (let ([w (hash-ref weights f 0.0)])
                        (and (> mean 0.0) (> w (* threshold mean)))))
      (hasheq 'path
              f
              'weight_seconds
              (exact->inexact (hash-ref weights f 0.0))
              'shard
              (hash-ref shard-of f #f)
              'ratio_to_mean
              (if (> mean 0.0)
                  (exact->inexact (/ (hash-ref weights f 0.0) mean))
                  0.0))))
  (define violations (classify limit-ratio))
  (hasheq 'ok
          (null? violations)
          'mean_shard_seconds
          mean
          'straddle_limit_ratio
          (exact->inexact limit-ratio)
          'straddling_once
          (classify 1.0)
          'violations
          violations))

;; Linear-interpolation quantile with millisecond rounding (byte-stable
;; regeneration): k = q·(n−1) over the ascending sample; empty → #f.
(define (quantile-interpolated samples q)
  (cond
    [(null? samples) #f]
    [(not (and (real? q) (<= 0.0 q 1.0)))
     (raise-argument-error 'quantile-interpolated "quantile in [0,1]" q)]
    [else
     (define sorted (sort (map exact->inexact samples) <))
     (define n (length sorted))
     (define k (* q (sub1 n)))
     (define lo (exact-floor k))
     (define hi (min (sub1 n) (exact-ceiling k)))
     (define frac (- k lo))
     (define lo-v (list-ref sorted lo))
     (define hi-v (list-ref sorted hi))
     (define raw (+ lo-v (* frac (- hi-v lo-v))))
     ;; round to milliseconds to keep regenerated artifacts byte-stable
     (exact->inexact (/ (round (* raw 1000.0)) 1000.0))]))

;; Retained CI wall anchors from a cohort seed (the machine-readable PR list
;; the coordinator fetched from the GitHub API; JSON array of PR objects with
;; `merged_at`, `ci_wall_seconds`, ...). RFC3339 Z timestamps compare
;; correctly as strings (fixed-width UTC format in the seed), so ordering is
;; a plain string sort — deterministic with no date library.
;;
;; Frozen W10 rules:
;;  - post-W4 subset: merged_at >= post-w4 cutoff (default: the W4 prepared-env
;;    expansion merge 2026-09-12T01:00:26Z, the topology change that added the
;;    prepared-env purge+identity lanes and RAISED CI walls — disclosed, not hidden);
;;  - the three retained CI wall time anchors: the three LATEST entries by
;;    merged_at STRICTLY BEFORE the cutoff (pre-W4 topology, i.e. the wall the
;;    fast-suite shard plan actually influences).
(define DEFAULT-POST-W4-CUTOFF "2026-09-12T01:00:26Z")

;; Defensive field access: read-json yields symbol keys in this tree, but
;; accept string keys too (canonicalization variance across load paths).
(define (hget h k [default #f])
  (define v (hash-ref h k 'hget-miss))
  (cond
    [(not (eq? v 'hget-miss)) v]
    [(symbol? k) (hash-ref h (symbol->string k) default)]
    [(string? k) (hash-ref h (string->symbol k) default)]
    [else default]))

(define (load-cohort-wall-anchors seed-path #:post-w4-cutoff [cutoff DEFAULT-POST-W4-CUTOFF])
  (define entries
    (let ([j (with-handlers ([exn:fail? (lambda (_) #f)])
               (call-with-input-file seed-path read-json))])
      (cond
        [(list? j)
         (for/list ([e (in-list j)]
                    #:when
                    (and (hash? e) (real? (hget e 'ci_wall_seconds)) (string? (hget e 'merged_at))))
           e)]
        [else '()])))
  (define pre
    (sort (filter (lambda (e) (string<? (hget e 'merged_at) cutoff)) entries)
          string<?
          #:key (lambda (e) (hget e 'merged_at))))
  (define post (filter (lambda (e) (not (string<? (hget e 'merged_at) cutoff))) entries))
  (define (row e)
    (hasheq 'number
            (hget e 'number)
            'merged_at
            (hget e 'merged_at)
            'ci_wall_seconds
            (exact->inexact (hget e 'ci_wall_seconds))))
  (define (walls es)
    (map (lambda (e) (hget e 'ci_wall_seconds)) es))
  (define three-anchors
    (for/list ([e (in-list (take-right pre (min 3 (length pre))))])
      (row e)))
  (hasheq 'source
          (path->string (simple-form-path seed-path))
          'anchor_rule
          "three latest pre-W4 merged PRs by merged_at (pre-topology-change CI walls)"
          'post_w4_cutoff
          cutoff
          'entries_total
          (length entries)
          'three_anchors
          three-anchors
          'three_anchor_mean_seconds
          (if (null? three-anchors)
              'null
              (exact->inexact (/ (for/sum ([a (in-list three-anchors)]) (hget a 'ci_wall_seconds))
                                 (length three-anchors))))
          'post_w4
          (hasheq 'count
                  (length post)
                  'p50_seconds
                  (or (quantile-interpolated (walls post) 0.5) 'null)
                  'p95_seconds
                  (or (quantile-interpolated (walls post) 0.95) 'null))
          'full
          (hasheq 'count
                  (length entries)
                  'p50_seconds
                  (or (quantile-interpolated (walls entries) 0.5) 'null)
                  'p95_seconds
                  (or (quantile-interpolated (walls entries) 0.95) 'null))))

;; W10 duration-aware plan regeneration: current fast-suite inventory +
;; duration model + retained cohort anchors → one deterministic jsexpr with
;; the plan, the starvation/tail checks, and the anchor statistics. This is
;; the machine-readable form embedded in the W10 decision evidence.
;;
;; Duration model (pre-registered): per-shard predicted duration = Σ of its
;; files' per-file durations. Per-file durations come from the LATEST
;; retained v1.00.28-final cohort duration evidence (the W7 post-remediation
;; fast-runtime census referenced by the v1.00.28-final decision) via
;; `#:durations`, OR from a local `--shard-plan measure` snapshot; unmeasured
;; files fall back to the conservative p95 default and are recorded.
(define (regenerate-plan-report files
                                shard-total
                                #:durations [durations #f]
                                #:duration-source [duration-source #f]
                                #:seed-path [seed-path #f]
                                #:post-w4-cutoff [cutoff DEFAULT-POST-W4-CUTOFF])
  (define plan
    (build-shard-plan/safe files shard-total #:durations durations #:duration-source duration-source))
  (define starve (starvation-check plan))
  (define tail (tail-straddle-check plan))
  (hasheq
   'schema
   "shard-plan-regeneration/1"
   'wave
   "v1.00.29-w10"
   'spec_reference
   "PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md §6 W8 (duration-aware plan regeneration; starvation/tail checks)"
   'plan
   (plan->jsexpr plan)
   'checks
   (hasheq 'starvation starve 'tail_straddle tail)
   'checks_definitions
   (hasheq
    'starvation
    "no shard predicted > 1.35× the mean shard duration"
    'tail_straddle
    "no file predicted to straddle a shard boundary twice (weight > 2× mean shard duration; files > 1× mean recorded informationally)")
   'cohort_wall_anchors
   (if seed-path
       (load-cohort-wall-anchors seed-path #:post-w4-cutoff cutoff)
       'null)))

;; Human-readable starvation/tail report appended to --shard-plan report and
;; measure output (report-only; changes nothing).
(define (print-starvation-tail-report plan [port (current-output-port)])
  (define starve (starvation-check plan))
  (define tail (tail-straddle-check plan))
  (fprintf port
           ";; starvation: ~a (limit ≤ ~a× mean ~as; offending: ~a)~n"
           (if (hash-ref starve 'ok) "ok" "VIOLATION")
           (~r (hash-ref starve 'max_ratio_limit) #:precision '(= 2))
           (~r (hash-ref starve 'mean_shard_seconds) #:precision '(= 1))
           (length (hash-ref starve 'offending)))
  (for ([o (in-list (hash-ref starve 'offending))])
    (fprintf port
             ";;   shard ~a/~a predicted ~as (~a× mean)~n"
             (hash-ref o 'index)
             (length (shard-plan-shards plan))
             (~r (hash-ref o 'predicted_seconds) #:precision '(= 1))
             (~r (hash-ref o 'ratio) #:precision '(= 3))))
  (fprintf port
           ";; tail-straddle: ~a (limit > ~a× mean; straddle-once files: ~a)~n"
           (if (hash-ref tail 'ok) "ok" "VIOLATION")
           (~r (hash-ref tail 'straddle_limit_ratio) #:precision '(= 2))
           (length (hash-ref tail 'straddling_once)))
  (for ([v (in-list (hash-ref tail 'violations))])
    (fprintf port
             ";;   file ~a straddles twice (~as = ~a× mean, shard ~a)~n"
             (hash-ref v 'path)
             (~r (hash-ref v 'weight_seconds) #:precision '(= 1))
             (~r (hash-ref v 'ratio_to_mean) #:precision '(= 3))
             (hash-ref v 'shard))))

;; ─────────────────────────────────────────────────────────────────────
;; Phase C fold: deterministic file ordering (moved verbatim from the
;; former scheduler-order.rkt; v1.00.23 W3).  Ordering is a SEPARATE
;; contract from shard planning/selection; it reuses this module's
;; duration-snapshot parser (load-duration-snapshot) directly.
;; ─────────────────────────────────────────────────────────────────────

;;
;; Ordering is a SEPARATE contract from scheduler selection: selection decides
;; WHICH files run (and whether they run serial/parallel); ordering decides
;; the START order of the selected files.  Two modes are supported:
;;
;;   fifo  — deterministic input order (the default; preserves today's
;;           behavior exactly, including the serial-mutating-first partition
;;           which happens AFTER ordering, on the ordered sequence).
;;   lpt   — longest-processing-time first: order by the maximum retained
;;           per-file duration, descending, with stable repository-path
;;           tie-breaks.
;;
;; LPT reuses the same duration-snapshot parser/planner that shard-plan.rkt
;; already uses (load-duration-snapshot), so the artifact format is
;; byte-compatible with the planner — no new/incompatible formats are
;; introduced.
;;
;; Fail-safe contract: whenever duration evidence is unusable (missing,
;; stale, malformed, wrong-inventory, or corrupt), ordering falls back to
;; FIFO and records a NAMED reason.  Every fallback is deterministic and
;; reproducible: given the same files, the same snapshot bytes, and the same
;; current time, the same decision is produced.

(define default-ordering 'fifo)
(define known-orderings '(fifo lpt))

;; Maximum accepted snapshot age (seconds).  Evidence older than this is
;; "stale" and falls back to FIFO with a named reason.  Config compatibility:
;; Q_TEST_DURATIONS_MAX_AGE_SECONDS overrides the default per environment.
(define default-max-age-seconds 3600)
(define (current-max-age-seconds)
  (define raw (getenv "Q_TEST_DURATIONS_MAX_AGE_SECONDS"))
  (define n (and raw (string->number raw)))
  (if (and n (real? n) (> n 0))
      (exact-floor n)
      default-max-age-seconds))

;; ── ordering record ─────────────────────────────────────────────────────
;; A plain (immutable) struct; the runner embeds the decision in the results
;; JSON via ordering-record->jsexpr so ordering is auditable per run.
(struct ordering-record
  (mode            ; 'fifo | 'lpt — the mode ACTUALLY applied
   requested       ; 'fifo | 'lpt — what the user asked for
   fallback-reason ; #f | string — named reason when mode != requested
   snapshot-checksum    ; string | #f — hex sha256 of the artifact bytes read
   snapshot-status      ; 'usable | 'missing | 'stale | 'malformed | 'wrong-inventory | 'corrupt
   snapshot-files       ; fixnum | #f — artifacts examined (not duration entries)
   freshness-decision   ; string — human-readable decision summary
   used-durations)      ; hash: path-string -> max retained seconds | #f
  #:transparent)

(define (ordering-record->jsexpr rec)
  ;; Deterministic key order.  used-durations is intentionally NOT embedded
  ;; (it can be large); the decision inputs are covered by the checksum +
  ;; status + freshness fields.
  (jsexpr->string
   (hasheq 'mode (symbol->string (ordering-record-mode rec))
           'requested (symbol->string (ordering-record-requested rec))
           'fallback_reason (ordering-record-fallback-reason rec)
           'snapshot_checksum (ordering-record-snapshot-checksum rec)
           'snapshot_status
           (if (ordering-record-snapshot-status rec)
               (symbol->string (ordering-record-snapshot-status rec))
               #f)
           'snapshot_files (ordering-record-snapshot-files rec)
           'freshness_decision (ordering-record-freshness-decision rec))))

;; ── stable repository-path representation (portable across platforms) ───
(define (path->string-repr p)
  (let ([s (if (string? p) p (path->string p))])
    (if (eq? (system-type) 'windows)
        (string-replace s "\\" "/")
        s)))

;; Normalize a path key so evidence and selection spellings agree even when
;; one side is relative and the other absolute.
(define (normalize-path-key p)
  (define s (if (string? p) p (path->string p)))
  (path->string-repr
   (simplify-path (if (absolute-path? s)
                      (string->path s)
                      (path->complete-path (string->path s))))))

;; ── snapshot probing ────────────────────────────────────────────────────
;; Named statuses: 'missing, 'malformed, 'corrupt (unreadable bytes),
;; 'stale (beyond max accepted age), 'wrong-inventory (zero overlap with
;; the current file inventory), 'usable.  Each unusable status carries a
;; deterministic fallback reason.

;; Probe 1: does the source exist and what artifacts does it provide?
;; Reuses shard-plan.rkt's artifact-json-files (identical enumeration order
;; to load-duration-snapshot), so probe and parser always agree.
;; → (values readable? artifact-paths age-seconds reason)
(define (probe-snapshot-readable! source max-age-seconds)
  (define artifacts (artifact-json-files source))
  (cond
    [(null? artifacts)
     (values #f '() #f
             (if source
                 (format "duration snapshot has no readable *.json artifacts: ~a" source)
                 "no duration snapshot (--durations not provided)"))]
    [else
     (define paths (map (lambda (a) (if (path? a) a (string->path a))) artifacts))
     (define ages
       (map (lambda (p)
              (exact-floor (- (current-seconds) (file-or-directory-modify-seconds p))))
            paths))
     (values #t paths (apply min ages) #f)]))

;; Probe 2: checksum of the artifact bytes the parser actually reads.
;; Single file → that file's bytes; directory → the concatenation of its
;; *.json artifacts in sorted order (same enumeration order as the parser).
;; → (values checksum read-ok?)
(define (artifact-checksum artifact-files)
  (define (read-bytes-safe p)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (file->bytes p)))
  (define parts (filter values (map read-bytes-safe artifact-files)))
  (cond
    [(< (length parts) (length artifact-files))
     (values #f #f)]
    [else
     (values (bytes->hex-string (sha256-bytes (apply bytes-append parts))) #t)]))

;; Probe 3: freshness (max accepted age) + inventory compatibility.
(define (probe-snapshot-freshness! durations files age-seconds max-age-seconds)
  (cond
    [(not age-seconds)
     (values 'stale "snapshot modification time unavailable; cannot prove freshness")]
     [(>= age-seconds max-age-seconds)
      (values 'stale (format "snapshot age ~a s exceeds max accepted age ~a s"
                             age-seconds max-age-seconds))]
    [else
     (define selected
       (for/hash ([f (in-list files)])
         (values (normalize-path-key f) #t)))
     (define overlapping
       (for/sum ([(k _) (in-hash durations)])
         (if (hash-ref selected (normalize-path-key k) #f) 1 0)))
     (cond
       [(and (> (hash-count durations) 0) (= overlapping 0))
        (values 'wrong-inventory
                "snapshot duration entries do not overlap the current file inventory")]
       [else
        (values 'usable #f)])]))

;; ── prepare-ordering ────────────────────────────────────────────────────
;; Pure (side effects limited to reading the snapshot artifacts): builds the
;; ordering decision for `files`.  `max-age-seconds` is the maximum accepted
;; snapshot age; `duration-source` is a path (string) or #f.
(define (prepare-ordering files requested max-age-seconds duration-source)
  (define-values (readable? artifact-files age-seconds read-reason)
    (probe-snapshot-readable! duration-source max-age-seconds))
  (define-values (durations parser-status)
    (if readable?
        (load-duration-snapshot duration-source)
        (values #f #f)))
  (define-values (checksum checksum-ok?)
    (if readable?
        (artifact-checksum artifact-files)
        (values #f #f)))
  (define snapshot-status
    (cond
      [(not readable?) 'missing]
      [(eq? parser-status 'corrupt) 'malformed]
      [(not checksum-ok?) 'corrupt]
      [(eq? parser-status 'missing) 'missing]
      [else
       (define-values (freshness-status _reason)
         (probe-snapshot-freshness! durations files age-seconds max-age-seconds))
       freshness-status]))
  (define reason
    (if (eq? requested 'lpt)
        (cond
          [(not readable?) read-reason]
          [(eq? snapshot-status 'malformed) "snapshot artifact is unreadable or has an invalid shape"]
          [(eq? snapshot-status 'corrupt) "snapshot artifact bytes could not be read (I/O error)"]
          [(eq? snapshot-status 'stale)
           (format "snapshot age ~a s exceeds max accepted age ~a s" age-seconds max-age-seconds)]
          [(eq? snapshot-status 'wrong-inventory)
           "snapshot duration entries do not overlap the current file inventory"]
          [else #f])
        #f))
  (define usable? (eq? snapshot-status 'usable))
  (define used-durations
    (if usable?
        (retained-max-durations durations files)
        #f))
  (define mode (if (and (eq? requested 'lpt) usable?) 'lpt 'fifo))
  (define n-artifacts (and readable? (length artifact-files)))
  (define decision
    (cond
      [(not (eq? requested 'lpt)) "ordering fifo requested"]
      [(eq? mode 'lpt)
       (format "lpt applied from ~a artifacts (checksum ~a, max age ~a s, ~a duration entr~a)"
               n-artifacts
               checksum
               max-age-seconds
               (hash-count durations)
               (if (= (hash-count durations) 1) "y" "ies"))]
      [else
       (format "fifo fallback (~a)" reason)]))
  (ordering-record mode requested reason checksum snapshot-status n-artifacts
                   decision used-durations))

;; Retain the MAX duration per file (not the latest, not the mean): queue
;; workers start historically-long files first, so the retained value is the
;; worst-case evidence for each file.  Only entries whose repository path is
;; among the selected `files` are retained; absent entries are simply not
;; used (LPT still applies to the files that HAVE evidence; the rest keep
;; their input-relative position via the stable path tie-break).
(define (retained-max-durations durations files)
  (define selected
    (for/hash ([f (in-list files)])
      (values (normalize-path-key f) #t)))
  (define result (make-hash))
  (hash-for-each durations
                 (lambda (k dur-seconds)
                   (define key (normalize-path-key k))
                   (when (hash-ref selected key #f)
                     (hash-update! result key
                                   (lambda (old) (max old dur-seconds))
                                   dur-seconds))))
  result)

;; ── order-files ─────────────────────────────────────────────────────────
;; Pure permutation: `order-files` returns a permutation of `files` — same
;; length, same membership, no duplicates.  The serial/parallel partition
;; happens AFTER ordering (in the runner), so ordering can never move a file
;; across the partition.  FIFO is the identity permutation.
(define (order-files files rec)
  (define mode (ordering-record-mode rec))
  (cond
    [(eq? mode 'fifo) files]
    [(eq? mode 'lpt)
     (define used (ordering-record-used-durations rec))
     (define indexed
       (for/list ([f (in-list files)]
                  [i (in-naturals)])
         (list f i (hash-ref used (normalize-path-key f) 0.0))))
     (define sorted
       (sort indexed
             (lambda (a b)
               (define dur-a (caddr a))
               (define dur-b (caddr b))
               (define path-a (path->string-repr (car a)))
               (define path-b (path->string-repr (car b)))
               (cond
                 [(> dur-a dur-b) #t]
                 [(< dur-a dur-b) #f]
                 [else
                  ;; Stable repository-path tie-break: shorter path first,
                  ;; then lexicographic, then original input index (total
                  ;; determinism for every input, including duplicates).
                  (cond
                    [(< (string-length path-a) (string-length path-b)) #t]
                    [(> (string-length path-a) (string-length path-b)) #f]
                    [(string<? path-a path-b) #t]
                    [(string>? path-a path-b) #f]
                    [else (< (cadr a) (cadr b))])]))
             #:key identity))
     (map car sorted)]
    [else files]))

(module+ test
  ;; Self-tests for the pure permutation + fallback contract (the dedicated
  ;; integration tests live in tests/test-runner-scheduler-order.rkt).
  (require rackunit)
  (define (fake-files n)
    (for/list ([i (in-range n)])
      (format "tests/test-~a.rkt" i)))
  (test-case "fifo is the identity permutation"
    (define files (fake-files 5))
    (define rec (prepare-ordering files 'fifo 7 #f))
    (check-eq? (ordering-record-mode rec) 'fifo)
    (check-equal? (order-files files rec) files)
    (check-equal? (ordering-record-fallback-reason rec) #f))
  (test-case "lpt without snapshot falls back to fifo with named reason"
    (define files (fake-files 5))
    (define rec (prepare-ordering files 'lpt 7 #f))
    (check-eq? (ordering-record-mode rec) 'fifo)
    (check-equal? (order-files files rec) files)
    (check-true (string? (ordering-record-fallback-reason rec)))
    (check-eq? (ordering-record-snapshot-status rec) 'missing))
  (test-case "ordering is a pure permutation"
    (define files (fake-files 8))
    (define rec (prepare-ordering files 'lpt 7 "/nonexistent/snapshot.json"))
    (define ordered (order-files files rec))
    (check-equal? (length ordered) (length files))
    (check-equal? (sort ordered string<?)
                  (sort files string<?))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (snapshot mode . files)
   (define rec (prepare-ordering files (string->symbol mode) 7 snapshot))
   (for ([f (in-list (order-files files rec))])
     (displayln f))))
