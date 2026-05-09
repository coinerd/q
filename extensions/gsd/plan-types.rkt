#lang typed/racket

;; extensions/gsd/plan-types.rkt — Structured Plan/Task/Wave types
;;
;; Wave 0 of v0.21.0: Machine-parseable plan types with validation.
;; Migrated to #lang typed/racket in v0.22.8 W2 (TR expansion).
;;
;; ── TR BOUNDARY ──────────────────────────────────────────────
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR's boundary system. Struct
;; constructors enforce field types at call sites in untyped modules.
;; New consumers should import normally — no require/typed needed.
;; ──────────────────────────────────────────────────────────────

(require racket/string
         racket/list
         racket/format)

;; ============================================================
;; Type definitions
;; ============================================================

(define-type WaveStatus (U 'pending 'in-progress 'completed 'failed 'skipped))

;; ============================================================
;; Core structs (immutable, typed)
;; ============================================================

(struct gsd-task
        ([name : String] [files : (Listof String)]
                         [action : String]
                         [verify : String]
                         [done : String]
                         [status : WaveStatus])
  #:transparent)

(struct gsd-wave
        ([index : Natural] [title : String]
                           [status : WaveStatus]
                           [root-cause : String]
                           [files : (Listof String)]
                           [tasks : (Listof gsd-task)]
                           [verify : String]
                           [done-criteria : (Listof String)])
  #:transparent)

(struct gsd-plan
        ([waves : (Listof gsd-wave)] [context-bundle : (U String #f)]
                                     [constraints : (Listof String)]
                                     [must-haves : (Listof String)])
  #:transparent)

(struct validation-result ([errors : (Listof String)] [warnings : (Listof String)]) #:transparent)

;; ============================================================
;; Normalized Plan IR (v0.24.2 — F7)
;; ============================================================
;; Immutable IR produced by parsing + normalization.
;; This is what the validator checks and executor consumes.

(define-type NormWaveStatus (U 'pending 'in-progress 'done 'skipped))

(struct gsd-normalized-wave
        ([index : Natural] [title : String]
                           [tasks : (Listof String)]
                           [verify-command : String]
                           [done-criteria : (Listof String)]
                           [files : (Listof String)]
                           [status : NormWaveStatus])
  #:transparent)

(struct gsd-normalized-plan
        ([waves : (Listof gsd-normalized-wave)] [context-bundle : (U String #f)]
                                                [metadata : (HashTable Symbol Any)]
                                                [source-path : (U String #f)])
  #:transparent)

(struct gsd-validated-plan ([plan : gsd-normalized-plan]) #:transparent)

;; ============================================================
;; Convenience constructors
;; ============================================================

(: make-gsd-task : String (Listof String) String String String * -> gsd-task)
(define (make-gsd-task name files action verify . rest)
  (gsd-task name
            files
            action
            verify
            (if (null? rest)
                ""
                (car rest))
            'pending))

(: make-gsd-wave
   :
   Natural
   String
   String
   (Listof String)
   (Listof gsd-task)
   String
   (Listof String)
   ->
   gsd-wave)
(define (make-gsd-wave index title root-cause files tasks verify done-criteria)
  (gsd-wave index title 'pending root-cause files tasks verify done-criteria))

;; ============================================================
;; Status setters (return new struct — immutable)
;; ============================================================

(: gsd-task-set-status : gsd-task WaveStatus -> gsd-task)
(define (gsd-task-set-status t status)
  (struct-copy gsd-task t [status status]))

(: gsd-wave-set-status : gsd-wave WaveStatus -> gsd-wave)
(define (gsd-wave-set-status w status)
  (struct-copy gsd-wave w [status status]))

;; ============================================================
;; Validation helpers
;; ============================================================

(: validation-valid? : validation-result -> Boolean)
(define (validation-valid? vr)
  (null? (validation-result-errors vr)))

;; Aliases for cleaner API
(define validation-errors validation-result-errors)
(define validation-warnings validation-result-warnings)

;; ============================================================
;; Validation
;; ============================================================

(: validate-plan : gsd-plan -> validation-result)
(define (validate-plan plan)
  (define waves (gsd-plan-waves plan))
  (: errors : (Listof String))
  (define errors '())
  (: warnings : (Listof String))
  (define warnings '())
  ;; Check: plan has at least 1 wave
  (when (null? waves)
    (set! errors (cons "Plan has no waves" errors)))
  ;; Per-wave checks
  (for ([w waves])
    (define widx (gsd-wave-index w))
    (define prefix (format "Wave ~a" widx))
    (when (string=? (gsd-wave-title w) "")
      (set! errors (cons (format "~a: missing title" prefix) errors)))
    (when (null? (gsd-wave-files w))
      (set! warnings (cons (format "~a: no file references" prefix) warnings)))
    (when (string=? (gsd-wave-verify w) "")
      (set! warnings (cons (format "~a: no verify command" prefix) warnings))))
  ;; Error: ALL waves are file-less
  (when (and (pair? waves) (andmap (lambda ([w : gsd-wave]) (null? (gsd-wave-files w))) waves))
    (set! errors (cons "Plan has no file references in any wave — nothing to execute" errors)))
  (validation-result (reverse errors) (reverse warnings)))

;; ============================================================
;; Plan utilities
;; ============================================================

(: plan-wave-ref : gsd-plan Natural -> (U gsd-wave #f))
(define (plan-wave-ref plan idx)
  (findf (lambda ([w : gsd-wave]) (= (gsd-wave-index w) idx)) (gsd-plan-waves plan)))

(: plan-pending-waves : gsd-plan -> (Listof gsd-wave))
(define (plan-pending-waves plan)
  (filter (lambda ([w : gsd-wave]) (eq? (gsd-wave-status w) 'pending)) (gsd-plan-waves plan)))

(: plan-next-pending-wave : gsd-plan -> (U gsd-wave #f))
(define (plan-next-pending-wave plan)
  (findf (lambda ([w : gsd-wave]) (eq? (gsd-wave-status w) 'pending)) (gsd-plan-waves plan)))

;; ============================================================
;; Status conversion helpers
;; ============================================================

(: wave-status->string : WaveStatus -> String)
(define (wave-status->string sym)
  (cond
    [(eq? sym 'pending) "Inbox"]
    [(eq? sym 'in-progress) "In-Progress"]
    [(eq? sym 'completed) "DONE"]
    [(eq? sym 'failed) "FAILED"]
    [(eq? sym 'skipped) "DEFERRED"]))

(: string->wave-status : String -> WaveStatus)
(define (string->wave-status str)
  (cond
    [(string=? str "Inbox") 'pending]
    [(string=? str "In-Progress") 'in-progress]
    [(string=? str "DONE") 'completed]
    [(string=? str "FAILED") 'failed]
    [(string=? str "DEFERRED") 'skipped]
    [else 'pending]))

(: gsd-wave-slug : gsd-wave -> String)
(define (gsd-wave-slug w)
  (define title (gsd-wave-title w))
  (if (> (string-length title) 0)
      (let* ([s
              :
              String
              (string-trim title)]
             [result
              :
              String
              (slugify-chars s)])
        (cond
          [(> (string-length result) 40) (string-trim (substring result 0 40) "-" #:right? #t)]
          [(string=? result "") "wave"]
          [else result]))
      "wave"))

(: slugify-chars : String -> String)
(define (slugify-chars s)
  (define cs
    :
    (Listof Char)
    (string->list s))
  (define mapped
    :
    (Listof (Option Char))
    (for/list :
      (Listof (Option Char))
      ([c : Char cs])
      (cond
        [(char-alphabetic? c) (char-downcase c)]
        [(char-numeric? c) c]
        [(char=? c #\-) #\-]
        [(char=? c #\space) #\-]
        [else #f])))
  (list->string (filter char? mapped)))

;; ============================================================
;; ============================================================
;; Normalization (v0.24.2 — F7)
;; ============================================================

(: normalize-plan : gsd-plan -> (U gsd-normalized-plan String))
(define (normalize-plan plan)
  ;; Validate structural integrity then produce normalized IR.
  ;; Returns gsd-normalized-plan on success, error string on failure.
  (define waves (gsd-plan-waves plan))
  ;; Check: sequential indices
  (define indices
    (for/list :
      (Listof Natural)
      ([w : gsd-wave waves])
      (gsd-wave-index w)))
  (define expected-indices (build-list (length waves) (lambda ([i : Natural]) i)))
  (cond
    [(not (equal? indices expected-indices))
     (format "Wave indices not sequential 0..~a: ~a" (sub1 (length waves)) indices)]
    [(not (null? waves))
     ;; Check: no duplicate titles
     (define titles
       (for/list :
         (Listof String)
         ([w : gsd-wave waves])
         (gsd-wave-title w)))
     (define unique-titles (remove-duplicates titles))
     (if (< (length unique-titles) (length titles))
         "Duplicate wave titles detected"
         ;; Check: all tasks have non-empty verify strings (warning, not error)
         (let* ([norm-waves
                 :
                 (Listof gsd-normalized-wave)
                 (for/list :
                   (Listof gsd-normalized-wave)
                   ([w : gsd-wave waves])
                   (gsd-normalized-wave (gsd-wave-index w)
                                        (gsd-wave-title w)
                                        (for/list :
                                          (Listof String)
                                          ([t : gsd-task (gsd-wave-tasks w)])
                                          (gsd-task-name t))
                                        (gsd-wave-verify w)
                                        (gsd-wave-done-criteria w)
                                        (gsd-wave-files w)
                                        (wave-status->norm-status (gsd-wave-status w))))]
                [norm-plan (gsd-normalized-plan norm-waves
                                                (gsd-plan-context-bundle plan)
                                                (hasheq 'constraints
                                                        (gsd-plan-constraints plan)
                                                        'must-haves
                                                        (gsd-plan-must-haves plan))
                                                #f)])
           norm-plan))]
    [else (gsd-normalized-plan '() #f (hasheq) #f)]))

(: wave-status->norm-status : WaveStatus -> NormWaveStatus)
(define (wave-status->norm-status ws)
  (cond
    [(eq? ws 'pending) 'pending]
    [(eq? ws 'in-progress) 'in-progress]
    [(eq? ws 'completed) 'done]
    [(eq? ws 'skipped) 'skipped]
    [(eq? ws 'failed) 'pending] ; failed → pending for re-execution
    [else 'pending]))

(: validated-plan->normalized : gsd-validated-plan -> gsd-normalized-plan)
(define (validated-plan->normalized vp)
  (gsd-validated-plan-plan vp))

;; ============================================================
;; Parsing re-exports from untyped parser module
;; ============================================================

(require/typed "plan-types-parser.rkt"
               [parse-waves-from-markdown-raw (String -> (Listof (HashTable Symbol Any)))]
               [parse-wave-content (String -> (HashTable Symbol Any))]
               [clean-file-path (String -> String)])

(: raw-ref : (HashTable Symbol Any) Symbol Any -> Any)
(define (raw-ref h k default)
  (if (hash-has-key? h k)
      (hash-ref h k)
      default))

;; Wrap raw parsed data into gsd-wave structs
;; I-18 (v0.35.1): Validated accessors replace unsafe casts
(: expect-natural : Any -> Natural)
(define (expect-natural v)
  (cond
    [(exact-nonnegative-integer? v) v]
    [else (error 'parse-waves "expected Natural for index, got ~a" v)]))

(: expect-string : Any -> String)
(define (expect-string v)
  (cond
    [(string? v) v]
    [else (error 'parse-waves "expected String, got ~a" v)]))

(: expect-string-list : Any -> (Listof String))
(define (expect-string-list v)
  (cond
    [(and (list? v) (andmap string? v)) v]
    [else (error 'parse-waves "expected (Listof String), got ~a" v)]))

(: parse-waves-from-markdown : String -> (Listof gsd-wave))
(define (parse-waves-from-markdown md-text)
  (for/list :
    (Listof gsd-wave)
    ([raw : (HashTable Symbol Any) (parse-waves-from-markdown-raw md-text)])
    (gsd-wave (expect-natural (raw-ref raw 'index 0))
              (expect-string (raw-ref raw 'title ""))
              'pending
              (expect-string (raw-ref raw 'root-cause ""))
              (expect-string-list (raw-ref raw 'files '()))
              '()
              (expect-string (raw-ref raw 'verify ""))
              (expect-string-list (raw-ref raw 'done '())))))

;; ============================================================
;; Provide
;; ============================================================

(provide (struct-out gsd-task)
         (struct-out gsd-wave)
         (struct-out gsd-plan)
         (struct-out validation-result)

         ;; Constructors with defaults
         make-gsd-task
         make-gsd-wave

         ;; Status setters
         gsd-task-set-status
         gsd-wave-set-status

         ;; Validation
         validation-valid?
         validate-plan

         ;; Aliases
         validation-errors
         validation-warnings

         ;; Parsing (re-exported from parser module)
         parse-waves-from-markdown
         parse-wave-content
         clean-file-path

         ;; Plan utilities
         plan-wave-ref
         plan-pending-waves
         plan-next-pending-wave

         ;; Status conversion helpers
         wave-status->string
         string->wave-status
         gsd-wave-slug

         ;; Normalized Plan IR (v0.24.2)
         (struct-out gsd-normalized-wave)
         (struct-out gsd-normalized-plan)
         (struct-out gsd-validated-plan)
         normalize-plan
         validated-plan->normalized
         NormWaveStatus

         ;; Type alias
         WaveStatus

         ;; I-18 (v0.35.1): Validated accessors
         expect-natural
         expect-string
         expect-string-list)
