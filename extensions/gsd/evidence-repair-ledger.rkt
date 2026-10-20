#lang racket/base
;; Trusted engine installs the registrar closure, never a JSON authority flag.
(require json
         racket/file
         racket/path
         racket/list
         "review-proof.rkt"
         "evidence-repair-format.rkt"
         "evidence-probe.rkt")
(provide load-evidence-repair
         reserve-evidence-repair!
         advance-evidence-repair!
         make-evidence-registrar
         register-owned-evidence!
         admit-promoted-captured-evidence!)
(define limits (hasheq 'reservations 1 'jobs 2 'timeout-sec 600))
(define keys
  '(plan-id wave
            attempt-id
            attempt-fence
            receipt-generation
            receipt-sha256
            evidence-revision
            repo
            branch
            origin
            head
            tree
            snapshot-sha256
            green-proof-sha256))
(define (context! c)
  (unless (and (valid-evidence-context? c) (jsexpr? c))
    (error 'ledger "incomplete canonical context")))
(define (directory plan wave [generation 0])
  (unless (and (string? plan)
               (regexp-match? #px"^[A-Za-z0-9_-]+$" plan)
               (exact-nonnegative-integer? wave)
               (exact-nonnegative-integer? generation))
    (error 'ledger "invalid plan/wave"))
  (build-path (simplify-path (build-path (current-owned-proof-root) 'up))
              "evidence-repairs"
              plan
              (if (zero? generation)
                  (format "W~a" wave)
                  (format "W~a.g~a" wave generation))))
(define (empty)
  (hasheq 'limits limits 'operation 'null 'registrations '()))
(define (read-state d)
  (define p (build-path d "ledger.json"))
  (define s
    (if (file-exists? p)
        (call-with-input-file p read-json)
        (empty)))
  ;; Config is not authority: hard ceiling cannot be increased by journal edits.
  (unless (equal? (hash-ref s 'limits #f) limits)
    (error 'ledger "altered limits"))
  (when (file-exists? p)
    (unless (equal? (hash-ref s 'sha256 #f) (canonical-evidence-digest s))
      (error 'ledger "ledger integrity mismatch")))
  (define op (hash-ref s 'operation))
  (unless (or (eq? op 'null)
              (and (hash? op)
                   (valid-evidence-context? (hash-ref op 'context #f))
                   (equal? (hash-ref op 'id #f) "repair-1")
                   (equal? (hash-ref op 'limits #f) limits)
                   (equal? (hash-ref op 'reserved-jobs #f) 2)
                   (member
                    (hash-ref op 'state #f)
                    '("reserved" "intent" "start" "captured" "registered" "blocked" "exhausted"))))
    (error 'ledger "invalid operation"))
  (define regs (hash-ref s 'registrations))
  (unless (and (list? regs)
               (<= (length regs) 1)
               (or (null? regs)
                   (and (hash? op)
                        (equal? (hash-ref op 'state) "registered")
                        (equal? (hash-ref (car regs) 'context #f) (hash-ref op 'context))
                        (equal? (hash-ref (car regs) 'probe #f) (hash-ref op 'probe #f))
                        (equal? (hash-ref (car regs) 'operation-id #f) (hash-ref op 'id)))))
    (error 'ledger "registration operation mismatch"))
  (for/fold ([prior 'null])
            ([r (in-list regs)]
             [revision (in-naturals)])
    (unless (and (equal? (hash-ref r 'sha256 #f) (canonical-evidence-digest r))
                 (equal? (hash-ref r 'prior-registration-sha256 #f) prior)
                 (equal? (hash-ref r 'previous-revision #f) revision)
                 (equal? (hash-ref r 'revision #f) (add1 revision)))
      (error 'ledger "registration chain mismatch"))
    (hash-ref r 'sha256))
  s)
(define (save! d s)
  (call-with-atomic-output-file (build-path d "ledger.json")
                                (lambda (out tmp)
                                  (write-json (hash-set s 'sha256 (canonical-evidence-digest s)) out)
                                  (flush-output out))))
(define (locked base plan wave f #:generation [generation 0])
  (define d (directory plan wave generation))
  (define identity (path->string (simplify-path (path->complete-path base) #t)))
  (for ([p (in-list (list d (build-path d "ledger.json") (build-path d "ledger.lock")))])
    (when (link-exists? p)
      (error 'ledger "linked managed resource")))
  (make-directory* d)
  (call-with-file-lock/timeout #f
                               'exclusive
                               (lambda ()
                                 (define s (read-state d))
                                 (unless (equal? (hash-ref s 'resource-identity identity) identity)
                                   (error 'ledger "resource identity mismatch"))
                                 (f d (hash-set s 'resource-identity identity)))
                               (lambda () (error 'ledger "busy"))
                               #:lock-file (build-path d "ledger.lock")))
(define (load-evidence-repair base plan wave [generation 0])
  (locked
   base
   plan
   wave
   #:generation generation
   (lambda (d s)
     (define op (hash-ref s 'operation))
     (hash-set
      (hash-set s
                'remaining-allowance
                (hasheq 'reservations (if (hash? op) 0 1) 'jobs (if (hash? op) 0 2) 'timeout-sec 600))
      'resume
      (if (and (hash? op) (member (hash-ref op 'state) '("intent" "start" "captured")))
          "paused"
          "ready")))))
(define (answer status op)
  (hasheq 'status status 'operation op))
(define (reserve-evidence-repair! base c spec #:limits [requested limits])
  (context! c)
  (unless (and (equal? requested limits) (string? spec) (positive? (string-length spec)))
    (error 'ledger "invalid immutable limits/spec hash"))
  (locked base
          (hash-ref c 'plan-id)
          (hash-ref c 'wave)
          #:generation (hash-ref c 'receipt-generation 0)
          (lambda (d s)
            (define old (hash-ref s 'operation))
            (cond
              [(hash? old)
               (answer (cond
                         [(equal? (hash-ref old 'context) c)
                          (if (equal? (hash-ref old 'spec) spec) "existing" "stale")]
                         [else "exhausted"])
                       old)]
              [else
               (define op
                 (hasheq 'id
                         "repair-1"
                         'context
                         c
                         'spec
                         spec
                         'state
                         "reserved"
                         'limits
                         limits
                         'reserved-jobs
                         2
                         'no-progress
                         spec))
               (save! d (hash-set s 'operation op))
               (answer "reserved" op)]))))
(define (checked-op s c id)
  (define op (hash-ref s 'operation))
  (unless (and (hash? op) (equal? (hash-ref op 'id) id) (equal? (hash-ref op 'context) c))
    (error 'ledger "stale identity"))
  op)
(define (advance-evidence-repair! base c id expected fields)
  (context! c)
  (locked base
          (hash-ref c 'plan-id)
          (hash-ref c 'wave)
          #:generation (hash-ref c 'receipt-generation 0)
          (lambda (d s)
            (define op (checked-op s c id))
            (define next (hash-ref fields 'state))
            (unless (and (equal? expected (hash-ref op 'state))
                         (for/and ([k (in-hash-keys fields)])
                           (memq k '(state probe reason)))
                         (or (and (equal? expected "reserved") (equal? next "intent"))
                             (and (equal? expected "intent") (equal? next "start"))
                             (and (equal? expected "start")
                                  (equal? next "captured")
                                  (hash-has-key? fields 'probe))
                             (and (member expected '("reserved" "intent" "start"))
                                  (member next '("blocked" "exhausted")))))
              (error 'ledger "stale or invalid transition"))
            (define updated
              (for/fold ([o op]) ([(k v) (in-hash fields)])
                (hash-set o k v)))
            (save! d (hash-set s 'operation updated))
            updated)))
;; ============================================================
;; BUG-0088 (d): the explicit promoted-captured ledger admission. ONLY the
;; protected owner transition can install a captured record for a promoted
;; supplemental operation: the opaque, single-consumption capability is
;; verified (non-destructively) here and consumed later by the registrar's
;; validate closure. The ordinary registrar's captured checks (exact
;; context, probe equality, revision/chain arithmetic, one registration)
;; are preserved unchanged; a caller-supplied captured JSON is never an
;; admission.
(define (admit-promoted-captured-evidence! base c probe id capability)
  (context! c)
  (unless (promoted-registration? capability)
    (error 'ledger "an opaque promoted capability is required"))
  (unless (verify-promoted-registration-capability! capability c probe id)
    (error 'ledger "the promotion capability does not bind this context/probe/operation"))
  (locked
   base
   (hash-ref c 'plan-id)
   (hash-ref c 'wave)
   #:generation (hash-ref c 'receipt-generation 0)
   (lambda (d s)
     (define old (hash-ref s 'operation))
     (cond
       ;; A fresh ledger carries the JSON 'null operation (truthy in
       ;; Racket): "no operation yet" is ANY non-hash, matching the
       ;; reserve-evidence-repair! and load-evidence-repair idiom.
       [(not (hash? old))
        (define op
          (hasheq 'id
                  "repair-1"
                  'context
                  c
                  'spec
                  (hash-ref probe 'sha256)
                  'state
                  "captured"
                  'limits
                  limits
                  'reserved-jobs
                  2
                  'no-progress
                  "promoted-supplemental-evidence"
                  'probe
                  probe
                  'promoted
                  (hasheq 'promotion
                          (promoted-registration-promotion capability)
                          'generation
                          (hash-ref c 'receipt-generation))))
        (save! d (hash-set s 'operation op))
        op]
       [else
        (unless (and (equal? (hash-ref old 'id #f) "repair-1") (equal? (hash-ref old 'context #f) c))
          (error 'ledger "a different operation owns this generation"))
        (define state (hash-ref old 'state #f))
        (cond
          [(equal? state "captured")
           ;; recovery replay: identical captured record required
           (unless (and (equal? (hash-ref old 'probe #f) probe)
                        (equal? (hash-ref old 'promoted #f)
                                (hasheq 'promotion
                                        (promoted-registration-promotion capability)
                                        'generation
                                        (hash-ref c 'receipt-generation))))
             (error 'ledger "conflicting promoted captured record"))
           old]
          ;; crash after registration: recovery reads the identical
          ;; existing registration instead of appending twice
          [(equal? state "registered") old]
          [else
           (error 'ledger
                  (format "ambiguous promoted operation state ~a; refusing adoption" state))])]))))
;; validate : opaque-token context probe-ref operation-id -> boolean
;; This factory is a trusted composition port, not a model-facing API.
(define (make-evidence-registrar validate)
  (unless (procedure-arity-includes? validate 4)
    (error 'ledger "validator arity"))
  ;; A consumed token may replay only in this owner process, never as JSON.
  (define consumed (make-weak-hasheq))
  (lambda (base plan wave #:context c #:probe probe #:authorization auth #:operation-id id)
    (context! c)
    (unless (and (equal? plan (hash-ref c 'plan-id)) (equal? wave (hash-ref c 'wave)))
      (error 'ledger "context route mismatch"))
    ;; Engine validation invokes the live guard, which reloads this ledger.
    ;; Never call it while holding the repair lock. CAS below remains final.
    (unless (equal? (hash-ref consumed auth #f) (list base c probe id))
      (unless (validate auth c probe id)
        (error 'ledger "unauthorized proof"))
      (hash-set! consumed auth (list base c probe id)))
    (locked base
            plan
            wave
            #:generation (hash-ref c 'receipt-generation 0)
            (lambda (d s)
              (define op (checked-op s c id))
              (unless (equal? probe (hash-ref op 'probe #f))
                (error 'ledger "unauthorized or conflicting proof"))
              (define regs (hash-ref s 'registrations))
              (define prior (findf (lambda (r) (equal? (hash-ref r 'operation-id) id)) regs))
              (cond
                [prior
                 (unless (and (equal? (hash-ref consumed auth #f) (list base c probe id))
                              (equal? c (hash-ref prior 'context))
                              (equal? probe (hash-ref prior 'probe)))
                   (error 'ledger "registration conflict"))
                 prior]
                [else
                 (define rev (hash-ref c 'evidence-revision))
                 (unless (and (equal? (hash-ref op 'state) "captured") (= rev (length regs)))
                   (error 'ledger "stale revision/state"))
                 (define r
                   (hasheq 'schema
                           "evidence-registration/v1"
                           'context
                           c
                           'previous-revision
                           rev
                           'revision
                           (add1 rev)
                           'probe
                           probe
                           'retained-green
                           (hash-ref c 'green-proof-sha256)
                           'operation-id
                           id
                           'prior-registration-sha256
                           (if (null? regs)
                               'null
                               (hash-ref (last regs) 'sha256))))
                 (define sealed (hash-set r 'sha256 (canonical-evidence-digest r)))
                 ;; One atomic journal commit: registration and operation cannot diverge.
                 (save! d
                        (hash-set (hash-set s 'registrations (append regs (list sealed)))
                                  'operation
                                  (hash-set op 'state "registered")))
                 sealed])))))

;; Production composition consumes the engine's real opaque capability.
(define registrars (make-hash))
(define (register-owned-evidence! base
                                  plan
                                  wave
                                  #:context c
                                  #:probe ref
                                  #:authorization auth
                                  #:operation-id id
                                  #:promoted-capability [promoted-capability #f])
  ;; Opaque capability identity uses the exact authenticated execution port,
  ;; never a path-equivalent campaign root or a prior campaign's closure.
  (define repo (hash-ref c 'repo))
  (validate-baseline-probe! repo ref c)
  (define registrar
    (if promoted-capability
        ;; The promoted capability is single-consumption and binds the exact
        ;; context/probe/operation; it replaces ONLY the authority token
        ;; consumption. Every ordinary registrar check stays identical.
        (make-evidence-registrar
         (lambda (_token context probe operation)
           (consume-promoted-registration-capability! promoted-capability context probe operation)))
        (hash-ref! registrars
                   base
                   (lambda ()
                     (make-evidence-registrar (lambda (token context probe operation)
                                                (consume-evidence-registration-authorization!
                                                 token
                                                 (hash-ref context 'repo)
                                                 context
                                                 probe
                                                 operation)))))))
  (registrar base plan wave #:context c #:probe ref #:authorization auth #:operation-id id))
