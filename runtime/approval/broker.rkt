#lang racket/base

;; Frontend-neutral, digest-bound approval broker.
;; Registration owns the immutable presentation and execution commitment.
;; Decisions must repeat that commitment, and approval yields an opaque,
;; one-use grant consumable only for the exact digest.

(require racket/async-channel
         racket/contract
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/random crypto-random-bytes))

(provide (contract-out [approval-channel? (-> any/c boolean?)]
                       [make-approval-channel
                        (->* () (#:timeout-ms exact-nonnegative-integer?) approval-channel?)]
                       [set-approval-channel! (-> (or/c approval-channel? #f) void?)]
                       [clear-approval-channel! (-> void?)]
                       [current-approval-channel (-> (or/c approval-channel? #f))]
                       [register-approval-request-for-channel!
                        (-> approval-channel? string? hash? (or/c string? #f))]
                       [approval-request-view (-> string? string? (or/c hash? #f))]
                       [approval-request-pending? (->* (string?) ((or/c string? #f)) boolean?)]
                       [approval-decide! (-> string? string? boolean? boolean?)]
                       [approval-await-grant
                        (->* (string? string?)
                             ((or/c exact-nonnegative-integer? #f))
                             (values symbol? (or/c approval-grant? #f)))]
                       [approval-grant? (-> any/c boolean?)]
                       [call-with-approval-grant (-> approval-grant? string? (-> any/c) any/c)]
                       [cancel-approval-request! (-> string? boolean?)]
                       [clear-pending-approvals! (-> void?)]
                       [pending-approval-count (-> exact-nonnegative-integer?)]))

(define DEFAULT-APPROVAL-TIMEOUT-MS 120000)
(define digest-rx #px"^[0-9a-f]{64}$")

;; This is an identity/lifecycle lease, not a raw decision transport.
(struct approval-channel (timeout-ms nonce) #:transparent)
(struct pending-approval (id generation digest view wake state approved? waiter?)
  #:mutable
  #:transparent)
;; Opaque outside this module. The semaphore makes consume exactly once.
(struct approval-grant (id generation digest consumed? semaphore))

(define current-channel-box (box #f))
(define generation-box (box 0))
(define pending-box (box (hash)))
(define registry-sem (make-semaphore 1))

(define (valid-digest? value)
  (and (string? value) (regexp-match? digest-rx value)))

;; Freeze frontend presentation data without depending on the tool layer.
(define (immutable-approval-copy value)
  (cond
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values (immutable-approval-copy key) (immutable-approval-copy item)))]
    [(list? value) (map immutable-approval-copy value)]
    [(pair? value) (cons (immutable-approval-copy (car value)) (immutable-approval-copy (cdr value)))]
    [(vector? value)
     (vector->immutable-vector (for/vector ([item (in-vector value)])
                                 (immutable-approval-copy item)))]
    [(string? value) (string->immutable-string (string-copy value))]
    [(bytes? value) (bytes->immutable-bytes (bytes-copy value))]
    [else value]))

;; Broker presentations are plain immutable-copyable data only. Opaque mutable
;; containers or executable values are rejected at registration.
(define (approval-presentation-value? value)
  (cond
    [(hash? value)
     (and (for/and ([key (in-hash-keys value)])
            (or (symbol? key) (string? key)))
          (for/and ([item (in-hash-values value)])
            (approval-presentation-value? item)))]
    [(list? value) (andmap approval-presentation-value? value)]
    [(vector? value)
     (for/and ([item (in-vector value)])
       (approval-presentation-value? item))]
    [else
     (or (string? value)
         (bytes? value)
         (symbol? value)
         (boolean? value)
         (number? value)
         (null? value)
         (void? value))]))

(define (make-approval-channel #:timeout-ms [timeout-ms DEFAULT-APPROVAL-TIMEOUT-MS])
  (approval-channel timeout-ms (crypto-random-bytes 16)))

;; Caller holds registry-sem.
(define (cancel-all-locked!)
  (define pending (unbox pending-box))
  (set-box! pending-box (hash))
  (for ([request (in-hash-values pending)])
    (when (memq (pending-approval-state request) '(pending delivered))
      (set-pending-approval-state! request 'cancelled)
      (async-channel-put (pending-approval-wake request) 'terminal))))

(define (replace-channel! channel)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (cancel-all-locked!)
                         (set-box! current-channel-box channel)
                         (set-box! generation-box (add1 (unbox generation-box)))))
  (void))

(define (set-approval-channel! channel)
  (replace-channel! channel))

(define (clear-approval-channel!)
  (replace-channel! #f))

(define (current-approval-channel)
  (call-with-semaphore registry-sem (lambda () (unbox current-channel-box))))

(define (register-approval-request-for-channel! expected-channel digest presentation)
  (cond
    [(or (not (valid-digest? digest))
         (not (hash? presentation))
         (not (approval-presentation-value? presentation)))
     #f]
    [else
     (call-with-semaphore
      registry-sem
      (lambda ()
        (cond
          [(not (eq? expected-channel (unbox current-channel-box))) #f]
          [else
           (define generation (unbox generation-box))
           (define id
             (format "approval-~a-~a" generation (bytes->hex-string (crypto-random-bytes 16))))
           (define request
             (pending-approval id
                               generation
                               (string->immutable-string (string-copy digest))
                               (immutable-approval-copy presentation)
                               (make-async-channel)
                               'pending
                               #f
                               #f))
           (set-box! pending-box (hash-set (unbox pending-box) id request))
           id])))]))

(define (lookup-live-locked id [digest #f])
  (define request (hash-ref (unbox pending-box) id #f))
  (and request
       (= (pending-approval-generation request) (unbox generation-box))
       (or (not digest) (string=? digest (pending-approval-digest request)))
       request))

(define (approval-request-view id digest)
  (and (valid-digest? digest)
       (call-with-semaphore registry-sem
                            (lambda ()
                              (define request (lookup-live-locked id digest))
                              (and request
                                   (eq? (pending-approval-state request) 'pending)
                                   (pending-approval-view request))))))

(define (approval-request-pending? id [digest #f])
  (and (or (not digest) (valid-digest? digest))
       (call-with-semaphore registry-sem
                            (lambda ()
                              (define request (lookup-live-locked id digest))
                              (and request (eq? (pending-approval-state request) 'pending) #t)))))

(define (approval-decide! id digest approved?)
  (and (valid-digest? digest)
       (call-with-semaphore
        registry-sem
        (lambda ()
          (define request (lookup-live-locked id digest))
          (cond
            [(not (and request (eq? (pending-approval-state request) 'pending))) #f]
            [else
             (set-pending-approval-approved?! request approved?)
             (set-pending-approval-state! request 'delivered)
             (async-channel-put (pending-approval-wake request) 'terminal)
             #t])))))

;; Caller holds registry-sem.
(define (remove-exact-locked! id request)
  (when (eq? (hash-ref (unbox pending-box) id #f) request)
    (set-box! pending-box (hash-remove (unbox pending-box) id))))

(define (terminal-values request)
  (case (pending-approval-state request)
    [(delivered)
     (if (pending-approval-approved? request)
         (values 'approved
                 (approval-grant (pending-approval-id request)
                                 (pending-approval-generation request)
                                 (pending-approval-digest request)
                                 (box #f)
                                 (make-semaphore 1)))
         (values 'denied #f))]
    [(cancelled) (values 'cancelled #f)]
    [(timed-out) (values 'timed-out #f)]
    [else (values 'cancelled #f)]))

(define (approval-await-grant id digest [timeout-ms #f])
  (cond
    [(or (not (valid-digest? digest)) (and timeout-ms (not (exact-nonnegative-integer? timeout-ms))))
     (values 'cancelled #f)]
    [else
     (define claimed #f)
     (define waiter-finished (make-semaphore 0))
     (define-values (mode value wait-ms)
       (call-with-semaphore registry-sem
                            (lambda ()
                              (define request (lookup-live-locked id digest))
                              (cond
                                [(not request) (values 'terminal #f #f)]
                                [(eq? (pending-approval-state request) 'delivered)
                                 (remove-exact-locked! id request)
                                 (values 'terminal request #f)]
                                [(or (not (eq? (pending-approval-state request) 'pending))
                                     (pending-approval-waiter? request))
                                 (remove-exact-locked! id request)
                                 (values 'terminal request #f)]
                                [else
                                 (set! claimed request)
                                 (set-pending-approval-waiter?! request (current-thread))
                                 (values 'wait
                                         request
                                         (or timeout-ms
                                             (let ([channel (unbox current-channel-box)])
                                               (and channel (approval-channel-timeout-ms channel)))
                                             DEFAULT-APPROVAL-TIMEOUT-MS))]))))
     ;; kill-thread does not unwind the target continuation. A monitor removes
     ;; the exact claimed request if its owner dies before normal completion.
     (when claimed
       (define owner (pending-approval-waiter? claimed))
       (thread (lambda ()
                 (define abandoned?
                   (sync (handle-evt (thread-dead-evt owner) (lambda (_) #t))
                         (handle-evt waiter-finished (lambda (_) #f))))
                 (when abandoned?
                   (call-with-semaphore registry-sem
                                        (lambda ()
                                          (when (eq? (hash-ref (unbox pending-box) id #f) claimed)
                                            (remove-exact-locked! id claimed)
                                            (set-pending-approval-state! claimed 'cancelled)
                                            (async-channel-put (pending-approval-wake claimed)
                                                               'terminal))))))))
     (define-values (outcome grant)
       (cond
         [(eq? mode 'terminal)
          (if value
              (terminal-values value)
              (values 'cancelled #f))]
         [else
          (sync/timeout (/ wait-ms 1000.0) (pending-approval-wake value))
          (call-with-semaphore registry-sem
                               (lambda ()
                                 (define request value)
                                 (when (eq? (pending-approval-state request) 'pending)
                                   (set-pending-approval-state! request 'timed-out))
                                 (remove-exact-locked! id request)
                                 (terminal-values request)))]))
     (semaphore-post waiter-finished)
     (values outcome grant)]))

;; Caller holds registry-sem. Consume exactly once only while the frontend
;; generation that issued the grant remains active.
(define (consume-grant-locked! grant digest)
  (and (valid-digest? digest)
       (string=? digest (approval-grant-digest grant))
       (unbox current-channel-box)
       (= (approval-grant-generation grant) (unbox generation-box))
       (call-with-semaphore (approval-grant-semaphore grant)
                            (lambda ()
                              (cond
                                [(unbox (approval-grant-consumed? grant)) #f]
                                [else
                                 (set-box! (approval-grant-consumed? grant) #t)
                                 #t])))))

;; Validate, consume, and execute as one lifecycle-atomic operation. Teardown
;; cannot revoke or replace the issuing frontend between grant consumption and
;; entry into the exact committed execution.
;;
;; INVARIANT: registry-sem is held for the entire thunk (including child
;; execution). Racket semaphores are non-reentrant: the executed thunk and
;; everything it calls must NEVER call back into this module (registration,
;; decision, pending checks, cancellation, grant operations). Violating this
;; invariant self-deadlocks. Concurrent approval operations block until the
;; thunk returns; this serialization is the price of atomic consume+execute
;; and is intentional.
(define (call-with-approval-grant grant digest thunk)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (if (consume-grant-locked! grant digest)
                             (thunk)
                             #f))))

(define (cancel-approval-request! id)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (define request (hash-ref (unbox pending-box) id #f))
                         (cond
                           [(not request) #f]
                           [else
                            (remove-exact-locked! id request)
                            (set-pending-approval-state! request 'cancelled)
                            (async-channel-put (pending-approval-wake request) 'terminal)
                            #t]))))

(define (clear-pending-approvals!)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (cancel-all-locked!)
                         ;; Revoke any grant already returned from the cleared generation.
                         (set-box! generation-box (add1 (unbox generation-box)))))
  (void))

(define (pending-approval-count)
  (call-with-semaphore registry-sem (lambda () (hash-count (unbox pending-box)))))
