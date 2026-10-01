#lang racket/base
;; Immutable per-verification diagnostics (v1.00.33 audit, §17.3 design).
;; ONE JSON file per owned verification run, capturing identity-before and
;; outcome-after at the single wrapper boundary (verify-with-delivery-receipt):
;; authentic campaign/wave/attempt/fence/base/head, expected vs resolved
;; branch (a literal unresolved branch is recorded, never dropped), authentic
;; repo/git roots, before/after snapshot trees, and the truthful receipt
;; decision taken by the wrapper's own branches.
;;
;; These records are DIAGNOSTICS ONLY. They live outside the delivery journal,
;; are ignored by every journal/campaign parser, never call receipt or marker
;; functions, and can never satisfy receipt validation, clear remote-pending
;; markers, or alter ladder state. They are not delivery proof.
;;
;; Publication is atomic and no-replace WITHOUT any rename and WITHOUT any
;; pre-write absence check (checking absence before an ordinary rename is a
;; TOCTOU race and guarantees nothing). The completed, exclusively created
;; temp file is published with a same-filesystem hard link (link(2) via the
;; libc FFI). link(2) fails rather than overwrites an existing destination,
;; so the kernel — not a check — provides the no-replace guarantee. On
;; EEXIST-style failure the random 128-bit verification ID is regenerated a
;; bounded number of times; every other failure, and any diagnostic failure
;; at all, is contained: publish-verification-diagnostic! never raises, so a
;; diagnostic problem can never change the verifier's own result or error.
(require ffi/unsafe
         json
         racket/file
         racket/list
         racket/path
         racket/random
         racket/string)
(provide verification-diagnostics-dir
         valid-verification-diagnostic?
         make-verification-diagnostic
         publish-verification-diagnostic!
         load-verification-diagnostics
         current-gsd-diagnostic-id-source
         current-gsd-diagnostic-logger)

;; Ordinary raised values are contained, including non-exception values.
;; Breaks remain cancellation signals and are never swallowed by diagnostics.
(define (diagnostic-failure? v)
  (not (exn:break? v)))
(define (diagnostic-failure-message v)
  (if (exn? v)
      (exn-message v)
      "non-exception diagnostic failure"))
(define max-publish-retries 5)
;; Injectable failure seam; callers contain errors from logging too.
(define current-gsd-diagnostic-logger
  (make-parameter (lambda (message) (log-warning "verification diagnostic contained: ~a" message))))

;; libc link(2). Resolved lazily at load; a host without the symbol fails
;; closed below (contained, never renamed, never truncated). unlink(2) is
;; only used on our own exclusive temp files.
(define c-link
  (with-handlers ([diagnostic-failure? (lambda (_) #f)])
    (get-ffi-obj "link" (ffi-lib #f) (_fun #:save-errno 'posix _path _path -> _int))))
(define c-unlink
  (with-handlers ([diagnostic-failure? (lambda (_) #f)])
    (get-ffi-obj "unlink" (ffi-lib #f) (_fun _path -> _int))))

;; Injectable for deterministic collision/regeneration tests. Production
;; default: 128 bits from a cryptographic source, rendered as 32 lowercase
;; hex characters. The SAME string is embedded in the record as
;; 'verification-id and used as the filename suffix, so file name and
;; content are correlated and a published file is self-identifying.
(define (random-verification-id)
  (apply string-append
         (for/list ([b (in-bytes (crypto-random-bytes 16))])
           (define h (number->string b 16))
           (if (= (string-length h) 1)
               (string-append "0" h)
               h))))
(define current-gsd-diagnostic-id-source (make-parameter random-verification-id))

(define (hex? s n)
  (and (string? s) (= (string-length s) n) (regexp-match? #px"^[0-9a-f]+$" s)))
(define (text? s)
  (and (string? s) (positive? (string-length s))))
(define (hex-or-empty? s n)
  (or (equal? s "") (hex? s n)))

;; Absent values are recorded as "" plus an explicit *-bound? boolean, never
;; as JSON null: read-json maps null to the 'null symbol, which would poison
;; later equality checks.
(define (valid-verification-diagnostic? d)
  (and (hash? d)
       (equal? (hash-ref d 'schema-version #f) 2)
       (equal? (hash-ref d 'kind #f) "verification-diagnostic")
       (hex? (hash-ref d 'plan-id #f) 64)
       (hex? (hash-ref d 'verification-id #f) 32)
       (exact-nonnegative-integer? (hash-ref d 'wave #f))
       (string? (hash-ref d 'attempt-id #f))
       (exact-integer? (hash-ref d 'attempt-fence #f))
       (boolean? (hash-ref d 'attempt-fence-bound? 'missing))
       (hex-or-empty? (hash-ref d 'base #f) 40)
       (boolean? (hash-ref d 'base-bound? 'missing))
       (string? (hash-ref d 'expected-branch #f))
       (string? (hash-ref d 'resolved-branch #f))
       (boolean? (hash-ref d 'branch-resolved? 'missing))
       (string? (hash-ref d 'repo-root #f))
       (string? (hash-ref d 'git-root #f))
       (hex-or-empty? (hash-ref d 'head #f) 40)
       (hex-or-empty? (hash-ref d 'snapshot-before-tree #f) 40)
       (hex-or-empty? (hash-ref d 'snapshot-after-tree #f) 40)
       (exact-integer? (hash-ref d 'started-at #f))
       (exact-integer? (hash-ref d 'finished-at #f))
       (member (hash-ref d 'outcome #f) '("approved" "rejected" "error"))
       (boolean? (hash-ref d 'snapshot-before? 'missing))
       (boolean? (hash-ref d 'snapshot-unchanged? 'missing))
       (text? (hash-ref d 'receipt-decision #f))
       (string? (hash-ref d 'receipt-reason #f))
       (string? (hash-ref d 'exception #f))))

;; Canonical record constructor: absent values are "" plus explicit
;; *-bound? booleans (never JSON null). The verification-id field is NOT set
;; here — it is injected at publish time from the freshly drawn ID and the
;; complete record is re-validated (fail closed) before anything is written.
(define (make-verification-diagnostic plan-id
                                      wave
                                      attempt-id
                                      attempt-fence
                                      attempt-fence-bound?
                                      base
                                      base-bound?
                                      expected-branch
                                      resolved-branch
                                      branch-resolved?
                                      repo-root
                                      git-root
                                      head
                                      before-tree
                                      after-tree
                                      started-at
                                      finished-at
                                      outcome
                                      snapshot-before?
                                      snapshot-unchanged?
                                      receipt-decision
                                      receipt-reason
                                      exception)
  (hasheq 'schema-version
          2
          'kind
          "verification-diagnostic"
          'plan-id
          plan-id
          'wave
          wave
          'attempt-id
          (or attempt-id "")
          'attempt-fence
          (or attempt-fence -1)
          'attempt-fence-bound?
          (and attempt-fence-bound? #t)
          'base
          (or base "")
          'base-bound?
          (and base-bound? #t)
          'expected-branch
          (or expected-branch "")
          'resolved-branch
          (or resolved-branch "")
          'branch-resolved?
          (and branch-resolved? #t)
          'repo-root
          (or repo-root "")
          'git-root
          (or git-root "")
          'head
          (or head "")
          'snapshot-before-tree
          (or before-tree "")
          'snapshot-after-tree
          (or after-tree "")
          'started-at
          (or started-at 0)
          'finished-at
          (or finished-at 0)
          'outcome
          outcome
          'snapshot-before?
          (and snapshot-before? #t)
          'snapshot-unchanged?
          (and snapshot-unchanged? #t)
          'receipt-decision
          receipt-decision
          'receipt-reason
          (or receipt-reason "")
          'exception
          (or exception "")))

(define (safe-name-part s)
  (if (and (string? s) (positive? (string-length s)) (regexp-match? #px"^[A-Za-z0-9._-]+$" s))
      s
      "attempt"))

(define (diagnostic-filename plan wave attempt-id fence started-at verification-id)
  (format "~a-w~a-a~a-f~a-~a-~a.json"
          plan
          wave
          (safe-name-part attempt-id)
          fence
          started-at
          verification-id))

(define (verification-diagnostics-dir root plan)
  (unless (hex? plan 64)
    (error 'verification-diagnostics "invalid campaign identity"))
  (build-path (path->complete-path root) ".planning" "campaigns" plan "verifications"))

(define (delete-quietly p)
  (with-handlers ([diagnostic-failure? (lambda (_) (void))])
    (and c-unlink (zero? (c-unlink p)) (void))))

;; Publish ONE diagnostic record. Atomic no-replace by kernel semantics:
;; 1. draw the 128-bit verification ID; embed it and re-validate (fail
;;    closed — a malformed record is never written);
;; 2. write the JSON into an EXCLUSIVELY created unique temp file in the
;;    same directory (same filesystem; no predictable path, no truncation
;;    of any pre-existing name — make-temporary-file creates O_EXCL);
;; 3. hard-link temp -> final. link(2) never replaces an existing
;;    destination: a collision returns nonzero with the destination intact,
;;    classified by destination existence AFTER the failed call (the
;;    no-overwrite guarantee comes from the kernel, not from this
;;    classification). Nonce collisions (same requested identity) are
;;    contained immediately as duplicate publications; anonymous collisions
;;    regenerate the ID, bounded by max-publish-retries;
;; 4. unlink the temp on every path (the final keeps the content via the
;;    hard link).
;; Returns (cons 'published path) or (cons 'contained reason); never raises.
(define (publish-verification-diagnostic! root plan record #:nonce [nonce #f])
  (with-handlers ([diagnostic-failure? (lambda (e)
                                         (cons 'contained
                                               (format "diagnostic contained: ~a"
                                                       (diagnostic-failure-message e))))])
    (unless (or (not nonce) (hex? nonce 32))
      (error 'verification-diagnostics "nonce must be a 32-hex verification id or absent"))
    (define dir (verification-diagnostics-dir root plan))
    ;; Refuse symlinked path components (including dangling links), rather
    ;; than permitting diagnostic publication outside the supplied root.
    (let loop ([p dir])
      (when (link-exists? p)
        (error 'verification-diagnostics "symlinked diagnostics path refused"))
      (define parent (simplify-path (build-path p 'up)))
      (unless (equal? p parent)
        (loop parent)))
    (unless (equal? (hash-ref record 'plan-id #f) plan)
      (error 'verification-diagnostics "record campaign identity mismatch"))
    (make-directory* dir)
    (define attempt-id (hash-ref record 'attempt-id))
    (define fence (hash-ref record 'attempt-fence))
    (define started-at (hash-ref record 'started-at))
    (define wave (hash-ref record 'wave))
    (or (for/or ([i (in-range max-publish-retries)])
          (define id ((current-gsd-diagnostic-id-source)))
          (unless (hex? id 32)
            (error 'verification-diagnostics "verification id source produced a non-32-hex id"))
          (define nonce-use? (and (zero? i) (text? nonce)))
          (define verification-id (if nonce-use? nonce id))
          (define final
            (build-path dir
                        (diagnostic-filename plan wave attempt-id fence started-at verification-id)))
          ;; Fail closed on the complete, ID-injected record before any write.
          (unless (valid-verification-diagnostic? (hash-set record 'verification-id verification-id))
            (error 'verification-diagnostics "invalid verification diagnostic; refusing to write"))
          (cond
            [(and nonce-use? (file-exists? final))
             ;; Duplicate publication of the SAME requested identity: contained,
             ;; never regenerated, never overwritten.
             (cons 'contained (format "diagnostic contained: destination collision: ~a" final))]
            [else
             (define tmp (build-path dir (string-append "vc-" (random-verification-id) ".json.tmp")))
             ;; #:exists 'error creates the output exclusively; no truncate or
             ;; reopening of an already-created temporary path is permitted.
             (define temp-owned? #f)
             (define r #f)
             (define errno #f)
             (dynamic-wind void
                           (lambda ()
                             (call-with-output-file
                              tmp
                              (lambda (out)
                                (set! temp-owned? #t)
                                (write-json (hash-set record 'verification-id verification-id) out)
                                (newline out))
                              #:exists 'error)
                             (when c-link
                               (set! r (c-link tmp final))
                               (set! errno (saved-errno))))
                           (lambda ()
                             (when temp-owned?
                               (delete-quietly tmp))))
             (cond
               [(and r (zero? r)) (cons 'published (path->string final))]
               [(not r) (error 'verification-diagnostics "hard-link publication unavailable")]
               [(equal? errno 17)
                ;; POSIX EEXIST: kernel no-replace collision (checked AFTER a failed link —
                ;; no precheck guarantees anything). A requested-identity
                ;; duplicate stays contained; an anonymous collision regenerates.
                (if nonce-use?
                    (cons 'contained (format "diagnostic contained: destination collision: ~a" final))
                    #f)]
               [else (error 'verification-diagnostics "diagnostic publication failed")])]))
        (cons 'contained "diagnostic contained: destination collision exhausted retries"))))

;; Tolerant loader: returns parsed records for *.json files (never *.tmp,
;; never subdirectories), sorted by filename; malformed files are skipped.
;; Unknown extra fields are tolerated (forward-compatible).
(define (load-verification-diagnostics root plan)
  (define dir (verification-diagnostics-dir root plan))
  (if (not (directory-exists? dir))
      '()
      (map cdr
           (sort (filter values
                         (for/list ([name (in-list (directory-list dir))]
                                    #:when (let ([p (build-path dir name)])
                                             (and (file-exists? p)
                                                  (equal? (path-get-extension name) #".json"))))
                           (with-handlers ([diagnostic-failure? (lambda (_) #f)])
                             (define v (call-with-input-file (build-path dir name) read-json))
                             (and (valid-verification-diagnostic? v)
                                  (equal? (hash-ref v 'plan-id) plan)
                                  (string-suffix? (path->string name)
                                                  (string-append (hash-ref v 'verification-id)
                                                                 ".json"))
                                  (cons (path->string name) v)))))
                 string<?
                 #:key car))))
