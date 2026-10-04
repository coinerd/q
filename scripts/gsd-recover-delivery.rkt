#lang racket/base

(require json
         racket/cmdline
         racket/match
         racket/path
         "../extensions/gsd/delivery-recovery.rkt")

(provide main)

(define (parse-nat who text)
  (define n (and text (string->number text)))
  (unless (and (exact-nonnegative-integer? n))
    (error 'gsd-recover-delivery "~a must be a non-negative integer: ~a" who text))
  n)

;; Details are diagnostics, not contracts: they may carry symbols, nested
;; lists or verbatim verify text. Everything is JSON-ified losslessly for
;; the operator (symbols become their names), so a successful dry-run can
;; never crash printing its own verdict.
(define (jsonable v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(list? v) (map jsonable v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)])
       (values (if (symbol? k)
                   (symbol->string k)
                   k)
               (jsonable val)))]
    [(pair? v) (list (jsonable (car v)) (jsonable (cdr v)))]
    [(or (string? v) (boolean? v) (exact-integer? v) (real? v) (null? v)) v]
    [else (format "~a" v)]))

(define (result->hash r)
  (hasheq 'status
          (symbol->string (recovery-result-status r))
          'reason
          (let ([v (recovery-result-reason r)]) (and v (symbol->string v)))
          'actions
          (map symbol->string (recovery-result-actions r))
          'details
          (map jsonable (recovery-result-details r))))

(define (main [argv (current-command-line-arguments)])
  (define root #f)
  (define plan #f)
  (define wave #f)
  (define attempt-id #f)
  (define fence #f)
  (define expected-head #f)
  (define expected-base #f)
  (define old-receipt-head #f)
  (define superseded-journal #f)
  (define verify-mode "repair-tail")
  (define apply? #f)
  (parameterize ([current-command-line-arguments argv])
    (command-line
     #:program "gsd-recover-delivery"
     #:once-each [("--root") v "Campaign root" (set! root v)]
     [("--plan") v "64-hex campaign plan id" (set! plan v)]
     [("--wave") v "Wave index" (set! wave (parse-nat '--wave v))]
     [("--attempt-id") v "Expected live attempt id" (set! attempt-id v)]
     [("--fence") v "Expected campaign/attempt fence" (set! fence (parse-nat '--fence v))]
     [("--expected-head") v "Expected repaired branch head" (set! expected-head v)]
     [("--expected-base") v "Expected attempt base commit" (set! expected-base v)]
     [("--old-receipt-head") v "Expected old receipt head" (set! old-receipt-head v)]
     [("--superseded-journal")
      v
      "Explicit .reconciled-superseded journal copy"
      (set! superseded-journal v)]
     [("--verify-mode")
      v
      "Declared-verify topology: repair-tail (default) or full"
      (set! verify-mode v)]
     [("--apply") "Apply effects; default is dry-run" (set! apply? #t)]))
  (for ([pair (in-list (list (cons '--root root)
                             (cons '--plan plan)
                             (cons '--wave wave)
                             (cons '--attempt-id attempt-id)
                             (cons '--fence fence)
                             (cons '--expected-head expected-head)
                             (cons '--expected-base expected-base)))])
    (unless (cdr pair)
      (error 'gsd-recover-delivery "missing required argument ~a" (car pair))))
  (define mode-symbol
    (cond
      [(string=? verify-mode "repair-tail") 'repair-tail]
      [(string=? verify-mode "full") 'full]
      [else
       (error 'gsd-recover-delivery "--verify-mode must be repair-tail or full: ~a" verify-mode)]))
  (define result
    (recover-delivery! #:root root
                       #:plan plan
                       #:wave wave
                       #:attempt-id attempt-id
                       #:fence fence
                       #:expected-head expected-head
                       #:expected-base expected-base
                       #:old-receipt-head old-receipt-head
                       #:superseded-journal superseded-journal
                       #:verify-mode mode-symbol
                       #:apply? apply?))
  (write-json (result->hash result))
  (newline)
  (unless (memq (recovery-result-status result) '(dry-run applied))
    (exit 1))
  result)

(module+ main
  (main))
