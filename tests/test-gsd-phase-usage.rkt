#lang racket/base
(require rackunit
         racket/file
         racket/string
         json
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/campaign-budgets.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../util/loop-result.rkt")

(define (with-durable-attempt proc)
  (define dir (make-temporary-file "phase-usage~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define m
       (make-campaign-manifest 1
                               "Usage"
                               '()
                               (list (make-campaign-wave-descriptor 0 "Wave" "w.md" "h"))
                               "c"))
     (define id (campaign-manifest-hash m))
     (define rec
       (make-campaign-record id m (list (make-campaign-wave 0 "Wave" 'pending 0 #f)) #f 0 #f 0 0))
     (begin-attempt! rec 0 1)
     (persist-campaign! dir rec)
     (proc dir id))
   (lambda () (delete-directory/files dir))))

(test-case "durable cumulative phase stamps replace, including final duplicate and retry"
  (with-durable-attempt (lambda (dir id)
                          (for ([u (list (usage-datum 10 4 14 1 #f)
                                         (usage-datum 30 10 40 3 #t)
                                         (usage-datum 30 10 40 3 #t))])
                            (stamp-observed-usage! dir id 0 (cons 0 u)))
                          (define rec (load-campaign-record dir id))
                          (define w (car (campaign-record-waves rec)))
                          (check-equal? (wave-usage-total-tokens w) 40)
                          (check-equal? (wave-usage-input-tokens w) 30)
                          (check-equal? (wave-usage-output-tokens w) 10)
                          (check-equal? (wave-usage-cost-usd w) 3)
                          (check-false (budget-pause-violation? rec 4 50))
                          (set-campaign-wave-status! w 'pending)
                          (begin-attempt! rec 0 2)
                          (persist-campaign! dir rec)
                          (for ([i (in-range 2)])
                            (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 2 1 3 1 #f))))
                          (define final (car (campaign-record-waves (load-campaign-record dir id))))
                          (check-equal? (wave-usage-total-tokens final) 43)
                          (check-equal? (wave-usage-cost-usd final) 4))))

(test-case "strict stamping propagates unavailable persistence; legacy remains best effort"
  (define dir (make-temporary-file "phase-strict~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (check-not-exn (lambda () (stamp-observed-usage! dir "absent" 0 #f)))
                  (check-exn exn:fail?
                             (lambda () (stamp-observed-usage! dir "absent" 0 #f #:strict? #t))))
                (lambda () (delete-directory/files dir))))

(test-case "durable repeated unknown stamps count one missing attempt, not zero usage"
  (with-durable-attempt (lambda (dir id)
                          (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 10 4 14 1 #f)))
                          (for ([i (in-range 3)])
                            (stamp-observed-usage! dir id 0 (cons 0 #f)))
                          (define w (car (campaign-record-waves (load-campaign-record dir id))))
                          (check-equal? (wave-usage-missing-attempts w) 1)
                          ;; Observed tokens/cost are REAL spend: the unknown
                          ;; restamp retains them (never a discarded total);
                          ;; missing-attempts carries the coverage honesty.
                          (check-equal? (wave-usage-total-tokens w) 14)
                          (check-equal? (wave-usage-cost-usd w) 1))))

(test-case "a never-informed attempt stays entirely unknown"
  (with-durable-attempt (lambda (dir id)
                          (stamp-observed-usage! dir id 0 (cons 0 #f))
                          (define w (car (campaign-record-waves (load-campaign-record dir id))))
                          (check-false (wave-usage-total-tokens w))
                          (check-false (wave-usage-cost-usd w))
                          (check-equal? (wave-usage-missing-attempts w) 1))))

(test-case "durable partial retry keeps unknown cost coverage and token known-sums"
  (with-durable-attempt (lambda (dir id)
                          (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 10 #f 14 #f #f)))
                          (define rec (load-campaign-record dir id))
                          (define w (car (campaign-record-waves rec)))
                          (set-campaign-wave-status! w 'pending)
                          (begin-attempt! rec 0 2)
                          (persist-campaign! dir rec)
                          (for ([i (in-range 3)])
                            (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 2 1 3 1 #f))))
                          (define final (load-campaign-record dir id))
                          (define fw (car (campaign-record-waves final)))
                          (check-equal? (wave-usage-total-tokens fw) 17)
                          (check-equal? (wave-usage-input-tokens fw) 12)
                          (check-equal? (wave-usage-output-tokens fw) 1)
                          ;; The durable wave total RETAINS the known $1 from
                          ;; the retried attempt; unknown coverage lives in
                          ;; 'prior-cost-unknown, so the summary reports cost
                          ;; #f and ceilings cannot trip on unprovable spend.
                          (check-equal? (wave-usage-cost-usd fw) 1)
                          (check-eq? (wave-usage-source fw) 'prior-cost-unknown)
                          (check-false (usage-summary-cost-usd (campaign-usage-summary final)))
                          (check-false (budget-pause-violation? final 0.5 #f)))))

(test-case "durable zero cost is known and repeated stamps remain zero"
  (with-durable-attempt (lambda (dir id)
                          (for ([i (in-range 2)])
                            (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 0 0 0 0 #f))))
                          (define rec (load-campaign-record dir id))
                          (check-equal? (wave-usage-cost-usd (car (campaign-record-waves rec))) 0)
                          (check-= (usage-summary-cost-usd (campaign-usage-summary rec)) 0 0))))

(test-case "same-attempt unknown restamp retains its own known spend"
  ;; Reviewer follow-up: stamp $5, then a usage report with unknown cost.
  ;; The retained wave spend must stay $5 (the observed bill is real), and
  ;; a later cumulative known replacement must not double-count it.
  (with-durable-attempt
   (lambda (dir id)
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 10 4 14 5 #f)))
     (define w1 (car (campaign-record-waves (load-campaign-record dir id))))
     (check-equal? (wave-usage-cost-usd w1) 5)
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 30 10 40 #f #f)))
     (define w2 (car (campaign-record-waves (load-campaign-record dir id))))
     (check-equal? (wave-usage-cost-usd w2) 5 "own known spend retained")
     (check-equal? (wave-usage-total-tokens w2) 40 "known tokens replace")
     (check-eq? (wave-usage-source w2) 'prior-cost-unknown)
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 30 10 40 6 #f)))
     (define w3 (car (campaign-record-waves (load-campaign-record dir id))))
     (check-equal? (wave-usage-cost-usd w3) 6 "cumulative replacement, not 5+6"))))

(test-case "cross-attempt unknown cost never discards prior known spend"
  ;; Reviewer scenario: attempt1 costs $5; attempt2 reports unknown cost
  ;; then $2. The wave must retain $7 (or honestly unknown), never $2 —
  ;; a lost cumulative total could admit spend past a cost ceiling.
  (with-durable-attempt
   (lambda (dir id)
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 10 4 14 5 #f)))
     (define rec (load-campaign-record dir id))
     (define w (car (campaign-record-waves rec)))
     (set-campaign-wave-status! w 'pending)
     (begin-attempt! rec 0 2)
     (persist-campaign! dir rec)
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 20 6 26 #f #f)))
     (define mid (car (campaign-record-waves (load-campaign-record dir id))))
     (check-equal? (wave-usage-cost-usd mid) 5 "prior attempt's $5 survives")
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 20 6 26 2 #f)))
     (define final (load-campaign-record dir id))
     (define fw (car (campaign-record-waves final)))
     (check-equal? (wave-usage-cost-usd fw) 7)
     (check-equal? (wave-usage-total-tokens fw) 40)
     (check-eq? (wave-usage-source fw) 'prior-cost-unknown)
     ;; Coverage stays honest: the unknown attempt keeps the summary cost
     ;; unknown so a max-cost ceiling cannot trip on partial knowledge.
     (check-false (usage-summary-cost-usd (campaign-usage-summary final)))
     (check-false (budget-pause-violation? final 0.5 #f))
     ;; Once every counted attempt's cost is known again the total is real.
     (stamp-observed-usage! dir id 0 (cons 0 (usage-datum 20 6 26 2 #f)))
     (check-equal? (wave-usage-cost-usd (car (campaign-record-waves (load-campaign-record dir id))))
                   7))))

(define (result usage)
  (make-loop-result '() 'done (hash 'usage usage)))
(define full (result (hash 'prompt_tokens 10 'completion_tokens 4 'total_tokens 14 'cost 0.2)))
(define second
  (result (hash 'prompt_tokens 20 'completion_tokens 6 'total_tokens 26 'cost 0.3 'estimated? #t)))

(test-case "two phases accumulate; peeking does not drain; next attempt starts clean"
  (define b (box #f))
  (parameterize ([current-campaign-usage-observation b])
    (record-usage-observation! 1 full)
    (check-equal? (unbox b) (cons 1 (loop-result->usage-datum full)))
    (record-usage-observation! 1 second)
    (define cumulative (cons 1 (usage-datum 30 10 40 0.5 #t)))
    (check-equal? (unbox b) cumulative)
    (check-equal? (take-usage-observation!) cumulative)
    (check-false (unbox b))
    (check-false (take-usage-observation!))
    (record-usage-observation! 1 full)
    (check-equal? (take-usage-observation!) (cons 1 (loop-result->usage-datum full)))))

(test-case "missing invocation poisons totals in either order until drain"
  (for ([missing (in-list (list #f (result #f) (result (hash))))])
    (for ([runs (in-list (list (list full missing second) (list missing full second)))])
      (parameterize ([current-campaign-usage-observation (box #f)])
        (for ([run (in-list runs)])
          (record-usage-observation! 2 run))
        (check-equal? (take-usage-observation!) (cons 2 #f))
        (record-usage-observation! 2 full)
        (check-equal? (take-usage-observation!) (cons 2 (loop-result->usage-datum full)))))))

(test-case "partial fields remain unknown rather than counting as zero"
  (parameterize ([current-campaign-usage-observation (box #f)])
    (record-usage-observation! 3 full)
    (record-usage-observation! 3 (result (hash 'prompt_tokens 2 'estimated? #t)))
    (check-equal? (take-usage-observation!) (cons 3 (usage-datum 12 #f 16 #f #t))))
  (parameterize ([current-campaign-usage-observation (box #f)])
    (record-usage-observation! 3 (result (hash 'cost 0.1)))
    (record-usage-observation! 3 second)
    (define u (cdr (take-usage-observation!)))
    (check-false (usage-datum-input-tokens u))
    (check-false (usage-datum-output-tokens u))
    (check-false (usage-datum-total-tokens u))
    (check-= (usage-datum-cost-usd u) 0.4 0.000001)))

(test-case "mismatched waves invalidate the attempt, not mix totals"
  (parameterize ([current-campaign-usage-observation (box #f)])
    (record-usage-observation! 4 full)
    (record-usage-observation! 5 second)
    (check-false (cdr (unbox (current-campaign-usage-observation))))
    (record-usage-observation! 4 full)
    (check-false (cdr (take-usage-observation!)))))

(test-case "strict budget pause propagates persistence failure; legacy stays best effort"
  ;; A ceiling crossing whose durable pause cannot be recorded (corrupt
  ;; record) must STOP an owned boundary dispatch, not silently allow the
  ;; next phase to spend past the ceiling.
  (define dir (make-temporary-file "pause-strict~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define m
       (make-campaign-manifest 1 "P" '() (list (make-campaign-wave-descriptor 0 "W" "w.md" "h")) "c"))
     (define id (campaign-manifest-hash m))
     (define rec
       (make-campaign-record id m (list (make-campaign-wave 0 "W" 'pending 0 #f)) #f 0 #f 0 0))
     (begin-attempt! rec 0 1)
     (stamp-wave-usage! rec 0 (usage-datum 100 40 140 9 #f))
     (persist-campaign! dir rec)
     ;; Real project settings in the fixture root set the token ceiling.
     (make-directory* (build-path dir ".q"))
     (call-with-output-file
      (build-path dir ".q" "config.json")
      (lambda (out) (write-json (hasheq 'gsd (hasheq 'campaign (hasheq 'max-tokens 50))) out))
      #:exists 'truncate)
     ;; Crossed ceiling (140 > 50): ordinary boundary may fail best effort,
     ;; owned boundary must surface the failure.
     (check-not-exn (lambda () (pause-campaign-if-over-budget! dir id)))
     (make-directory* (build-path dir ".planning" "campaigns"))
     (call-with-output-file (build-path dir ".planning" "campaigns" (string-append id ".rktd"))
                            (lambda (out) (display "#<<corrupt" out))
                            #:exists 'truncate)
     (check-exn exn:fail? (lambda () (pause-campaign-if-over-budget! dir id #:strict? #t)))
     ;; With a loadable record the strict pause persists the durable
     ;; ceiling pause and reports the named reason.
     (delete-file (build-path dir ".planning" "campaigns" (string-append id ".rktd")))
     (persist-campaign! dir rec)
     (define why (pause-campaign-if-over-budget! dir id #:strict? #t))
     (check-true (and (string? why) (string-contains? why "gsd.campaign.max-tokens")))
     (check-not-false (campaign-record-budget-pause (load-campaign-record dir id))))
   (lambda () (delete-directory/files dir))))
