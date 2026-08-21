#lang racket/base

;; @speed fast
;; @suite fast
;; @boundary unit

(require rackunit
         rackunit/text-ui
         "../scripts/run-tests/full-regression-status.rkt")

(define (record name #:verdict [verdict "pass"] #:files [files 1]
                #:fail [fail 0] #:timeout [timeout 0])
  (lane-record name
               (hasheq 'suite "all"
                       'profile "ci"
                       'verdict verdict
                       'run_summary
                       (hasheq 'shard (hasheq 'index name 'total 6)
                               'execution_mode "subprocess"
                               'file_count files
                               'pass (if (equal? verdict "pass") files 0)
                               'fail fail
                               'timeout timeout
                               'skip 0
                               'wall_clock_seconds 1.0))
               #f))

(define (passing-records prefix count)
  (for/list ([index (in-range count)])
    (record (format "~a-~a" prefix index))))

(define (summary #:shards [shards (passing-records "shard" 6)]
                 #:workflows [workflows (list (record "workflows"))]
                 #:platform [platform (list (record "platform"))]
                 #:test-result [test-result "success"]
                 #:workflows-result [workflows-result "success"]
                 #:platform-result [platform-result "success"])
  (evaluate-full-regression
   #:shard-records shards
   #:workflows-records workflows
   #:platform-records platform
   #:test-result test-result
   #:workflows-result workflows-result
   #:platform-result platform-result))

(define (status result) (json-ref result 'status))
(define (lane result key)
  (json-ref (json-ref result 'required_lanes) key))

(define suite
  (test-suite
   "Full-regression L4 status contract"

   (test-case "complete all-lane evidence is pass"
     (define result (summary))
     (check-equal? (status result) "pass")
     (check-equal? (json-ref (lane result 'linux_shards) 'status) "pass")
     (check-equal? (json-ref (lane result 'workflows) 'status) "pass")
     (check-equal? (json-ref (lane result 'platform) 'status) "pass"))

   (test-case "one failed Linux shard is fail"
     (define shards (passing-records "shard" 5))
     (define failed-shard (record "shard-failed" #:verdict "fail" #:fail 1))
     (define result (summary #:shards (append shards (list failed-shard))
                             #:test-result "failure"))
     (check-equal? (status result) "fail")
     (check-equal? (json-ref (lane result 'linux_shards) 'status) "fail"))

   (test-case "one failed workflows suite is fail"
     (define result (summary #:workflows (list (record "workflows" #:verdict "fail" #:fail 1))
                             #:workflows-result "failure"))
     (check-equal? (status result) "fail")
     (check-equal? (json-ref (lane result 'workflows) 'status) "fail"))

   (test-case "one failed platform suite is fail even when Linux is green"
     (define result (summary #:platform (list (record "platform" #:verdict "fail" #:fail 1))
                             #:platform-result "failure"))
     (check-equal? (status result) "fail")
     (check-equal? (json-ref (lane result 'platform) 'status) "fail"))

   (test-case "missing platform artifact is timeout, never pass"
     (define result (summary #:platform '() #:platform-result "success"))
     (check-equal? (status result) "timeout")
     (check-equal? (json-ref (lane result 'platform) 'status) "timeout"))

   (test-case "malformed platform JSON is timeout, never pass"
     (define malformed (lane-record "platform.json" (hasheq 'verdict "pass") #f))
     (define result (summary #:platform (list malformed)))
     (check-equal? (status result) "timeout")
     (check-equal? (json-ref (lane result 'platform) 'status) "timeout"))

   (test-case "cancelled platform job is timeout, never pass"
     (define result (summary #:platform-result "cancelled"))
     (check-equal? (status result) "timeout")
     (check-equal? (json-ref (lane result 'platform) 'status) "timeout"))

   (test-case "unexpectedly skipped required platform job is fail"
     (define result (summary #:platform-result "skipped"))
     (check-equal? (status result) "fail")
     (check-equal? (json-ref (lane result 'platform) 'status) "fail"))

   (test-case "missing Linux shard is timeout, never pass"
     (define result (summary #:shards (passing-records "shard" 5)))
     (check-equal? (status result) "timeout")
     (check-equal? (json-ref (lane result 'linux_shards) 'status) "timeout"))))

(module+ test
  (define failures (run-tests suite))
  (exit (if (zero? failures) 0 1)))
