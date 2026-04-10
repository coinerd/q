#lang racket

;; test-subprocess.rkt — focused tests for sandbox/subprocess.rkt and limits.rkt

(require rackunit
         rackunit/text-ui
         "../sandbox/limits.rkt"
         "../sandbox/subprocess.rkt")

(define-test-suite subprocess-tests

  ;; --- limits.rkt standalone constants ---

  (test-case "default-timeout-seconds is 300"
    (check-equal? default-timeout-seconds 300))

  (test-case "default-max-output-bytes is 10 MB"
    (check-equal? default-max-output-bytes 10485760))

  ;; --- with-resource-limits ---

  (test-case "with-resource-limits: successful execution returns result"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) 42)
                            #:timeout 5))
    (check-equal? result 42)
    (check-false timed-out?))

  (test-case "with-resource-limits: timeout triggers"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) (sleep 30) 99)
                            #:timeout 1))
    (check-true timed-out?))

  ;; --- run-subprocess with #:timeout ---

  (test-case "run-subprocess: simple echo"
    (define result (run-subprocess "echo" #:args '("hello world")))
    (check-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result))
    (check regexp-match? #rx"hello world" (subprocess-result-stdout result)))

  (test-case "run-subprocess: #:timeout override"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 30")
                                    #:timeout 1))
    (check-true (subprocess-result-timed-out? result))
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "run-subprocess: non-zero exit code"
    (define result (run-subprocess "false"))
    (check-not-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result)))

  (test-case "run-subprocess: output truncation marker"
    ;; Generate large output with a very small limit
    (define strict (exec-limits 10 100 536870912 10))
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "seq 1 10000")
                                    #:limits strict))
    (check-true (string-contains? (subprocess-result-stdout result)
                                  "[output truncated at 100 bytes]")))

  (test-case "run-subprocess: process killed on timeout"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 60")
                                    #:timeout 1))
    (check-true (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) -1)
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "kill-subprocess! does not crash"
    (define cust (make-custodian))
    (check-not-exn (lambda () (kill-subprocess! cust))))

  ;; --- SEC-06: Environment sanitization ---

  (test-case "sanitize-env strips API_KEY pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"MY_API_KEY" #"secret123")
    (environment-variables-set! env #"PATH" #"/usr/bin")
    (define clean (sanitize-env env))
    (check-equal? (environment-variables-ref clean #"PATH") #"/usr/bin")
    (check-false (environment-variables-ref clean #"MY_API_KEY")))

  (test-case "sanitize-env strips SECRET pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"MY_SECRET" #"hush")
    (environment-variables-set! env #"HOME" #"/home/user")
    (define clean (sanitize-env env))
    (check-equal? (environment-variables-ref clean #"HOME") #"/home/user")
    (check-false (environment-variables-ref clean #"MY_SECRET")))

  (test-case "sanitize-env strips TOKEN pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"GITHUB_TOKEN" #"ghp_abc")
    (environment-variables-set! env #"LANG" #"en_US.UTF-8")
    (define clean (sanitize-env env))
    (check-equal? (environment-variables-ref clean #"LANG") #"en_US.UTF-8")
    (check-false (environment-variables-ref clean #"GITHUB_TOKEN")))

  (test-case "sanitize-env strips PASSWORD pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"DB_PASSWORD" #"pass123")
    (define clean (sanitize-env env))
    (check-false (environment-variables-ref clean #"DB_PASSWORD")))

  (test-case "sanitize-env strips CREDENTIAL pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"AWS_CREDENTIAL" #"cred")
    (define clean (sanitize-env env))
    (check-false (environment-variables-ref clean #"AWS_CREDENTIAL")))

  (test-case "sanitize-env strips AUTH pattern"
    (define env (make-environment-variables))
    (environment-variables-set! env #"MY_AUTH_TOKEN" #"tok")
    (define clean (sanitize-env env))
    (check-false (environment-variables-ref clean #"MY_AUTH_TOKEN")))

  (test-case "sanitize-env is case-insensitive"
    (define env (make-environment-variables))
    (environment-variables-set! env #"api_key" #"lower")
    (environment-variables-set! env #"Api_Key" #"mixed")
    (define clean (sanitize-env env))
    (check-false (environment-variables-ref clean #"api_key"))
    (check-false (environment-variables-ref clean #"Api_Key")))

  (test-case "sanitize-env preserves safe vars like PATH and HOME"
    (define env (make-environment-variables))
    (environment-variables-set! env #"PATH" #"/usr/bin")
    (environment-variables-set! env #"HOME" #"/home/user")
    (environment-variables-set! env #"TERM" #"xterm")
    (define clean (sanitize-env env))
    (check-equal? (environment-variables-ref clean #"PATH") #"/usr/bin")
    (check-equal? (environment-variables-ref clean #"HOME") #"/home/user")
    (check-equal? (environment-variables-ref clean #"TERM") #"xterm"))

  (test-case "secret-env-var? detects common secret patterns"
    (check-true (secret-env-var? "API_KEY"))
    (check-true (secret-env-var? "MY_APIKEY"))
    (check-true (secret-env-var? "SECRET_TOKEN"))
    (check-true (secret-env-var? "PASSWORD"))
    (check-true (secret-env-var? "AWS_CREDENTIAL"))
    (check-true (secret-env-var? "AUTH_HEADER"))
    (check-false (secret-env-var? "PATH"))
    (check-false (secret-env-var? "HOME"))
    (check-false (secret-env-var? "TERM")))

  (test-case "run-subprocess default env strips secrets"
    ;; Set a sensitive env var, run a subprocess that prints env,
    ;; and verify the secret is NOT present in the subprocess env.
    (putenv "Q_TEST_SECRET_API_KEY" "should-be-stripped")
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "env")))
    (check-equal? (subprocess-result-exit-code result) 0)
    (check-false (string-contains? (subprocess-result-stdout result)
                                    "should-be-stripped")
                 "secret env var should not appear in subprocess env"))
)

(run-tests subprocess-tests)
