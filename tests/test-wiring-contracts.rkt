#lang racket/base

;; @speed fast
;; test-wiring-contracts.rkt — Verify tightened wiring/run-modes contracts (C2-03)
;; @suite fast

(require rackunit
         racket/port
         (only-in "../cli/args.rkt"
                  cli-config
                  cli-config?
                  cli-config-command
                  cli-config-mode
                  cli-config-model
                  cli-config-project-dir)
         (only-in "../runtime/session/session-config.rkt"
                  session-config?
                  session-config->hash)
         (only-in "../runtime/gsd-query.rkt" current-gsd-mode-query)
         (only-in "../wiring/run-modes.rkt"
                  build-runtime-from-cli
                  mode-for-config
                  reload-config!))

;; ============================================================
;; mode-for-config contract tests
;; ============================================================

(test-case "mode-for-config rejects non-cli-config input"
  (check-exn
   exn:fail:contract?
   (lambda ()
     (mode-for-config "not a config"))))

(test-case "mode-for-config returns symbol for valid cli-config"
  (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f))
  (check-equal? (mode-for-config cfg) 'interactive))

(test-case "mode-for-config returns 'help for help command"
  (define cfg (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f))
  (check-equal? (mode-for-config cfg) 'help))

(test-case "mode-for-config returns 'version for version command"
  (define cfg (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f))
  (check-equal? (mode-for-config cfg) 'version))

;; ============================================================
;; build-runtime-from-cli contract test
;; ============================================================

(test-case "build-runtime-from-cli rejects non-cli-config input"
  (check-exn
   exn:fail:contract?
   (lambda ()
     (build-runtime-from-cli 42))))

(test-case "build-runtime-from-cli rejects hash input"
  (check-exn
   exn:fail:contract?
   (lambda ()
     (build-runtime-from-cli (hasheq 'mode 'interactive)))))

;; ============================================================
;; reload-config! contract test
;; ============================================================

(test-case "reload-config! rejects string input"
  (check-exn
   exn:fail:contract?
   (lambda ()
     (reload-config! "not a session-config"))))
