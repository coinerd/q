#lang racket

;; tests/test-run-modes.rkt — tests for wiring/run-modes.rkt (#225)
;;
;; Covers:
;;   mode-for-config: maps cli-config to mode symbol

(require rackunit
         (only-in "../wiring/run-modes.rkt"
                  mode-for-config)
         (only-in "../cli/args.rkt"
                  cli-config))

;; ============================================================
;; mode-for-config tests
;; ============================================================

(test-case "mode-for-config: command='help → 'help"
  (define cfg (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'help))

(test-case "mode-for-config: command='version → 'version"
  (define cfg (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'version))

(test-case "mode-for-config: command='doctor → 'doctor"
  (define cfg (cli-config 'doctor #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'doctor))

(test-case "mode-for-config: command='sessions → 'sessions"
  (define cfg (cli-config 'sessions #f #f #f 'interactive #f #f #f 10 #f '() #f 'list '() #f))
  (check-equal? (mode-for-config cfg) 'sessions))

(test-case "mode-for-config: command='chat with mode='interactive → 'interactive"
  (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'interactive))

(test-case "mode-for-config: command='prompt with mode='single → 'single"
  (define cfg (cli-config 'prompt #f "hello" #f 'single #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'single))

(test-case "mode-for-config: command='chat with mode='json → 'json"
  (define cfg (cli-config 'chat #f #f #f 'json #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'json))

(test-case "mode-for-config: command='chat with mode='rpc → 'rpc"
  (define cfg (cli-config 'chat #f #f #f 'rpc #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'rpc))

(test-case "mode-for-config: command='chat with mode='tui → 'tui"
  (define cfg (cli-config 'chat #f #f #f 'tui #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'tui))

(test-case "mode-for-config: command='resume with mode='interactive → 'interactive"
  (define cfg (cli-config 'resume "sess1" #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))
  (check-equal? (mode-for-config cfg) 'interactive))
