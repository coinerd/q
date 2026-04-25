#lang racket

;; test-wiring-run-modes.rkt — Tests for wiring/run-modes.rkt
;;
;; Tests mode-for-config, load-extensions-from-dir!, and module exports.
;; build-runtime-from-cli is tested implicitly via integration tests
;; since it requires full provider setup.

(require rackunit
         rackunit/text-ui
         racket/file
         (only-in "../cli/args.rkt" cli-config)
         (only-in "../wiring/run-modes.rkt"
                  mode-for-config
                  load-extensions-from-dir!
                  build-runtime-from-cli
                  run-interactive
                  run-single-shot
                  run-resume
                  run-json
                  run-rpc
                  make-terminal-subscriber)
         (only-in "../extensions/api.rkt" make-extension-registry extension-registry?))

(define test-wiring-run-modes
  (test-suite "wiring/run-modes"

    ;; --------------------------------------------------
    ;; Test 1: mode-for-config — all command mappings
    ;; --------------------------------------------------
    (test-case "mode-for-config maps 'help to 'help"
      (define cfg (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (check-equal? (mode-for-config cfg) 'help))

    (test-case "mode-for-config maps 'version to 'version"
      (define cfg (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (check-equal? (mode-for-config cfg) 'version))

    (test-case "mode-for-config falls through to mode for chat"
      (define cfg (cli-config 'chat #f #f #f 'tui #f #f #f 10 #f '() #f #f '() #f #f))
      (check-equal? (mode-for-config cfg) 'tui))

    (test-case "mode-for-config maps 'single mode"
      (define cfg (cli-config 'prompt #f "hi" #f 'single #f #f #f 10 #f '() #f #f '() #f #f))
      (check-equal? (mode-for-config cfg) 'single))

    (test-case "mode-for-config maps 'json mode"
      (define cfg (cli-config 'chat #f #f #f 'json #f #f #f 10 #f '() #f #f '() #f #f))
      (check-equal? (mode-for-config cfg) 'json))

    ;; --------------------------------------------------
    ;; Test 2: load-extensions-from-dir! is safe with missing dir
    ;; --------------------------------------------------
    (test-case "load-extensions-from-dir! with non-existent directory is safe"
      (define reg (make-extension-registry))
      (check-not-exn (lambda () (load-extensions-from-dir! reg "/tmp/q-ext-no-such-dir-xyz-999"))))

    ;; --------------------------------------------------
    ;; Test 3: load-extensions-from-dir! with empty directory
    ;; --------------------------------------------------
    (test-case "load-extensions-from-dir! with empty directory is safe"
      (define reg (make-extension-registry))
      (define tmp-dir (make-temporary-file "q-ext-test-~a" 'directory))
      (dynamic-wind void
                    (lambda () (check-not-exn (lambda () (load-extensions-from-dir! reg tmp-dir))))
                    (lambda ()
                      (with-handlers ([exn:fail? void])
                        (delete-directory/files tmp-dir)))))

    ;; --------------------------------------------------
    ;; Test 4: Re-exported symbols are bound
    ;; --------------------------------------------------
    (test-case "run-interactive is bound"
      (check-pred procedure? run-interactive))

    (test-case "run-single-shot is bound"
      (check-pred procedure? run-single-shot))

    (test-case "run-resume is bound"
      (check-pred procedure? run-resume))

    (test-case "run-json is bound"
      (check-pred procedure? run-json))

    (test-case "run-rpc is bound"
      (check-pred procedure? run-rpc))

    (test-case "make-terminal-subscriber is bound"
      (check-pred procedure? make-terminal-subscriber))

    (test-case "build-runtime-from-cli is bound"
      (check-pred procedure? build-runtime-from-cli))))

(run-tests test-wiring-run-modes)
