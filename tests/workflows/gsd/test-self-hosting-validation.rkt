#lang racket

;; tests/workflows/gsd/test-self-hosting-validation.rkt
;; v0.18.5 Wave 1: Self-hosting validation suite

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../extensions/gsd-planning.rkt"
         "../../../extensions/api.rkt"
         (only-in "../../../extensions/loader.rkt" load-extension!)
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt"
         (only-in "../../../tools/builtins/skill-router.rkt" tool-skill-route)
         "../fixtures/mock-provider.rkt"
         "../fixtures/workflow-runner.rkt")

(define (with-temp-dir proc)
  (define dir (make-temporary-file "self-host-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define gsd-ext-path
  (build-path (or (current-load-relative-directory) (current-directory))
              "../../../extensions/gsd-planning.rkt"))

(define test-self-hosting-validation
  (test-suite "Self-Hosting Validation Tests (v0.18.5 Wave 1)"

    (test-case "valid-artifact-name? accepts standard GSD names"
      (check-true (valid-artifact-name? "PLAN"))
      (check-true (valid-artifact-name? "STATE"))
      (check-true (valid-artifact-name? "VALIDATION"))
      (check-true (valid-artifact-name? "SUMMARY"))
      (check-false (valid-artifact-name? "README"))
      (check-false (valid-artifact-name? "")))

    (test-case "skill router returns valid tool-result"
      (define result (tool-skill-route (hasheq 'action "list")))
      (check-true (tool-result? result)))

    (test-case "extension registry loads and lists extensions"
      (define ext-reg (make-extension-registry))
      (define bus (make-event-bus))
      (load-extension! ext-reg gsd-ext-path #:event-bus bus)
      (define exts (list-extensions ext-reg))
      (check-true (>= (length exts) 0)))

    (test-case "event bus publish/subscribe works"
      (define bus (make-event-bus))
      (define received '())
      (subscribe! bus (lambda (evt) (set! received (cons evt received))))
      (publish! bus (hasheq 'type "test" 'data "hello"))
      (check-equal? (length received) 1))

    (test-case "dogfood sample tasks are valid JSON"
      (define tasks-dir (build-path (current-directory) "tests" "workflows" "dogfood" "tasks"))
      (when (directory-exists? tasks-dir)
        (define task-files
          (for/list ([f (in-directory tasks-dir)]
                     #:when (regexp-match? #rx"\\.json$" (path->string f)))
            f))
        (check-true (>= (length task-files) 3))
        (for ([f (in-list task-files)])
          (define spec (call-with-input-file f read-json))
          (check-true (hash? spec))
          (check-true (hash-has-key? spec 'name))
          (check-true (hash-has-key? spec 'prompt)))))))

(run-tests test-self-hosting-validation)
