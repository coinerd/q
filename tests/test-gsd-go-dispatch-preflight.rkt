#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../extensions/gsd/command-handlers.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../util/hook-types.rkt"
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!))
(define-runtime-path source "../extensions/gsd/command-handlers.rkt")
(define (call-with-parked status proc)
  (define dir (make-temporary-file "go-preflight-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (git-quiet! dir "init" "-q")
     (hermetic-identity! dir)
     (make-directory* (build-path dir ".planning/waves"))
     (display-to-file "# Plan: Dispatch\n\n- [Inbox] W0: Dispatch → waves/W0-dispatch.md\n"
                      (build-path dir ".planning/PLAN.md"))
     (display-to-file
      "# W0: Dispatch\n\nStatus: PENDING\n\n## Files\n- File: target.txt\n\n## Verify\nraco test tests\n\n## Done\n- Verified\n"
      (build-path dir ".planning/waves/W0-dispatch.md"))
     (display-to-file "target" (build-path dir "target.txt"))
     (git-quiet! dir "add" ".")
     (git-quiet! dir "commit" "-qm" "fixture")
     (define rec (migrate-campaign! dir))
     (define w (car (campaign-record-waves rec)))
     (set-campaign-wave-status! w status)
     (persist-campaign! dir rec)
     (proc dir rec))
   (lambda () (delete-directory/files dir))))
(module+ test
  (test-case "command preflight never performs authenticated delivery readback"
    (define text (file->string source))
    (define prepare
      (cadr (regexp-match
             #px"(?s:\\(define \\(prepare-go-campaign(.*?)\\(define \\(handle-go-command)"
             text)))
    (check-false (regexp-match? #rx"delivery-pending-wave|delivery-readback" prepare)))
  (for ([status '(done awaiting-delivery)])
    (test-case (format "~a dispatch schedules final delivery without certifying completion" status)
      (call-with-parked
       status
       (lambda (dir rec)
         (define result (handle-go-command dir "/go"))
         (define payload (hook-result-payload result))
         (check-eq? (hook-result-action result) 'amend)
         (check-true (hash-has-key? payload 'campaign-token))
         (check-true (string-contains? (hash-ref payload 'text) "Executing final delivery"))
         (define live (load-campaign-record dir (campaign-plan-id rec)))
         (check-eq? (campaign-wave-status (car (campaign-record-waves live))) status)
         (check-false
          (file-exists?
           (build-path dir ".planning/campaigns" (campaign-plan-id rec) "delivery-w0.rktd"))))))))
