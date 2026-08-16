#lang racket/base

;; @speed fast
;; @suite extensions

;; Regression coverage for campaign-owned TUI session switches. The normal
;; session-shutdown hook must remain active, but its GSD reset is suppressed
;; only while a campaign moves between isolated wave sessions.

(require rackunit
         racket/file
         "../extensions/api.rkt"
         (only-in "../extensions/gsd-planning.rkt" the-extension)
         (only-in "../extensions/gsd/core.rkt"
                  call-with-gsd-campaign-ownership
                  call-with-gsd-owned-session-switch
                  reset-all-gsd-state!)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-current gsm-transition-to!)
         (only-in "../runtime/session/session-switch.rkt" switch-session!))

(test-case "campaign-owned real lifecycle switch preserves GSD until normal shutdown"
  (define dir (make-temporary-file "gsd-lifecycle-~a" 'directory))
  (dynamic-wind
   (lambda ()
     (reset-all-gsd-state!)
     (gsm-transition-to! 'executing))
   (lambda ()
     (define registry (make-extension-registry))
     (register-extension! registry the-extension)
     (call-with-gsd-campaign-ownership 'campaign-test
                                       (lambda ()
                                         (call-with-gsd-owned-session-switch
                                          (lambda ()
                                            (switch-session! #:old-session-id "initiating"
                                                             #:old-extension-registry registry
                                                             #:new-session-id "wave-0"
                                                             #:new-session-dir dir
                                                             #:new-bus #f
                                                             #:new-extension-registry registry
                                                             #:reason 'fork)))))
     (check-eq? (gsm-current) 'executing "campaign wave switch must retain coordinator state")

     ;; Ownership alone does not suppress deliberate resets.
     (call-with-gsd-campaign-ownership 'campaign-test reset-all-gsd-state!)
     (check-eq? (gsm-current) 'idle)
     (gsm-transition-to! 'executing)

     ;; The suppression is scoped: the same real lifecycle outside campaign
     ;; ownership still invokes the extension's ordinary reset behavior.
     (switch-session! #:old-session-id "wave-0"
                      #:old-extension-registry registry
                      #:new-session-id "interactive"
                      #:new-session-dir dir
                      #:new-bus #f
                      #:new-extension-registry registry
                      #:reason 'resume)
     (check-eq? (gsm-current) 'idle))
   (lambda ()
     (reset-all-gsd-state!)
     (delete-directory/files dir #:must-exist? #f))))
