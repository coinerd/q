#lang racket/base

;; Least-authority TUI adapter for the frontend-neutral approval broker.
;; Frontends may own a lifecycle lease, read its authoritative presentation,
;; test liveness, and submit an exact-digest decision. Registration, awaiting,
;; grants, cancellation, and registry mutation remain runtime-only.

(require (only-in "../runtime/approval/broker.rkt"
                  approval-channel?
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  approval-request-view
                  approval-request-pending?
                  approval-decide!))

(provide approval-channel?
         make-approval-channel
         set-approval-channel!
         clear-approval-channel!
         current-approval-channel
         approval-request-view
         approval-request-pending?
         approval-decide!)
