#lang racket/base

;; extensions/gsd/campaign-result.rkt — the campaign terminal result value.
;;
;; Extracted from go-orchestrator.rkt (which still re-provides it) so the
;; delivery checkpoint modules can construct and classify campaign results
;; without depending on the orchestrator. Keeping the struct here also keeps
;; the orchestrator under its documented size target: extract a module instead
;; of growing it.

(struct campaign-result (status completed-waves message) #:transparent)

(provide campaign-result
         campaign-result-status
         campaign-result-completed-waves
         campaign-result-message)
