#lang racket

;; tests/test-browser-adapter-contracts.rkt — Adapter contract boundary tests
;;
;; Validates that adapter contracts reject invalid inputs.

(require rackunit
         "../browser/adapter.rkt"
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (noop . args) 'ok)

(define (make-valid-adapter)
  (make-browser-adapter
   #:open noop #:close noop #:navigate noop
   #:observe noop #:act noop #:screenshot noop))

;; ---------------------------------------------------------------------------
;; make-browser-adapter contract: requires procedure args
;; ---------------------------------------------------------------------------

(test-case "make-browser-adapter rejects non-procedure open-fn"
  (check-exn exn:fail:contract?
             (lambda ()
               (make-browser-adapter
                #:open "not-a-proc"
                #:close noop #:navigate noop
                #:observe noop #:act noop #:screenshot noop))))

(test-case "make-browser-adapter rejects non-procedure close-fn"
  (check-exn exn:fail:contract?
             (lambda ()
               (make-browser-adapter
                #:open noop
                #:close 42
                #:navigate noop
                #:observe noop #:act noop #:screenshot noop))))

;; ---------------------------------------------------------------------------
;; browser-adapter-open/close/navigate contracts: require browser-adapter?
;; ---------------------------------------------------------------------------

(test-case "browser-adapter-open rejects non-adapter"
  (check-exn exn:fail:contract?
             (lambda ()
               (browser-adapter-open "not-adapter" "s1" "url"))))

(test-case "browser-adapter-close rejects non-adapter"
  (check-exn exn:fail:contract?
             (lambda ()
               (browser-adapter-close 'foo "s1"))))

(test-case "browser-adapter-navigate rejects non-string url"
  (check-exn exn:fail:contract?
             (lambda ()
               (browser-adapter-navigate (make-valid-adapter) "s1" 123))))

;; ---------------------------------------------------------------------------
;; browser-adapter-act requires browser-action?
;; ---------------------------------------------------------------------------

(test-case "browser-adapter-act rejects non-browser-action"
  (check-exn exn:fail:contract?
             (lambda ()
               (browser-adapter-act (make-valid-adapter) "s1" "not-an-action"))))

(test-case "browser-adapter-act accepts browser-action-click"
  (check-equal?
   (browser-adapter-act (make-valid-adapter) "s1" (browser-action-click "#btn" "left"))
   'ok))

(test-case "browser-adapter-act accepts browser-action-navigate"
  (check-equal?
   (browser-adapter-act (make-valid-adapter) "s1" (browser-action-navigate "https://x.com" "load"))
   'ok))

(test-case "browser-adapter-act accepts browser-action-type"
  (check-equal?
   (browser-adapter-act (make-valid-adapter) "s1" (browser-action-type "#inp" "hi" #f))
   'ok))

;; ---------------------------------------------------------------------------
;; Keyword arg contracts
;; ---------------------------------------------------------------------------

(test-case "browser-adapter-screenshot rejects non-boolean full-page?"
  (check-exn exn:fail:contract?
             (lambda ()
               (browser-adapter-screenshot (make-valid-adapter) "s1"
                                            #:full-page? "yes"))))
