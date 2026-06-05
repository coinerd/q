#lang racket

;; q/tests/test-ui-widget-descriptor.rkt — Widget descriptor schema tests
;;
;; W6.1 (v0.94.6): Verify widget descriptor construction, validation,
;; sorting, filtering, and trust levels.

(require rackunit
         rackunit/text-ui
         "../ui-core/widget-descriptor.rkt")

(define-test-suite
 test-ui-widget-descriptor

 ;; ── Construction ───

 (test-case "make-widget-descriptor creates valid descriptor"
   (define w (make-widget-descriptor 'my-widget 'my-ext
                                      #:zone 'sidebar
                                      #:kind 'text
                                      #:content "hello"))
   (check-eq? (widget-descriptor-id w) 'my-widget)
   (check-eq? (widget-descriptor-extension-id w) 'my-ext)
   (check-eq? (widget-descriptor-zone w) 'sidebar)
   (check-eq? (widget-descriptor-kind w) 'text)
   (check-equal? (widget-descriptor-content w) "hello"))

 (test-case "default values"
   (define w (make-widget-descriptor 'w 'ext))
   (check-false (widget-descriptor-zone w))
   (check-eq? (widget-descriptor-kind w) 'text)
   (check-equal? (widget-descriptor-content w) "")
   (check-equal? (widget-descriptor-priority w) 100)
   (check-equal? (widget-descriptor-capabilities w) '())
   (check-eq? (widget-descriptor-trust-level w) 'extension))

 (test-case "lifecycle token is unique per descriptor"
   (define w1 (make-widget-descriptor 'w 'ext))
   (define w2 (make-widget-descriptor 'w 'ext))
   (check-not-eq? (widget-descriptor-lifecycle-token w1)
                   (widget-descriptor-lifecycle-token w2)))

 ;; ── Trust levels ───

 (test-case "valid trust levels"
   (check-true (trust-level? 'core))
   (check-true (trust-level? 'extension))
   (check-true (trust-level? 'untrusted))
   (check-false (trust-level? 'invalid))
   (check-false (trust-level? 42)))

 (test-case "core trust level descriptor"
   (define w (make-widget-descriptor 'w 'core #:trust-level 'core))
   (check-eq? (widget-descriptor-trust-level w) 'core))

 ;; ── Validation ───

 (test-case "widget-descriptor-valid? accepts valid descriptor"
   (define w (make-widget-descriptor 'w 'ext))
   (check-true (widget-descriptor-valid? w)))

 (test-case "widget-descriptor-valid? rejects non-descriptor"
   (check-false (widget-descriptor-valid? "not a descriptor"))
   (check-false (widget-descriptor-valid? 42)))

 ;; ── Sorting ───

 (test-case "widget-descriptors->sorted sorts by priority"
   (define descs
     (list (make-widget-descriptor 'c 'ext #:priority 300)
           (make-widget-descriptor 'a 'ext #:priority 100)
           (make-widget-descriptor 'b 'ext #:priority 200)))
   (define sorted (widget-descriptors->sorted descs))
   (check-equal? (map widget-descriptor-id sorted) '(a b c)))

 (test-case "empty list sorting"
   (check-equal? (widget-descriptors->sorted '()) '()))

 ;; ── Zone filtering ───

 (test-case "filter-widgets-by-zone"
   (define descs
     (list (make-widget-descriptor 'a 'ext #:zone 'sidebar)
           (make-widget-descriptor 'b 'ext #:zone 'toolbar)
           (make-widget-descriptor 'c 'ext #:zone 'sidebar)))
   (define sidebar (filter-widgets-by-zone descs 'sidebar))
   (check-equal? (map widget-descriptor-id sidebar) '(a c)))

 (test-case "filter-widgets-by-zone returns empty for no match"
   (define descs (list (make-widget-descriptor 'a 'ext #:zone 'sidebar)))
   (check-equal? (filter-widgets-by-zone descs 'toolbar) '()))

 ;; ── Trust filtering ───

 (test-case "filter-widgets-by-trust"
   (define descs
     (list (make-widget-descriptor 'a 'core #:trust-level 'core)
           (make-widget-descriptor 'b 'ext #:trust-level 'extension)
           (make-widget-descriptor 'c 'ext #:trust-level 'untrusted)))
   (check-equal? (map widget-descriptor-id (filter-widgets-by-trust descs 'core))
                 '(a))
   (check-equal? (map widget-descriptor-id (filter-widgets-by-trust descs 'untrusted))
                 '(c)))

 ;; ── Capabilities ───

 (test-case "capabilities can list requested features"
   (define w (make-widget-descriptor 'w 'ext
                                      #:capabilities '(scroll clickable)))
   (check-equal? (widget-descriptor-capabilities w) '(scroll clickable)))

 ;; ── Safe mode filtering scenario ───

 (test-case "safe mode: untrusted widgets excluded"
   (define descs
     (list (make-widget-descriptor 'core-status 'core #:trust-level 'core)
           (make-widget-descriptor 'ext-tool 'ext #:trust-level 'extension)
           (make-widget-descriptor 'sketchy 'unknown #:trust-level 'untrusted)))
   (define safe (filter (lambda (d) (not (eq? (widget-descriptor-trust-level d) 'untrusted)))
                        descs))
   (check-equal? (length safe) 2)
   (check-not-false (member 'core-status (map widget-descriptor-id safe)))))

(run-tests test-ui-widget-descriptor)
