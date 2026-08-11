#lang racket/base

;; Test-only architecture checker for v0.99.91 Path-B provider ownership.
;; It deliberately does not parse or normalize provider wire events.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string)

(provide (struct-out source-unit)
         (struct-out provider-protocol)
         (struct-out neutral-helper)
         (struct-out provider-locality-policy)
         (struct-out locality-violation)
         load-provider-locality-policy
         production-llm-source-units
         check-provider-locality-policy
         check-provider-change-locality
         locality-violation->string)

(struct source-unit (path source) #:transparent)
(struct provider-protocol (name owners markers) #:transparent)
(struct neutral-helper
        (module primitives evidence
          )
  #:transparent)
(struct provider-locality-policy (version protocols generic-streaming-modules neutral-helpers)
  #:transparent)
(struct locality-violation (path provider marker reason allowed-owners) #:transparent)

(define (section datum name)
  (define found (assoc name datum))
  (unless found
    (error 'load-provider-locality-policy "missing policy section: ~a" name))
  (cdr found))

(define (entry-field entry name)
  (define found (assoc name (cdr entry)))
  (unless found
    (error 'load-provider-locality-policy "missing ~a in ~a" name (car entry)))
  (cdr found))

(define (load-provider-locality-policy path)
  (define datum (call-with-input-file path read))
  (define protocols
    (for/list ([entry (in-list (section datum 'provider-protocols))])
      (provider-protocol (car entry) (entry-field entry 'owners) (entry-field entry 'markers))))
  (define neutral-helpers
    (for/list ([entry (in-list (section datum 'neutral-helpers))])
      (neutral-helper (car entry) (entry-field entry 'primitives) (entry-field entry 'evidence))))
  (provider-locality-policy (section datum 'version)
                            protocols
                            (section datum 'generic-streaming-modules)
                            neutral-helpers))

(define (racket-source-atoms source)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define lines (string-split source "\n" #:trim? #f))
    (define readable
      (string-join (if (and (pair? lines) (string-prefix? (car lines) "#lang"))
                       (cdr lines)
                       lines)
                   "\n"))
    (define forms (port->list read (open-input-string readable)))
    (define atoms '())
    (define (walk value)
      (cond
        [(pair? value)
         (walk (car value))
         (walk (cdr value))]
        [(symbol? value) (set! atoms (cons (symbol->string value) atoms))]
        [(string? value) (set! atoms (cons value atoms))]
        [else (void)]))
    (for-each walk forms)
    (remove-duplicates atoms string=?)))

(define (production-llm-source-units repo-root)
  (define resolved-root (simplify-path repo-root))
  (define llm-root (build-path resolved-root "llm"))
  (sort (for/list ([path (in-directory llm-root)]
                   #:when (and (file-exists? path) (regexp-match? #rx"[.]rkt$" (path->string path))))
          (source-unit (path->string (find-relative-path resolved-root path)) (file->string path)))
        string<?
        #:key source-unit-path))

(define (check-provider-change-locality policy units)
  (sort (append*
         (for/list ([unit (in-list units)])
           (define path (source-unit-path unit))
           (define atoms (racket-source-atoms (source-unit-source unit)))
           (append* (for/list ([protocol (in-list (provider-locality-policy-protocols policy))])
                      (for/list ([marker (in-list (provider-protocol-markers protocol))]
                                 #:when (and (member marker atoms)
                                             (not (member path (provider-protocol-owners protocol)))))
                        (locality-violation
                         path
                         (provider-protocol-name protocol)
                         marker
                         (if (member path (provider-locality-policy-generic-streaming-modules policy))
                             'generic-stream-protocol
                             'wrong-provider-owner)
                         (provider-protocol-owners protocol)))))))
        string<?
        #:key (lambda (violation)
                (format "~a|~a|~a"
                        (locality-violation-path violation)
                        (locality-violation-provider violation)
                        (locality-violation-marker violation)))))

(define (check-provider-locality-policy policy repo-root)
  (define units (production-llm-source-units repo-root))
  (define units-by-path
    (for/hash ([unit (in-list units)])
      (values (source-unit-path unit) unit)))
  (define protocols (provider-locality-policy-protocols policy))
  (define all-markers (append-map provider-protocol-markers protocols))
  (append
   (if (= (provider-locality-policy-version policy) 1)
       '()
       (list (list 'unsupported-version (provider-locality-policy-version policy))))
   (for/list ([marker (in-list (remove-duplicates all-markers))]
              #:when (> (count (lambda (candidate) (string=? candidate marker)) all-markers) 1))
     (list 'duplicate-marker marker))
   (append* (for/list ([protocol (in-list protocols)])
              (append (for/list ([owner (in-list (provider-protocol-owners protocol))]
                                 #:unless (hash-has-key? units-by-path owner))
                        (list 'missing-owner (provider-protocol-name protocol) owner))
                      (for/list ([marker (in-list (provider-protocol-markers protocol))]
                                 #:unless
                                 (for/or ([owner (in-list (provider-protocol-owners protocol))])
                                   (define unit (hash-ref units-by-path owner #f))
                                   (and unit
                                        (member marker
                                                (racket-source-atoms (source-unit-source unit))))))
                        (list 'unobserved-marker (provider-protocol-name protocol) marker)))))
   (for/list ([path (in-list (provider-locality-policy-generic-streaming-modules policy))]
              #:unless (hash-has-key? units-by-path path))
     (list 'missing-generic-streaming-module path))
   (append*
    (for/list ([helper (in-list (provider-locality-policy-neutral-helpers policy))])
      (define unit (hash-ref units-by-path (neutral-helper-module helper) #f))
      (cond
        [(not unit) (list (list 'missing-neutral-helper-module (neutral-helper-module helper)))]
        [else
         (define atoms (racket-source-atoms (source-unit-source unit)))
         (for/list ([primitive (in-list (neutral-helper-primitives helper))]
                    #:unless (member (symbol->string primitive) atoms))
           (list 'missing-neutral-primitive (neutral-helper-module helper) primitive))])))))

(define (locality-violation->string violation)
  (format
   "Provider protocol marker ~s for ~a appears in ~a (~a); move it to an allowed ownership path: ~a"
   (locality-violation-marker violation)
   (locality-violation-provider violation)
   (locality-violation-path violation)
   (locality-violation-reason violation)
   (string-join (locality-violation-allowed-owners violation) ", ")))
