#lang racket/base

;; scripts/lint-doc-freshness.rkt — Documentation freshness/drift checker (v0.54.6)
;;
;; Usage:
;;   racket scripts/lint-doc-freshness.rkt          # check all docs
;;   racket scripts/lint-doc-freshness.rkt --fix     # auto-fix version markers

(require racket/string
         racket/port
         racket/file)

;; Extract version from util/version.rkt source (avoids loading the module)
(define q-version
  (let ([src (file->string "util/version.rkt")])
    (define m (regexp-match #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" src))
    (and m (cadr m))))

(define canonical-docs
  '("docs/install.md"
    "docs/getting-started/index.md"
    "docs/extension-guide.md"
    "docs/security-trust-model.md"
    "docs/self-hosting.md"
    "docs/style-guide.md"
    "docs/trust-model.md"
    "docs/workflow-testing.md"
    "docs/architecture/overview.md"
    "docs/event-taxonomy.md"))

(define (get-file-version path)
  (define text (file->string path))
  (define m1 (regexp-match #rx"verified-against:[ ]*([0-9]+\\.[0-9]+\\.[0-9]+)" text))
  (cond [m1 (cadr m1)]
        [else
         (define m2 (regexp-match #rx"## Version[ \t]*\n+v?([0-9]+\\.[0-9]+\\.[0-9]+)" text))
         (cond [m2 (cadr m2)]
               [else
                (define m3 (regexp-match #rx"[Qq] ([0-9]+\\.[0-9]+\\.[0-9]+)" text))
                (and m3 (cadr m3))])]))

(define (check-doc path)
  (cond
    [(not (file-exists? path))
     (printf "  ⚠ MISSING: ~a\n" path)
     'missing]
    [else
     (define ver (get-file-version path))
     (cond
       [(not ver)
        (printf "  ⚠ NO VERSION MARKER: ~a\n" path)
        'no-marker]
       [(equal? ver q-version)
        (printf "  ✓ ~a (~a)\n" path ver)
        'ok]
       [else
        (printf "  ✗ STALE: ~a has ~a (expected ~a)\n" path ver q-version)
        'stale])]))

(define (fix-doc path)
  (when (file-exists? path)
    (define text (file->string path))
    (define new-text
      (let* ([t (regexp-replace* #rx"verified-against:[ ]*[0-9]+\\.[0-9]+\\.[0-9]+"
                                 text
                                 (format "verified-against: ~a" q-version))]
             [t (regexp-replace* #rx"(## Version[ \t]*\n+v?)[0-9]+\\.[0-9]+\\.[0-9]+"
                                 t
                                 (format "\\1~a" q-version))]
             [t (regexp-replace* #rx"([Qq] )[0-9]+\\.[0-9]+\\.[0-9]+"
                                 t
                                 (format "\\1~a" q-version))])
        t))
    (unless (equal? text new-text)
      (display-to-file new-text path #:exists 'truncate/replace)
      (printf "  Fixed: ~a\n" path))))

(define (main args)
  (define fix? (member "--fix" args))
  (printf ";; lint-doc-freshness: version=~a, docs=~a\n" q-version (length canonical-docs))
  (define results
    (for/list ([doc (in-list canonical-docs)])
      (check-doc doc)))
  (define errors (length (filter (lambda (r) (eq? r 'stale)) results)))
  (define missing (length (filter (lambda (r) (eq? r 'no-marker)) results)))
  (when (and fix? (> (+ errors missing) 0))
    (printf "\nFixing stale/missing markers...\n")
    (for ([doc (in-list canonical-docs)])
      (fix-doc doc)))
  (cond
    [(= (+ errors missing) 0)
     (printf "\nDoc freshness check PASSED\n")
     (exit 0)]
    [else
     (printf "\nDoc freshness check FAILED: ~a stale, ~a missing markers\n" errors missing)
     (exit 1)]))

(main (vector->list (current-command-line-arguments)))
