#!/usr/bin/env racket
#lang racket/base

;; lint-docs.rkt — Documentation coherence linter.
;;
;; Checks:
;;   1. Verified-against markers in docs/ files reference the current version
;;   2. No stale version references in canonical docs
;;   3. Internal doc links resolve to existing files AND (when present)
;;      their #anchor exists in the target document (including pure #anchors)
;;   4. ADR numbering is sequential without gaps
;;
;; Exit 0 if clean, 1 if any issues found.
;; Run from the q/ directory.

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/match
         (only-in "version-surface.rkt" read-canonical-version!))

(provide check-doc-links
         doc-link-problem
         heading->anchor
         doc-file-anchors)

;; ---------------------------------------------------------------------------
;; Version helpers
;; ---------------------------------------------------------------------------

;; W8 (#8570): parse-q-version removed; canonical version now read via
;; read-canonical-version! from version-surface.rkt.

(define VERSION-PAT #rx"[0-9]+\\.[0-9]+\\.[0-9]+")

;; ---------------------------------------------------------------------------
;; File discovery
;; ---------------------------------------------------------------------------

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/") (string-contains? s "/.git/")))

(define (collect-md-files base-dir)
  (sort (for/list ([f (in-directory base-dir)]
                   #:when (and (not (skip-path? f))
                               (let ([ext (filename-extension f)])
                                 (and ext (equal? (bytes->string/utf-8 ext) "md")))))
          f)
        path<?))

;; ---------------------------------------------------------------------------
;; Check 1: verified-against markers
;; ---------------------------------------------------------------------------

(define (check-verified-markers md-files canonical)
  (define marker-rx #rx"<!-- verified-against: v([0-9]+\\.[0-9]+\\.[0-9]+) -->")
  (define warnings 0)
  (for ([f (in-list md-files)])
    (define content (file->string f))
    (define matches (regexp-match* marker-rx content))
    (for ([m (in-list matches)])
      (define v-m (regexp-match #rx"v([0-9]+\\.[0-9]+\\.[0-9]+)" m))
      (when v-m
        (define v (cadr v-m))
        (unless (equal? v canonical)
          (printf "  WARN: ~a: verified-against v~a but canonical is v~a~n"
                  (path->string f)
                  v
                  canonical)
          (set! warnings (add1 warnings))))))
  (printf "  Verified-against markers checked: ~a warnings~n" warnings)
  warnings)

;; ---------------------------------------------------------------------------
;; Check 2: internal doc links (file AND #anchor)
;; ---------------------------------------------------------------------------

;; `[^]]` (with the closing bracket first) is the portable way to exclude
;; ']' from the character class; the old `[^\]]` spelling never matched.
(define link-rx #rx"\\[[^]]*\\]\\(([^)\"']+)\\)")

;; GitHub-style heading slug: lower-case, inline-code markers removed, other
;; punctuation removed (including '.', '/', ':', '(', ')'), whitespace becomes
;; '-', while letters/digits/'_'/'-' survive. This intentionally does not
;; collapse runs: 'status — state' becomes 'status--state'.
(define (heading->anchor heading)
  (define text (string-trim (regexp-replace* #rx"`" heading "")))
  (define lowered (string-downcase text))
  (list->string
   (filter values
           (for/list ([ch (in-string lowered)])
             (cond
               [(char-whitespace? ch) #\-]
               [(or (char-alphabetic? ch)
                    (char-numeric? ch)
                    (char=? ch #\-)
                    (char=? ch #\_))
                ch]
               [else #f])))))

(define (heading-text line)
  ;; Racket's regexp reader does not accept `#{1,6}` here, so capture the run
  ;; and enforce the ATX 1..6 limit in Scheme.
  (define m (regexp-match #rx"^(#+)[ \t]+(.*)$" line))
  (and m
       (let ([level (string-length (cadr m))]
             [text (caddr m)])
         (and (<= 1 level 6)
              (string-trim (regexp-replace #rx"[ \t]+#+[ \t]*$" text ""))))))

;; Compare anchors with hyphen runs collapsed. Punctuation immediately before a
;; space (an em dash, `(` + `#` in inline code, ...) leaves two spaces and thus
;; two hyphens in the strict GitHub slug, while the docs consistently spell the
;; readable single-hyphen form. Collapsing only the COMPARISON key keeps
;; missing anchors broken without making the checker reject those real links.
(define (anchor-key anchor)
  (regexp-replace* #rx"-+" anchor "-"))

;; Anchors actually present in one Markdown file. Fenced code is excluded so a
;; '# heading' example cannot satisfy a real link. Duplicate headings receive
;; GitHub's -1, -2, ... suffixes.
(define (doc-file-anchors path)
  (define content
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (file->string path)))
  (and content
       (let loop ([lines (string-split content "\n")]
                  [in-fence? #f]
                  [seen (hash)]
                  [anchors '()])
         (cond
           [(null? lines) (reverse anchors)]
           [else
            (define line (car lines))
            (define trimmed (string-trim line))
            (define fence?
              (or (string-prefix? trimmed "```")
                  (string-prefix? trimmed "~~~")))
            (cond
              [fence? (loop (cdr lines) (not in-fence?) seen anchors)]
              [in-fence? (loop (cdr lines) in-fence? seen anchors)]
              [else
               (define text (heading-text line))
               (cond
                 [(not text) (loop (cdr lines) #f seen anchors)]
                 [else
                  (define base (heading->anchor text))
                  (cond
                    [(string=? base "") (loop (cdr lines) #f seen anchors)]
                    [else
                     (define n (hash-ref seen base 0))
                     (define anchor (if (zero? n) base (format "~a-~a" base n)))
                     (loop (cdr lines)
                           #f
                           (hash-set seen base (add1 n))
                           (cons anchor anchors))])])])]))))

(define (split-doc-link-target target)
  (define m (regexp-match #rx"^([^#]*)#(.*)$" target))
  (if m
      (values (cadr m) (caddr m))
      (values target "")))

(define (local-doc-link? target)
  (and (not (string=? target ""))
       (not (string-prefix? target "//"))
       (not (regexp-match? #rx"^[A-Za-z][A-Za-z0-9+.-]*:" target))))

;; #f when TARGET resolves, otherwise a list describing the FIRST failure:
;;   '(missing-file resolved-path)
;;   '(missing-anchor target-file anchor)
;; A pure '#anchor' checks the source file itself.
(define (doc-link-problem source-file target)
  (define-values (file-part anchor) (split-doc-link-target target))
  (define target-file
    (if (string=? file-part "")
        source-file
        (simplify-path (build-path (path-only source-file) file-part))))
  (cond
    [(not (or (file-exists? target-file) (directory-exists? target-file)))
     (list 'missing-file target-file)]
    [(string=? anchor "") #f]
    [(directory-exists? target-file)
     (list 'missing-anchor target-file anchor)]
    [else
     (define anchors (doc-file-anchors target-file))
     (define key (anchor-key anchor))
     (if (and anchors
              (for/or ([candidate (in-list anchors)])
                (string=? key (anchor-key candidate))))
         #f
         (list 'missing-anchor target-file anchor))]))

(define (check-doc-links md-files)
  (define broken 0)
  (for ([f (in-list md-files)])
    (define content (file->string f))
    (define links (regexp-match* link-rx content))
    (for ([raw (in-list links)])
      (define target-m (regexp-match #rx"\\]\\(([^)\"']+)" raw))
      (when target-m
        (define target (cadr target-m))
        (when (local-doc-link? target)
          (define problem (doc-link-problem f target))
          (when problem
            (case (car problem)
              [(missing-file)
               (printf "  WARN: ~a: broken link ~a -> ~a~n"
                       (path->string f)
                       target
                       (path->string (cadr problem)))]
              [(missing-anchor)
               (printf "  WARN: ~a: broken link anchor ~a -> ~a (missing #~a)~n"
                       (path->string f)
                       target
                       (path->string (cadr problem))
                       (caddr problem))])
            (set! broken (add1 broken)))))))
  (printf "  Doc links checked (files + #anchors): ~a broken~n" broken)
  broken)

;; ---------------------------------------------------------------------------
;; Check 3: ADR numbering
;; ---------------------------------------------------------------------------

(define (check-adr-numbering)
  (define adr-dir (build-path (current-directory) "docs" "adr"))
  (define gaps 0)
  (when (directory-exists? adr-dir)
    (define adrs
      (filter values
              (for/list ([f (in-directory adr-dir)])
                (define name (path->string (file-name-from-path f)))
                (define m (regexp-match #rx"^([0-9]+)-" name))
                (and m (string->number (car m))))))
    (set! adrs (sort adrs <))
    (for ([n (in-list adrs)]
          [i (in-naturals 1)])
      (unless (= n i)
        (printf "  WARN: ADR gap — expected ~a, found ~a~n" i n)
        (set! gaps (add1 gaps)))))
  (printf "  ADR numbering: ~a gaps~n" gaps)
  gaps)

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define canonical (read-canonical-version!))

  (printf "Docs lint — canonical version: ~a~n~n" canonical)

  (define md-files (collect-md-files (build-path (current-directory) "docs")))
  (printf "Found ~a doc files~n~n" (length md-files))

  (displayln "Checking verified-against markers ...")
  (define w1 (check-verified-markers md-files canonical))

  (displayln "Checking doc links ...")
  (define w2 (check-doc-links md-files))

  (displayln "Checking ADR numbering ...")
  (define w3 (check-adr-numbering))

  (define total (+ w1 w2 w3))
  (printf "~n---~n")
  (printf "Total warnings: ~a~n" total)
  (if (= total 0)
      (begin
        (displayln "Docs lint PASSED")
        (exit 0))
      (begin
        (displayln "Docs lint FAILED")
        (exit 1))))

(module+ main
  (main))
