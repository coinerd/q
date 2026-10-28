#lang racket/base
;; @covers extensions/gsd/verify-command-format.rkt
;; @covers extensions/gsd/wave-docs.rkt
;; @covers extensions/gsd/prompts.rkt
;; @covers docs/gsd-guide.md
;; @speed fast
;; @suite extensions
;; @boundary integration
;;
;; W5 documentation contract: the operator documentation and the code share ONE
;; contract, and the contract is TESTED:
;;
;;   1. Every Verify example in the machine-readable block of
;;      `docs/gsd-guide.md` is parsed by the real grammar
;;      (`verify-declaration-extraction`) and must produce exactly the
;;      documented outcome: ACCEPT, a named rejection, or a named refusal.
;;   2. The wave-doc authoring template emitted by `planning-prompt` is built
;;      from the SAME required-header/section constants that `lint-wave-doc`
;;      enforces (no hand-maintained second copy).
;;   3. The published lifecycle section and recovery table exist exactly once
;;      (one lifecycle diagram, one recovery table) and the other documents
;;      link to them instead of restating them.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         (only-in "../extensions/gsd/verify-command-format.rkt"
                  verify-declaration-extraction
                  verify-rejection-reasons
                  verify-refusal-reasons)
         (only-in "../extensions/gsd/wave-docs.rkt"
                  WAVE-DOC-HEADING-PREFIX
                  WAVE-DOC-STATUS-PREFIX
                  WAVE-DOC-FILE-LINE-PREFIX
                  WAVE-DOC-FILES-SECTION
                  WAVE-DOC-VERIFY-SECTION
                  WAVE-DOC-DONE-SECTION
                  WAVE-DOC-INITIAL-STATUS
                  WAVE-DOC-REQUIRED-SECTIONS
                  wave-doc-required-section-names
                  lint-wave-doc
                  parse-wave-doc-from-string
                  wave-doc-violation-section)
         "../extensions/gsd/prompts.rkt")

(define q-root (simplify-path (build-path (syntax-source #'here) ".." "..")))
(define guide-path (build-path q-root "docs" "gsd-guide.md"))
(define architecture-path (build-path q-root "docs" "gsd-architecture.md"))
(define review-path (build-path q-root "docs" "automatic-review.md"))
(define adr-path (build-path q-root "docs" "adr" "0011-gsd-state-machine-rewrite.md"))

(define (file-text path)
  (and (file-exists? path) (file->string path)))

(define guide-text (or (file-text guide-path) ""))

;; ============================================================
;; Line helpers (anchors and section slicing without inline regex flags)
;; ============================================================

(define (lines-of text) (string-split text "\n"))

(define (count-exact-lines text target)
  (count (lambda (l) (string=? l target)) (lines-of text)))

(define (line-offset text target)
  (let loop ([lines (lines-of text)] [offset 0])
    (cond
      [(null? lines) #f]
      [(string=? (car lines) target) offset]
      [else (loop (cdr lines) (+ offset (string-length (car lines)) 1))])))

;; Text of one `## ` section: heading line through the line before the next
;; `## ` heading. #f when the heading is absent.
(define (h2-section text heading)
  (define start (line-offset text heading))
  (and start
       (let* ([after-heading (+ start (string-length heading))]
              [lines (lines-of (substring text after-heading))])
         (let loop ([ls lines] [consumed 0])
           (cond
             [(null? ls) (substring text start)]
             [(string-prefix? (car ls) "## ")
              (substring text start (+ after-heading consumed))]
             [else (loop (cdr ls) (+ consumed (string-length (car ls)) 1))])))))

;; Drop one `## ` section (heading + body) from TEXT.
(define (drop-h2-section text heading)
  (define section (h2-section text heading))
  (cond
    [(not section) text]
    [(string=? section text) ""]
    [else (regexp-replace (regexp-quote section) text "")]))

;; ============================================================
;; 1. Documented Verify grammar examples execute against the parser
;; ============================================================

;; The documented block:
;;   <!-- verify-grammar-examples:start -->
;;   ```text
;;   <ACCEPT|REJECT reason> | <Verify declaration text>
;;   ```
;;   <!-- verify-grammar-examples:end -->
;; `\n` (two characters) in the declaration stands for a line break.
(define (first-pipe-index s)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) #\|) i]
      [else (loop (add1 i))])))

(define (documented-verify-examples text)
  (define m (regexp-match #rx"<!-- verify-grammar-examples:start -->\n(.*?)<!-- verify-grammar-examples:end -->"
                          text))
  (unless m
    (error 'documented-verify-examples
           "verify grammar examples block missing from docs/gsd-guide.md"))
  (for/list ([line (in-list (string-split (cadr m) "\n"))]
             #:when (non-empty-string? (string-trim line))
             #:unless (string-prefix? (string-trim line) "```"))
    (define trimmed (string-trim line))
    (define bar (first-pipe-index trimmed))
    (unless bar
      (error 'documented-verify-examples (format "example line has no `|` separator: ~s" line)))
    (define head (string-trim (substring trimmed 0 bar)))
    (define body (string-trim (substring trimmed (add1 bar))))
    (define declaration (string-replace body "\\n" "\n"))
    (define verdict-tokens (string-split head))
    (cond
      [(equal? verdict-tokens '("ACCEPT")) (list 'accept declaration trimmed)]
      [(and (= (length verdict-tokens) 2) (equal? (car verdict-tokens) "REJECT"))
       (list 'reject (string->symbol (cadr verdict-tokens)) declaration trimmed)]
      [else (error 'documented-verify-examples (format "bad verdict ~s" head))])))

(define examples (documented-verify-examples guide-text))

(define (reason-codes extraction)
  (append (for/list ([rj (in-list (hash-ref extraction 'rejections '()))])
            (hash-ref rj 'reason))
          (let ([refusal (hash-ref extraction 'refusal #f)])
            (if refusal (list refusal) '()))))

(test-case "W5: the documented Verify grammar section exists and has both outcomes"
  (check-true (string-contains? guide-text "### Verify grammar (`## Verify`)"))
  (check-true (>= (length examples) 10))
  (check-not-false (filter (lambda (e) (eq? (car e) 'accept)) examples))
  (check-not-false (filter (lambda (e) (eq? (car e) 'reject)) examples)))

(test-case "W5: every documented Verify ACCEPT example is accepted by the parser"
  (for ([e (in-list examples)] #:when (eq? (car e) 'accept))
    (define declaration (cadr e))
    (define extraction (verify-declaration-extraction declaration))
    (check-equal? (reason-codes extraction) '()
                  (format "documented ACCEPT refused: ~s" (caddr e)))
    (check-false (hash-ref extraction 'refusal)
                 (format "documented ACCEPT has a refusal: ~s" (caddr e)))
    (check-not-false (pair? (hash-ref extraction 'commands))
                     (format "documented ACCEPT shaped no executable command: ~s" (caddr e)))))

(test-case "W5: every documented Verify REJECT example produces the documented reason"
  (for ([e (in-list examples)] #:when (eq? (car e) 'reject))
    (define documented-reason (cadr e))
    (define declaration (caddr e))
    (define extraction (verify-declaration-extraction declaration))
    (define codes (reason-codes extraction))
    (check-not-false (member documented-reason codes)
                     (format "documented REJECT ~a not produced for ~s (got ~a)"
                             documented-reason (cadddr e) codes))
    ;; The documented reason must belong to the closed vocabulary, and a
    ;; rejection must never leave an executable command behind.
    (check-not-false (or (memq documented-reason verify-rejection-reasons)
                         (memq documented-reason verify-refusal-reasons))
                     (format "~a is outside the closed reason vocabulary" documented-reason))
    (when (memq documented-reason verify-rejection-reasons)
      (check-equal? (hash-ref extraction 'commands) '()
                    "a rejected declaration must not keep executable stages"))))

(test-case "W5: an undocumented reason would fail the documentation contract"
  (define extraction (verify-declaration-extraction "- `ls -l` lists"))
  (check-not-false (member 'unsupported-command (reason-codes extraction)))
  (check-false (member 'not-a-real-reason (reason-codes extraction))))

;; ============================================================
;; 2. One required wave-doc contract: template generated from lint constants
;; ============================================================

(define (emitted-wave-doc-template)
  (define p (planning-prompt "test"))
  (define m (regexp-match-positions #rx"### Wave Doc Format[^\n]*\n```\n" p))
  (unless m (error 'emitted-wave-doc-template "wave-doc template fence missing"))
  (define start (cdr (car m)))
  (define close (regexp-match-positions #rx"\n```" (substring p start)))
  (unless close (error 'emitted-wave-doc-template "closing fence missing"))
  (substring p start (+ start (caar close))))

(test-case "W5: the prompt template is generated from the wave-docs contract"
  (define tpl (emitted-wave-doc-template))
  (check-true (string-prefix? tpl (format "~a0\n~a~a\n"
                                          WAVE-DOC-HEADING-PREFIX
                                          WAVE-DOC-STATUS-PREFIX
                                          WAVE-DOC-INITIAL-STATUS)))
  ;; Every required section of the shared table appears exactly once.
  (for ([name (in-list (wave-doc-required-section-names))])
    (check-equal? (count-exact-lines tpl (format "## ~a" name))
                  1
                  (format "section `## ~a` must appear exactly once in the template" name)))
  ;; The table is exactly Files/file-line, Verify/nonempty, Done/nonempty.
  (check-equal? (wave-doc-required-section-names)
                (list WAVE-DOC-FILES-SECTION WAVE-DOC-VERIFY-SECTION WAVE-DOC-DONE-SECTION))
  (check-equal? (map (lambda (entry) (list-ref entry 1)) WAVE-DOC-REQUIRED-SECTIONS)
                '(file-line nonempty nonempty))
  ;; The Files bullet uses the shared prefix, so the lint's file-line pattern matches.
  (check-true (string-contains? tpl (string-append WAVE-DOC-FILE-LINE-PREFIX "path/to/file"))))

(test-case "W5: the emitted template lints clean; each missing section is reported by name"
  (define tpl (emitted-wave-doc-template))
  (define doc (parse-wave-doc-from-string tpl 0 "short-title"))
  (check-equal? (map wave-doc-violation-section (lint-wave-doc doc)) '())
  (for ([entry (in-list WAVE-DOC-REQUIRED-SECTIONS)])
    (define name (first entry))
    (define expected (list-ref entry 2))
    (define broken (parse-wave-doc-from-string
                    (drop-h2-section tpl (format "## ~a" name))
                    0
                    "short-title"))
    (check-not-false (member expected (map wave-doc-violation-section (lint-wave-doc broken)))
                     (format "removing `## ~a` must report violation ~a" name expected))))

;; ============================================================
;; 3. ONE lifecycle diagram and ONE recovery table, linked everywhere
;; ============================================================

(define lifecycle-section (h2-section guide-text "## Campaign lifecycle"))

(test-case "W5: the guide publishes exactly one campaign lifecycle and recovery table"
  (check-equal? (count-exact-lines guide-text "## Campaign lifecycle") 1)
  (check-equal? (count-exact-lines guide-text "### Recovery table") 1)
  (check-true (string? lifecycle-section))
  (check-true (string-contains? (or lifecycle-section "") "decide-campaign-action"))
  ;; The six decision kinds are all documented in the lifecycle section.
  (for ([kind (in-list '("complete" "launch" "resume" "finalize" "block" "cancel"))])
    (check-true (string-contains? (or lifecycle-section "") (format "`~a`" kind))
                (format "decision kind `~a` documented in the lifecycle section" kind)))
  ;; No other doc restates the section: the other documents link to it.
  (for ([path (in-list (list architecture-path review-path adr-path))])
    (define text (file-text path))
    (check-true (and text (string-contains? text "gsd-guide.md#"))
                (format "~a must link to the one lifecycle section" path))
    (check-equal? (if text (count-exact-lines text "## Campaign lifecycle") 0)
                  0
                  (format "~a must not duplicate the lifecycle section" path))))

(test-case "W5: the recovery table forbids destructive recovery and force release"
  (define section (or lifecycle-section ""))
  (check-true (string-contains? section "### Recovery table"))
  (for ([phrase (in-list '("Deleting campaign state is not a repair"
                           "no force-release command"
                           "never rewritten"
                           "never marked DONE"))])
    (check-true (string-contains? section phrase)
                (format "recovery table must state: ~a" phrase))))

;; ============================================================
;; 4. Module document keeps the responsibility/dependency rules
;; ============================================================

(test-case "W5: architecture doc states the single-owner and removal rules"
  (define arch (or (file-text architecture-path) ""))
  (for ([phrase (in-list '("## Responsibility inventory and dependency rules"
                           "Only `campaign-state.rkt` / `campaign-repository.rkt` may write durable"
                           "Compatibility wrappers delegate only"
                           "TODO(#v1.00.40)"))])
    (check-true (string-contains? arch phrase)
                (format "architecture doc must state: ~a" phrase))))
