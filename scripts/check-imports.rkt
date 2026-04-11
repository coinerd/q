#!/usr/bin/env racket
#lang racket/base

;; check-imports.rkt — Detect duplicate identifier provides across required modules.
;;
;; For each .rkt file (excluding benchmarks/), parses require forms for relative
;; paths, extracts provided identifiers from each required module, and reports
;; any identifier provided by more than one required module.
;;
;; Exit 0 if no conflicts found, 1 otherwise.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string)

;; mutable-set is in full racket; use a hash instead
(define (mutable-set) (make-hash))
(define (set-add! h v) (hash-set! h v #t))
(define (set->list h) (hash-keys h))

;; --- Skip paths ---

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (string-contains? s "/benchmarks/")))

(define (collect-rkt-files base-dir)
  (sort
   (for/list ([f (in-directory base-dir)]
              #:when (and (not (skip-path? f))
                          (let ([ext (filename-extension f)])
                            (and ext (equal? (bytes->string/utf-8 ext) "rkt")))))
     f)
   path<?))

;; --- Built-in module detection ---

(define (builtin-module? mod-path)
  ;; Anything that doesn't look like a relative path (starts with " and contains .rkt)
  ;; is assumed to be a built-in / collection path — skip it.
  (not (and (string? mod-path)
            (string-prefix? mod-path "\"")
            (string-suffix? mod-path ".rkt\""))))

;; --- Extract relative require paths from file content ---

(define (extract-relative-requires content)
  ;; Find string literals inside require forms that look like relative .rkt paths.
  ;; Strategy: find (require ...) s-expressions, then extract string literals.
  ;; Simplified: use regex to find quoted strings containing .rkt inside require forms.
  (define requires-regex #rx"\\(require\\s+[^)]*\\)")
  (define string-regex #rx"\"[^\"]*\\.rkt\"")
  (for*/list ([match (in-list (regexp-match* requires-regex content))]
              [path-str (in-list (regexp-match* string-regex match))])
    ;; Strip surrounding quotes
    (substring path-str 1 (sub1 (string-length path-str)))))

;; --- Extract provide identifiers from file content ---

(define (extract-provides content)
  ;; Find (provide ...) forms and extract identifiers.
  ;; Handles: (provide id1 id2 ...) and (provide (struct-out name)) etc.
  ;; We extract plain symbols only.
  (define provides-regex #rx"\\(provide\\s+[\\s\\S]*?\\)")
  (define id-regex #rx"[a-zA-Z_][a-zA-Z0-9_-]*(!\\?)?")
  ;; We need to handle multi-line provide forms carefully.
  ;; Use a port-based approach to find balanced parens.
  (define ids (mutable-set))
  (define in (open-input-string content))
  (let loop ()
    (define c (read-char in))
    (cond
      [(eof-object? c) (void)]
      [(char=? c #\()
       (define pos (file-position in))
       ;; Peek to see if this is a provide form
       (define next-chars
         (with-handlers ([exn:fail? (λ (_) "")])
           (read-string 7 in)))
       (file-position in pos)
       (when (and (string? next-chars)
                  (>= (string-length next-chars) 7)
                  (string=? (substring next-chars 0 7) "provide")
                  ;; Make sure it's preceded by ( — check previous char context
                  )
         ;; Try to read the full (provide ...) form
         (file-position in (sub1 pos))
         (with-handlers ([exn:fail:read? (λ (_) (void))])
           (define form (read in))
           (when (and (list? form)
                      (>= (length form) 2)
                      (eq? (car form) 'provide))
             (for ([item (in-list (cdr form))])
               (match item
                 [(? symbol? s)
                  (set-add! ids s)]
                 [`(struct-out ,(? symbol? s))
                  (set-add! ids s)]
                 [`(contract-out ,(? symbol? s) ,_ ...)
                  (set-add! ids s)]
                 [`(all-defined-out) (void)]
                 [`(all-from-out ,_ ...) (void)]
                 [`(rename ,_ ,(? symbol? s) ,_ ...)
                  (set-add! ids s)]
                 [`(protect ,_ ...) (void)]
                 [_ (void)])))))
       (loop)]
      [else (loop)]))
  (set->list ids))

;; --- Resolve relative path from source file ---

(define (resolve-relative-path source-file rel-path)
  (define src-dir (path-only source-file))
  (if src-dir
      (simplify-path (build-path src-dir rel-path))
      (build-path rel-path)))

;; --- Main logic ---

(define (check-file source-file)
  (define content (file->string source-file))
  (define rel-requires (extract-relative-requires content))
  (if (null? rel-requires)
      '()
      (let ()
        ;; For each relative require, resolve and extract provides
        (define mod-provides
          (for/fold ([acc '()])
                    ([rel-path (in-list rel-requires)])
            (define resolved (resolve-relative-path source-file rel-path))
            (define content-result
              (with-handlers ([exn:fail:filesystem? (λ (_) #f)])
                (if (file-exists? resolved)
                    (file->string resolved)
                    #f)))
            (if content-result
                (let ([ids (extract-provides content-result)])
                  (if (null? ids)
                      acc
                      (cons (cons (path->string resolved) ids) acc)))
                acc)))
        ;; Now find identifiers provided by more than one module
        (define id->modules (make-hash))
        (for ([mp (in-list mod-provides)])
          (define mod-path (car mp))
          (define ids (cdr mp))
          (for ([id (in-list ids)])
            (hash-update! id->modules id
                          (λ (mods) (cons mod-path mods))
                          '())))
        (define conflicts
          (for/list ([(id mods) (in-hash id->modules)]
                     #:when (> (length mods) 1))
            (list id (reverse mods))))
        conflicts)))

(define (main)
  ;; Determine q/ root: parent of scripts/
  (define script-dir (path-only (resolved-module-path-name
                                  (variable-reference->resolved-module-path
                                   (#%variable-reference)))))
  (define q-root (simplify-path (build-path script-dir "..")))
  (printf "Scanning for import conflicts in: ~a~n" q-root)

  (define files (collect-rkt-files q-root))
  (define all-conflicts '())

  (for ([f (in-list files)])
    (define conflicts (check-file f))
    (when (not (null? conflicts))
      (for ([c (in-list conflicts)])
        (define id (car c))
        (define mods (cadr c))
        (define rel-file (find-relative-path q-root f))
        (define rel-mods (map (λ (p) (find-relative-path q-root (string->path p))) mods))
        (printf "[CONFLICT] ~a: identifier '~a' provided by both ~a and ~a~n"
                rel-file id (car rel-mods) (cadr rel-mods))
        (set! all-conflicts (cons c all-conflicts)))))

  (if (null? all-conflicts)
      (begin
        (printf "~nNo import conflicts found.~n")
        (exit 0))
      (begin
        (printf "~n~a conflict(s) found.~n" (length all-conflicts))
        (exit 1))))

(main)
