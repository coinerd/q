#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation offline
;; @boundary scripts/ci

;; verify-artifact-provenance.rkt — v1.00.31 W5 (#9728, F7).
;;
;; Declared wave artifacts under artifacts/**/v*/ must be reproducible and
;; internally consistent: a record cannot be committed with a stale recorded
;; head or self-contradictory values.
;;
;;   racket scripts/ci/verify-artifact-provenance.rkt --root <repo-root> \
;;       [--current-wave v1.00.31-w5 --wave-tip <sha>]
;;
;; Per artifact version directory:
;;   1. SHA256SUMS integrity — regenerating the checksum file from the
;;      committed bytes reproduces it byte-identically (hard).
;;   2. Provenance fields — values of head/sha/commit/tree-named fields that
;;      are 40-hex strings must resolve to real commits (hard for the current
;;      wave; reported for historical directories, whose records may
;;      deliberately pin blocked-branch heads as reproduction evidence) and,
;;      for the current wave, must be ancestors of the wave tip (hard).
;;   3. Canonical JSON — the current wave's non-raw JSON artifacts must be
;;      byte-identical to their canonical form (sorted keys, 1-space indent);
;;      historical files only need to parse (their formatting is frozen
;;      history — reported, never rewritten).
;;   4. Cross-artifact consistency — inside an artifact that declares a
;;      structured `timing` object with `*-ms` keys, prose "<n> ms" values
;;      must agree exactly with the structured numbers; a disagreement is a
;;      typed refusal naming both sources (the F7 rollback-drill shape:
;;      prose 260/245 ms vs eager-fallback-ms 247/256 ms).
;;
;; All failures are typed `provenance-drift` and exit 2. Historical
;; observations that cannot be strictly verified are printed as
;; `provenance-note` and never silently ignored.

(require json
         racket/set
         racket/bool
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         (file "../run-tests/sha256.rkt"))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(define drifts '())
(define notes '())

(define (provenance-drift! fmt . args)
  (set! drifts (cons (apply format fmt args) drifts)))

(define (provenance-note! fmt . args)
  (set! notes (cons (apply format fmt args) notes)))

(define (finish!)
  (for ([n (in-list (reverse notes))])
    (displayln (string-append "provenance-note: " n)))
  (for ([d (in-list (reverse drifts))])
    (displayln (string-append "provenance-drift: " d)))
  (when (pair? drifts)
    (exit 2))
  (displayln (string-append "artifact-provenance ok (" (number->string (length notes)) " note(s))"))
  (exit 0))

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------

(define hex40-rx #px"^[0-9a-f]{40}$")
(define hex64-rx #px"^[0-9a-f]{64}$")
(define version-dir-rx #px"^v[0-9]+\\.[0-9]+\\.[0-9]+(-w[0-9]+)?$")

(define (run-git root . args)
  ;; Returns (values exit-code stdout-string). Subprocess ports must be file
  ;; streams, so stdout/stderr are captured through temporary files.
  (define out-file (make-temporary-file "vapt-out-~a"))
  (define err-file (make-temporary-file "vapt-err-~a"))
  (define out (open-output-file out-file #:exists 'truncate))
  (define err (open-output-file err-file #:exists 'truncate))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (define-values (p _in _out _err)
                    (parameterize ([current-directory root]
                                   [current-subprocess-custodian-mode 'kill])
                      (apply subprocess out #f err (find-executable-path "git") args)))
                  (subprocess-wait p)
                  (values (subprocess-status p) (string-trim (file->string out-file))))
                (lambda ()
                  (close-output-port out)
                  (close-output-port err)
                  (delete-file out-file)
                  (delete-file err-file))))

;; Per-run memoization: the same recorded SHAs recur across artifact files
;; and checks; without the cache the real-tree run spawns git hundreds of
;; times and takes minutes.
(define commit-cache (make-hash))
(define ancestor-cache (make-hash))
(define tree-cache (make-hash))

(define (commit-exists? root sha)
  (hash-ref! commit-cache
             sha
             (lambda ()
               (define-values (code _out)
                 (run-git root "cat-file" "-e" (string-append sha "^{commit}")))
               (equal? 0 code))))

(define (is-ancestor? root sha tip)
  (hash-ref! ancestor-cache
             sha
             (lambda ()
               (define-values (code _out) (run-git root "merge-base" "--is-ancestor" sha tip))
               (equal? 0 code))))

(define (commit-tree root sha)
  (hash-ref! tree-cache
             sha
             (lambda ()
               (define-values (code out) (run-git root "rev-parse" (string-append sha "^{tree}")))
               (and (equal? 0 code) out))))

(define (git-head root)
  (define-values (_code out) (run-git root "rev-parse" "HEAD"))
  out)

(define (sha256-file-hex path)
  (bytes->hex-string (sha256 (call-with-input-file path port->bytes))))

;; ---------------------------------------------------------------------------
;; Canonical JSON (byte-compatible with python json.dumps(indent=1,
;; sort_keys=True, ensure_ascii=True) plus a trailing newline)
;; ---------------------------------------------------------------------------

(define (json-escape s)
  (string-append "\""
                 (for/fold ([acc ""]) ([c (in-string s)])
                   (string-append acc
                                  (cond
                                    [(char=? c #\") "\\\""]
                                    [(char=? c #\\) "\\\\"]
                                    [(char=? c #\newline) "\\n"]
                                    [(char=? c #\return) "\\r"]
                                    [(char=? c #\tab) "\\t"]
                                    [(char<? c #\space)
                                     (string-append "\\u"
                                                    (string-upcase (~r (char->integer c)
                                                                       #:base 16
                                                                       #:min-width 4
                                                                       #:pad-string "0")))]
                                    [else (string c)])))
                 "\""))

(define (json-canonical v indent)
  (define pad (make-string indent #\space))
  (define inner (make-string (+ indent 1) #\space))
  (cond
    [(hash? v)
     (if (zero? (hash-count v))
         "{}"
         (string-append "{\n"
                        (string-join (for/list ([k (in-list (sort (map ~a (hash-keys v)) string<?))])
                                       (string-append inner
                                                      (json-escape k)
                                                      ": "
                                                      (json-canonical (hash-ref v (string->symbol k))
                                                                      (+ indent 1))))
                                     ",\n")
                        "\n"
                        pad
                        "}"))]
    [(list? v)
     (if (null? v)
         "[]"
         (string-append "[\n"
                        (string-join (for/list ([x (in-list v)])
                                       (string-append inner (json-canonical x (+ indent 1))))
                                     ",\n")
                        "\n"
                        pad
                        "]"))]
    [(string? v) (json-escape v)]
    [(boolean? v) (if v "true" "false")]
    [(null? v) "null"]
    [(real? v)
     (if (integer? v)
         (number->string v)
         (~v v))]
    [else (json-escape (format "~a" v))]))

(define (canonical-json-string v)
  (string-append (json-canonical v 0) "\n"))

;; ---------------------------------------------------------------------------
;; Discovery
;; ---------------------------------------------------------------------------

(struct artifact-dir (path family version current?) #:transparent)

(define (discover-artifact-dirs! root current-wave)
  (define artifacts-root (build-path root "artifacts"))
  (if (not (directory-exists? artifacts-root))
      '()
      (for/list ([family (in-list (sort (map path->string (directory-list artifacts-root)) string<?))]
                 #:when (directory-exists? (build-path artifacts-root family))
                 [v (in-list (sort (map path->string
                                        (directory-list (build-path artifacts-root family)))
                                   string<?))]
                 #:when (regexp-match? version-dir-rx v))
        (artifact-dir (build-path artifacts-root family v)
                      family
                      v
                      (and current-wave (string=? v current-wave))))))

;; ---------------------------------------------------------------------------
;; Checks
;; ---------------------------------------------------------------------------

;; Verify a SHA256SUMS binding: every recorded digest must match the named
;; file (paths are relative to the repository root and may include files
;; outside the artifact directory, e.g. the wave report), and every file
;; under the directory must be recorded. "Regenerating" the checksum file is
;; a per-line digest comparison plus coverage, never a format assumption.
(define (check-sums! root ad)
  (define dir (artifact-dir-path ad))
  (define sums-path (build-path dir "SHA256SUMS"))
  (cond
    [(file-exists? sums-path)
     (define recorded
       (for/list ([line (in-list (string-split (file->string sums-path) "\n"))]
                  #:when (non-empty-string? line))
         (match (regexp-split #px"  +" line)
           [(list hex path) (cons hex path)]
           [_ (cons #f line)])))
     (define (entry-broken? entry)
       (define rel (cdr entry))
       ;; Two historical conventions coexist: repository-root-relative and
       ;; artifact-directory-relative paths. Accept either spelling.
       (define candidates (list (build-path root rel) (build-path dir rel)))
       (not (for/or ([p (in-list candidates)])
              (and (file-exists? p) (equal? (car entry) (sha256-file-hex p))))))
     (define broken
       (for/list ([entry (in-list recorded)]
                  #:when (and (car entry) (entry-broken? entry)))
         (cdr entry)))
     (for ([rel (in-list broken)]
           #:when rel)
       ;; Historical digest drift cannot be repaired without rewriting frozen
       ;; records; it is reported (never silently ignored). The current wave
       ;; is refused outright.
       (if (artifact-dir-current? ad)
           (provenance-drift! "~a/~a: SHA256SUMS entry for ~a does not match the committed bytes"
                              (artifact-dir-family ad)
                              (artifact-dir-version ad)
                              rel)
           (provenance-note!
            "~a/~a: SHA256SUMS entry for ~a no longer matches the committed bytes (historical drift)"
            (artifact-dir-family ad)
            (artifact-dir-version ad)
            rel)))
     (when (member #f (map car recorded))
       (provenance-drift! "~a/~a: SHA256SUMS contains malformed lines"
                          (artifact-dir-family ad)
                          (artifact-dir-version ad)))
     (define recorded-paths (list->set (map cdr recorded)))
     ;; R4: for the current wave the binding itself must be canonical —
     ;; regenerating it reproduces byte-identically (sorted repository-root
     ;; relative paths, "<hex>  <path>" lines, LF-terminated). Historical
     ;; directories keep their recorded spelling.
     (when (artifact-dir-current? ad)
       (define canonical
         (string-join (for/list ([line (in-list (sort (map cdr recorded) string<?))])
                        (string-append (sha256-file-hex (build-path root line)) "  " line))
                      "\n"))
       (unless (equal? (string-append canonical "\n") (file->string sums-path))
         (provenance-drift!
          "~a/~a: SHA256SUMS is not canonical (regeneration diverges from the committed bytes)"
          (artifact-dir-family ad)
          (artifact-dir-version ad))))
     (define dir-files
       (for/list ([p (in-directory dir)]
                  #:when (and (file-exists? p) (not (equal? p sums-path))))
         p))
     (for ([p (in-list dir-files)]
           #:unless (or (set-member? recorded-paths (path->string (find-relative-path root p)))
                        (set-member? recorded-paths (path->string (find-relative-path dir p)))))
       (if (artifact-dir-current? ad)
           (provenance-drift! "~a/~a: ~a is not bound by SHA256SUMS"
                              (artifact-dir-family ad)
                              (artifact-dir-version ad)
                              p)
           (provenance-note! "~a/~a: ~a is not bound by SHA256SUMS (historical directory)"
                             (artifact-dir-family ad)
                             (artifact-dir-version ad)
                             p)))]
    [else
     (if (artifact-dir-current? ad)
         (provenance-drift! "~a/~a: declares artifact files but binds no SHA256SUMS"
                            (artifact-dir-family ad)
                            (artifact-dir-version ad))
         (provenance-note! "~a/~a: has no SHA256SUMS (historical directory)"
                           (artifact-dir-family ad)
                           (artifact-dir-version ad)))]))

;; Provenance-named field detection: the last path segment must carry
;; head/sha/commit/tree semantics (segment boundaries, so plan-sha256 or
;; observed-sha256 do NOT qualify) and the value must be a bare 40-hex
;; string (64-hex plan/digest ids are not commit identities).
(define provenance-key-rx #px"(^|[-_])(head|sha|commit|tree)([-_]|$)")

(define (walk-provenance-values v path accept!)
  ;; R1: provenance semantics come from the LAST path segment, decided at the
  ;; leaf — a non-provenance ancestor key must not disable its descendants.
  (define last-segment (cadr (regexp-match #px"([^/]*)$" path)))
  (cond
    [(hash? v)
     (for* ([k (in-hash-keys v)]
            [seg (in-value (format "~a" k))])
       (walk-provenance-values (hash-ref v k) (string-append path "/" seg) accept!))]
    [(list? v)
     (for ([x (in-list v)]
           [i (in-naturals)])
       (walk-provenance-values x (string-append path "/" (number->string i)) accept!))]
    [(string? v)
     (when (and (regexp-match? hex40-rx v) (regexp-match? provenance-key-rx last-segment))
       (accept! path v))]
    [else (void)]))

(define (check-provenance-heads! root ad tip)
  (define dir (artifact-dir-path ad))
  (for ([f (in-list (sort (map path->string (directory-list dir)) string<?))]
        #:when (string-suffix? f ".json"))
    (define p (build-path dir f))
    (define rel (string-append (artifact-dir-family ad) "/" (artifact-dir-version ad) "/" f))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (when (eq? parsed 'parse-error)
      (provenance-drift! "~a: JSON does not parse" rel))
    (unless (eq? parsed 'parse-error)
      (define heads '())
      (walk-provenance-values parsed
                              ""
                              (lambda (path value) (set! heads (cons (cons path value) heads))))
      (for ([entry (in-list (reverse heads))])
        (define field-path (string-append rel (car entry)))
        (define sha (cdr entry))
        (cond
          [(not (commit-exists? root sha))
           (if (artifact-dir-current? ad)
               (provenance-drift! "~a: recorded head ~a does not resolve to a commit" field-path sha)
               (provenance-note!
                "~a: recorded head ~a does not resolve in this checkout (historical record)"
                field-path
                sha))]
          [(and (artifact-dir-current? ad) (not (is-ancestor? root sha tip)))
           (provenance-drift!
            "~a: recorded head ~a is not an ancestor of the wave tip ~a (stale provenance)"
            field-path
            sha
            tip)]
          [(and (not (artifact-dir-current? ad)) (not (is-ancestor? root sha tip)))
           (provenance-note!
            "~a: recorded head ~a is not an ancestor of the tip (historical pinned record)"
            field-path
            sha)]
          [else (void)])))))

;; R3: recorded tree fields must match the actual tree of their sibling
;; recorded head (convention: within the same object, a tree-named field
;; pairs with a head-named field). Current wave: hard drift; historical:
;; reported.
(define (hex40-value? x)
  (and (string? x) (regexp-match? hex40-rx x)))

(define (tree-pair-of names)
  ;; A tree-named 40-hex field pairs with a head-named 40-hex sibling field.
  (define head-v
    (for/first ([(k2 v2) (in-hash names)]
                #:when (and (regexp-match? #px"(^|[-_])head([-_]|$)" k2) (hex40-value? v2)))
      v2))
  (and head-v
       (for/first ([(k v) (in-hash names)]
                   #:when (and (regexp-match? #px"(^|[-_])tree([-_]|$)" k) (hex40-value? v)))
         (cons v head-v))))

(define (walk-tree-pairs v acc)
  (cond
    [(hash? v)
     (define names
       (for/hash ([k (in-hash-keys v)])
         (values (format "~a" k) (hash-ref v k))))
     (define pair (tree-pair-of names))
     (define acc*
       (if pair
           (cons pair acc)
           acc))
     (for/fold ([acc* acc*]) ([k (in-hash-keys v)])
       (walk-tree-pairs (hash-ref v k) acc*))]
    [(list? v)
     (for/fold ([acc* acc]) ([x (in-list v)])
       (walk-tree-pairs x acc*))]
    [else acc]))

(define (check-tree-pairs! root ad)
  (define dir (artifact-dir-path ad))
  (for ([f (in-list (sort (map path->string (directory-list dir)) string<?))]
        #:when (string-suffix? f ".json"))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file (build-path dir f) read-json)))
    (unless (eq? parsed 'parse-error)
      (define pairs (walk-tree-pairs parsed '()))
      (for ([pair (in-list pairs)])
        (define tree (car pair))
        (define head (cdr pair))
        (define rel (string-append (artifact-dir-family ad) "/" (artifact-dir-version ad) "/" f))
        (cond
          [(not (commit-exists? root head))
           (if (artifact-dir-current? ad)
               (provenance-drift! "~a: paired head ~a does not resolve; tree ~a cannot be verified"
                                  rel
                                  head
                                  tree)
               (provenance-note! "~a: paired head ~a does not resolve in this checkout (historical)"
                                 rel
                                 head))]
          [else
           (define actual-tree (commit-tree root head))
           (cond
             [(not (equal? actual-tree tree))
              (if (artifact-dir-current? ad)
                  (provenance-drift! "~a: recorded tree ~a does not match the actual tree of ~a"
                                     rel
                                     tree
                                     head)
                  (provenance-note!
                   "~a: recorded tree ~a does not match the actual tree of ~a (historical)"
                   rel
                   tree
                   head))]
             [else (void)])])))))

(define (check-canonical-json! root ad)
  (define dir (artifact-dir-path ad))
  (for ([f (in-list (sort (map path->string (directory-list dir)) string<?))]
        #:when (string-suffix? f ".json"))
    (define p (build-path dir f))
    (define rel (string-append (artifact-dir-family ad) "/" (artifact-dir-version ad) "/" f))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      (cond
        [(artifact-dir-current? ad)
         (define canonical (canonical-json-string parsed))
         (unless (equal? canonical (file->string p))
           (provenance-drift!
            "~a: JSON is not in canonical form (sorted keys, 1-space indent, trailing newline)"
            rel))]
        [else (provenance-note! "~a: canonical form not enforced on historical artifacts" rel)]))))

;; Cross-artifact consistency: inside an artifact declaring a structured
;; `timing` object with `*-ms` keys, every prose "<n> ms" mention must agree
;; exactly with the structured numbers (F7: prose 260/245 ms vs
;; eager-fallback-ms 247/256 ms).
(define prose-ms-rx #px"([0-9]+)[ ]ms")
(define timing-key-rx #px"-ms$")

(define (collect-timing-ms timing)
  (for/fold ([acc '()]) ([k (in-hash-keys timing)])
    (define v (hash-ref timing k))
    (define nums
      (cond
        [(list? v)
         (for/list ([x (in-list v)]
                    #:when (real? x))
           x)]
        [(real? v) (list v)]
        [else '()]))
    (append acc nums)))

(define (walk-prose-ms v path acc)
  (cond
    [(hash? v)
     (for/fold ([acc acc]) ([k (in-hash-keys v)])
       (walk-prose-ms (hash-ref v k) (string-append path "/" (format "~a" k)) acc))]
    [(list? v)
     (for/fold ([acc acc])
               ([x (in-list v)]
                [i (in-naturals)])
       (walk-prose-ms x (string-append path "/" (number->string i)) acc))]
    [(string? v)
     (for/fold ([acc acc]) ([m (in-list (regexp-match* prose-ms-rx v #:match-select values))])
       (cons (cons path (list-ref m 1)) acc))]
    [else acc]))

(define (check-cross-artifact-consistency! root ad)
  (define dir (artifact-dir-path ad))
  (for ([f (in-list (sort (map path->string (directory-list dir)) string<?))]
        #:when (string-suffix? f ".json"))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file (build-path dir f) read-json)))
    (unless (eq? parsed 'parse-error)
      ;; racket/json parses objects into hasheq (symbol keys); accept both
      ;; key types so the check is independent of the parser.
      (define timing (and (hash? parsed) (hash-ref parsed "timing" (hash-ref parsed 'timing #f))))
      (when (and (hash? timing)
                 (for/or ([k (in-hash-keys timing)])
                   (regexp-match? timing-key-rx (format "~a" k))))
        (define structured (collect-timing-ms timing))
        (define prose-mentions (walk-prose-ms parsed "" '()))
        (for ([mention (in-list prose-mentions)])
          (define n (string->number (cdr mention)))
          (unless (member n structured)
            (provenance-drift!
             "~a/~a: prose value ~a ms disagrees with structured timing ~a (sources: prose at ~a vs timing.*-ms)"
             (artifact-dir-family ad)
             (artifact-dir-version ad)
             n
             (string-join (map number->string (sort structured <)) "/")
             (car mention))))))))

;; Current-wave observed digests must be bound to committed raw bytes.
(define (check-observed-digests! root ad)
  (define dir (artifact-dir-path ad))
  (define raw-dir (build-path dir "raw"))
  (define raw-digests
    (if (directory-exists? raw-dir)
        (for/hash ([p (in-directory raw-dir)]
                   #:when (file-exists? p))
          (values (sha256-file-hex p) #t))
        (hash)))
  (for ([f (in-list (sort (map path->string (directory-list dir)) string<?))]
        #:when (string-suffix? f ".json"))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file (build-path dir f) read-json)))
    (unless (eq? parsed 'parse-error)
      (let loop ([v parsed]
                 [path (string-append (artifact-dir-family ad) "/" (artifact-dir-version ad) "/" f)])
        (cond
          [(hash? v)
           (for ([k (in-hash-keys v)])
             (loop (hash-ref v k) (string-append path "/" (format "~a" k))))]
          [(list? v)
           (for ([x (in-list v)]
                 [i (in-naturals)])
             (loop x (string-append path "/" (number->string i))))]
          [(string? v)
           (when (and (regexp-match? hex64-rx v)
                      (string-contains? path "observed-sha256")
                      (not (hash-has-key? raw-digests v)))
             (provenance-drift! "~a: observed digest ~a is not bound to any committed raw artifact"
                                path
                                v))]
          [else (void)])))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (run! root current-wave wave-tip)
  (define dirs (discover-artifact-dirs! root current-wave))
  (for ([ad (in-list dirs)])
    (check-sums! root ad)
    (check-canonical-json! root ad)
    (check-provenance-heads! root ad wave-tip)
    (check-cross-artifact-consistency! root ad)
    (check-tree-pairs! root ad)
    (when (artifact-dir-current? ad)
      (check-observed-digests! root ad)))
  (finish!))

(module+ main
  (define root (current-directory))
  (define current-wave #f)
  (define wave-tip #f)
  (command-line
   #:program "verify-artifact-provenance"
   #:once-each ["--root" r "repository root" (set! root (simplify-path (path->complete-path r)))]
   ["--current-wave"
    w
    "version directory treated as the current wave (strict mode)"
    (set! current-wave w)]
   ["--wave-tip" t "commit the current wave's recorded heads must descend from" (set! wave-tip t)])
  (run! root current-wave (or wave-tip (git-head root))))
