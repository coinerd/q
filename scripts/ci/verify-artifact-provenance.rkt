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
(define version-dir-rx
  ;; every declared artifact version: vX.Y.Z plus any dash suffix used by
  ;; the historical records (-wN waves, -cN candidates, -final, -census,
  ;; -hotspots, -prepared-env, ...)
  #px"^v[0-9]+\\.[0-9]+\\.[0-9]+(-[A-Za-z0-9]+)*$")

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

(define (u-escape n)
  ;; Python json.dumps(ensure_ascii=True) uses lowercase four-digit escapes.
  (string-append "\\u" (string-downcase (~r n #:base 16 #:min-width 4 #:pad-string "0"))))

(define (json-escape s)
  (string-append
   "\""
   (for/fold ([acc ""]) ([c (in-string s)])
     (string-append
      acc
      (cond
        [(char=? c #\") "\\\""]
        [(char=? c #\\) "\\\\"]
        [(char=? c #\backspace) "\\b"]
        [(char=? c #\newline) "\\n"]
        [(char=? c #\page) "\\f"]
        [(char=? c #\return) "\\r"]
        [(char=? c #\tab) "\\t"]
        [(char<? c #\space) (u-escape (char->integer c))]
        [(char<=? #\space c #\rubout) (string c)]
        [(<= (char->integer c) #xFFFF) (u-escape (char->integer c))]
        [else
         ;; astral chars are escaped as a surrogate pair
         (define n (- (char->integer c) #x10000))
         (string-append
          "\\u"
          (string-downcase
           (~r (+ #xD800 (quotient n #x400)) #:base 16 #:min-width 4 #:pad-string "0"))
          "\\u"
          (string-downcase
           (~r (+ #xDC00 (remainder n #x400)) #:base 16 #:min-width 4 #:pad-string "0")))])))
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
    ;; racket/json represents JSON null as the symbol 'null (a JSON string
    ;; "null" is the string). The empty list is an empty array, handled above.
    [(eq? v 'null) "null"]
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

;; Recursive: every directory named like a version under artifacts/ (the
;; declared scope is artifacts/**/v*/), at any nesting depth. A version
;; directory's own subdirectories are not descended into; the family is the
;; relative path between artifacts/ and the version directory.
(define (discover-artifact-dirs! root current-wave)
  (define artifacts-root (build-path root "artifacts"))
  (define (walk dir rel)
    (for/fold ([acc '()]) ([n (in-list (sort (directory-list dir) string<? #:key path->string))])
      (define name (path->string n))
      (define sub (build-path dir n))
      (cond
        [(not (directory-exists? sub)) acc]
        [(regexp-match? version-dir-rx name)
         (cons (artifact-dir sub rel name (and current-wave (string=? name current-wave))) acc)]
        [else (append (walk sub (string-append rel (if (string=? rel "") "" "/") name)) acc)])))
  (if (not (directory-exists? artifacts-root))
      '()
      (walk artifacts-root "")))

;; ---------------------------------------------------------------------------
;; Checks
;; ---------------------------------------------------------------------------

;; Verify a SHA256SUMS binding: every recorded digest must match the named
;; file (paths are relative to the repository root and may include files
;; outside the artifact directory, e.g. the wave report), and every file
;; under the directory must be recorded. "Regenerating" the checksum file is
;; a per-line digest comparison plus coverage, never a format assumption.
;; R6: artifact JSON checks are recursive over the version directory;
;; only the raw/ subtree is exempt (captured payloads, intentionally
;; unenforced). SHA256SUMS coverage is recursive too, so a bound nested
;; artifact can no longer bypass the checks.
(define (artifact-json-files dir)
  (sort (for/list ([p (in-directory dir)]
                   #:when (and (file-exists? p)
                               (string-suffix? (path->string (file-name-from-path p)) ".json")
                               (let ([rel (path->string (find-relative-path dir p))])
                                 (and (not (equal? rel "raw"))
                                      ;; path must not be inside raw/
                                      (not (regexp-match? #px"(^|/)raw/" rel))))))
          p)
        string<?
        #:key path->string))

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
       (if (artifact-dir-current? ad)
           (provenance-drift! "~a/~a: SHA256SUMS contains malformed lines"
                              (artifact-dir-family ad)
                              (artifact-dir-version ad))
           (provenance-note! "~a/~a: SHA256SUMS contains malformed lines (historical)"
                             (artifact-dir-family ad)
                             (artifact-dir-version ad))))
     (define recorded-paths (list->set (map cdr recorded)))
     ;; R4: for the current wave the binding itself must be canonical —
     ;; regenerating it reproduces byte-identically (sorted repository-root
     ;; relative paths, "<hex>  <path>" lines, LF-terminated). Historical
     ;; directories keep their recorded spelling.
     (when (and (artifact-dir-current? ad) (not (member #f (map car recorded))) (null? broken))
       ;; R5: canonical regeneration reads every recorded target; skip it
       ;; when malformed lines or missing/broken files exist — those are
       ;; already typed drift above, and hashing here could raise a raw
       ;; filesystem exception instead of the typed refusal.
       ;; P2 (R11): recorded entries may use either spelling; resolve the same
       ;; way the digest check does, so a directory-relative entry whose file
       ;; exists yields a typed "not canonical" refusal instead of an uncaught
       ;; filesystem exception.
       (define (resolve-recorded line)
         (cond
           [(file-exists? (build-path root line)) (build-path root line)]
           [(file-exists? (build-path dir line)) (build-path dir line)]
           [else #f]))
       (define canonical
         (string-join (for/list ([line (in-list (sort (map cdr recorded) string<?))])
                        (define target (resolve-recorded line))
                        (string-append (if target
                                           (sha256-file-hex target)
                                           "")
                                       "  "
                                       ;; the canonical spelling is repository-root
                                       ;; relative, so a directory-relative entry is
                                       ;; refused as non-canonical (never a crash)
                                       (if target
                                           (path->string (find-relative-path root target))
                                           line)))
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
;; `tree` is deliberately absent: a tree-named field is not a commit
;; identity — it is validated against its paired head by check-tree-pairs!.
(define provenance-key-rx #px"(^|[-_])(head|sha|commit)([-_]|$)")

;; Fields that CLAIM to name the wave's own verified head must equal the wave
;; tip exactly (the contract's "recorded head must match the commit", F3/F7);
;; fields that merely record an observation head (recorded-head, head, base,
;; pinned ...) are pinned by construction — a committed artifact cannot carry
;; its own commit hash, so they are required to be ancestors of the tip
;; instead (provenance-drift when they are not).
(define exact-head-key-rx
  #px"(^|[-_])(implementation-sha|reviewed-sha|verified-head|receipt-head)([-_]|$)")

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
  (for ([p (in-list (artifact-json-files dir))])
    (define rel
      (string-append (artifact-dir-family ad)
                     "/"
                     (artifact-dir-version ad)
                     "/"
                     (path->string (find-relative-path dir p))))
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
          [(and (artifact-dir-current? ad)
                (regexp-match? exact-head-key-rx (cadr (regexp-match #px"([^/]*)$" field-path)))
                (not (string=? sha tip)))
           (provenance-drift!
            "~a: head-claiming field ~a does not equal the wave tip ~a (exact head binding)"
            field-path
            sha
            tip)]
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
  (for ([p (in-list (artifact-json-files dir))])
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      (define rel
        (string-append (artifact-dir-family ad)
                       "/"
                       (artifact-dir-version ad)
                       "/"
                       (path->string (find-relative-path dir p))))
      (define pairs (walk-tree-pairs parsed '()))
      (for ([pair (in-list pairs)])
        (define tree (car pair))
        (define head (cdr pair))
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
  (for ([p (in-list (artifact-json-files dir))])
    (define rel
      (string-append (artifact-dir-family ad)
                     "/"
                     (artifact-dir-version ad)
                     "/"
                     (path->string (find-relative-path dir p))))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      (cond
        [(artifact-dir-current? ad)
         (define canonical (canonical-json-string parsed))
         (unless (equal? canonical (file->string p))
           (provenance-drift!
            "~a: JSON is not in canonical form (sorted keys, 1-space indent, trailing newline, Python-compatible escapes)"
            rel))]
        [else (provenance-note! "~a: canonical form not enforced on historical artifacts" rel)]))))

;; Cross-artifact consistency: inside an artifact declaring a structured
;; `timing` object with `*-ms` keys, every prose "<n> ms" mention must agree
;; exactly with the structured numbers (F7: prose 260/245 ms vs
;; eager-fallback-ms 247/256 ms).
(define prose-ms-rx #px"([0-9]+)[ ]ms")
(define timing-key-rx #px"-ms$")

(define (collect-timing-ms timing)
  ;; R5: only keys matching the *-ms convention feed the prose agreement
  ;; check — unrelated numeric fields must never legitimize prose values.
  (for/fold ([acc '()])
            ([k (in-hash-keys timing)]
             #:when (regexp-match? timing-key-rx (format "~a" k)))
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

;; R8: prose ms values in markdown reports BOUND by this directory's
;; SHA256SUMS must agree with the version's structured timing. Blocks that
;; are explicitly labeled (red-fixture, refusal, history, older waves) are
;; exempt — a report may legitimately cite non-delivered values there.
(define report-marker-rx #px"(red-first|fixture|refus|blocked-branch|v1\\.00\\.(2[0-9]|30)-|histor)")

(define (report-blocks text)
  ;; Group lines into blocks: a non-blank, non-indented line starts a block;
  ;; blank and indented lines continue the current one (markdown lists nest
  ;; continuation lines under their first line).
  (define rev-blocks '())
  (define cur '())
  (define (flush!)
    (unless (null? cur)
      (set! rev-blocks (cons cur rev-blocks))
      (set! cur '())))
  (for ([l (in-list (string-split text "\n"))])
    (if (and (non-empty-string? (string-trim l))
             (not (and (positive? (string-length l)) (char-whitespace? (string-ref l 0)))))
        (begin
          (flush!)
          (set! cur (list l)))
        (set! cur (cons l cur))))
  (flush!)
  (reverse rev-blocks))

(define (check-cross-artifact-consistency! root ad)
  (define dir (artifact-dir-path ad))
  ;; version-wide union of structured timing values, for the report scan
  (define union-box (box '()))
  (for ([p (in-list (artifact-json-files dir))])
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      ;; racket/json parses objects into hasheq (symbol keys); accept both
      ;; key types so the check is independent of the parser.
      (define timing (and (hash? parsed) (hash-ref parsed "timing" (hash-ref parsed 'timing #f))))
      (when (and (hash? timing)
                 (for/or ([k (in-hash-keys timing)])
                   (regexp-match? timing-key-rx (format "~a" k))))
        (define structured (collect-timing-ms timing))
        (set-box! union-box (remove-duplicates (append (unbox union-box) structured)))
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
             (car mention)))))))
  ;; Report-vs-JSON agreement is decided against the union of EVERY artifact's
  ;; structured timing, so it runs only after the JSON loop has completed — a
  ;; report may cite a value recorded by any artifact in the version.
  (define union (unbox union-box))
  (when (and (artifact-dir-current? ad) (pair? union))
    (define sums-path (build-path dir "SHA256SUMS"))
    (when (file-exists? sums-path)
      (define sums-line-rx #px"^([0-9a-f]{64})  (.+)$")
      (for ([l (in-list (string-split (file->string sums-path) "\n"))]
            #:when (and (non-empty-string? (string-trim l))
                        (let ([m (regexp-match sums-line-rx l)])
                          ;; group 1 is the digest, group 2 the path
                          (and m (string-suffix? (caddr m) ".md"))))
            [md-rel (in-value (caddr (regexp-match sums-line-rx l)))]
            [md-path (in-value (build-path root (caddr (regexp-match sums-line-rx l))))]
            #:when (file-exists? md-path))
        (for ([blk (in-list (report-blocks (file->string md-path)))]
              #:unless (regexp-match? report-marker-rx (string-join blk " "))
              [m (in-list (regexp-match* prose-ms-rx (string-join blk " ") #:match-select values))])
          (define n (string->number (list-ref m 1)))
          (unless (member n union)
            (provenance-drift!
             "~a: bound report prose value ~a ms disagrees with structured timing ~a (report vs JSON agreement)"
             md-rel
             n
             (string-join (map number->string (sort union <)) "/"))))))))

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
  (for ([p (in-list (artifact-json-files dir))])
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      (let loop ([v parsed]
                 [path (string-append (artifact-dir-family ad)
                                      "/"
                                      (artifact-dir-version ad)
                                      "/"
                                      (path->string (find-relative-path dir p)))])
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

;; R8: a recorded `sha256` field beside a `path` field must match the bytes
;; of the file it names. Current wave: typed drift; historical: note (post-hoc
;; byte drift is reported, never rewritten).
(define (check-recorded-file-digests! root ad)
  (define dir (artifact-dir-path ad))
  (define rel-base (string-append (artifact-dir-family ad) "/" (artifact-dir-version ad)))
  (for ([p (in-list (artifact-json-files dir))])
    (define rel (string-append rel-base "/" (path->string (find-relative-path dir p))))
    (define parsed
      (with-handlers ([exn:fail? (lambda (_e) 'parse-error)])
        (call-with-input-file p read-json)))
    (unless (eq? parsed 'parse-error)
      (let loop ([v parsed])
        (cond
          [(hash? v)
           (define path-field (or (hash-ref v "path" #f) (hash-ref v 'path #f)))
           (define dig-field (or (hash-ref v "sha256" #f) (hash-ref v 'sha256 #f)))
           (when (and (string? path-field) (string? dig-field) (regexp-match? hex64-rx dig-field))
             (define target
               (cond
                 [(file-exists? (build-path root path-field)) (build-path root path-field)]
                 [(file-exists? (build-path dir path-field)) (build-path dir path-field)]
                 [else #f]))
             (cond
               [(not target)
                (if (artifact-dir-current? ad)
                    (provenance-drift! "~a: recorded sha256 ~a names no existing file ~a"
                                       rel
                                       dig-field
                                       path-field)
                    (provenance-note! "~a: recorded sha256 names no existing file ~a (historical)"
                                      rel
                                      path-field))]
               [(not (equal? dig-field (sha256-file-hex target)))
                (if (artifact-dir-current? ad)
                    (provenance-drift! "~a: recorded sha256 ~a does not match the bytes of ~a"
                                       rel
                                       dig-field
                                       path-field)
                    (provenance-note! "~a: recorded sha256 does not match bytes of ~a (historical)"
                                      rel
                                      path-field))]
               [else (void)]))
           (for ([k (in-hash-keys v)])
             (loop (hash-ref v k)))]
          [(list? v)
           (for ([x (in-list v)])
             (loop x))]
          [else (void)])))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (run! root current-wave wave-tip [only-current-wave? #f])
  (define dirs (discover-artifact-dirs! root current-wave))
  (define checked
    ;; `--only-current-wave` is the constrained-environment path: it checks the
    ;; current wave's directory alone, so a latency-bounded CI shard (and a
    ;; shallow checkout, where historical heads cannot resolve) does not pay for
    ;; the full historical sweep. The full sweep remains the default and is what
    ;; the wave evidence records.
    (if only-current-wave?
        (filter (lambda (ad) (and (artifact-dir-current? ad) ad)) dirs)
        dirs))
  ;; Fail closed: on the constrained path a missing/renamed current-wave
  ;; directory must be a typed refusal, never a silent success.
  (when (and only-current-wave? (null? checked))
    (provenance-drift!
     "current wave ~a: no artifact version directory named ~a was found under artifacts/** (--only-current-wave)"
     current-wave
     current-wave))
  (for ([ad (in-list checked)])
    (check-sums! root ad)
    (check-canonical-json! root ad)
    (check-provenance-heads! root ad wave-tip)
    (check-cross-artifact-consistency! root ad)
    (check-tree-pairs! root ad)
    (when (artifact-dir-current? ad)
      (check-observed-digests! root ad))
    (check-recorded-file-digests! root ad))
  (finish!))

(module+ main
  (define root (current-directory))
  (define current-wave #f)
  (define wave-tip #f)
  (define only-current-wave? #f)
  (command-line
   #:program "verify-artifact-provenance"
   #:once-each ["--root" r "repository root" (set! root (simplify-path (path->complete-path r)))]
   ["--current-wave"
    w
    "version directory treated as the current wave (strict mode)"
    (set! current-wave w)]
   ["--wave-tip" t "commit the current wave's recorded heads must descend from" (set! wave-tip t)]
   ["--only-current-wave"
    "check the current wave's directory only (constrained-environment path)"
    (set! only-current-wave? #t)])
  (when (and only-current-wave? (not current-wave))
    (displayln "verify-artifact-provenance: --only-current-wave requires --current-wave")
    (exit 2))
  (run! root current-wave (or wave-tip (git-head root)) only-current-wave?))
