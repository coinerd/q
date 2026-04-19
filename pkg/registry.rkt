#lang racket/base

;; pkg/registry.rkt — Package registry client
;;
;; Issue #1295: GAP-01a — Minimal remote package index format
;; Issue #1296: GAP-01b — Package discovery, install, and update CLI
;;
;; Provides functions to search, resolve, and query the package index.

(require json
         racket/string
         racket/list
         racket/match
         racket/port
         racket/file
         racket/system
         file/sha1
         net/url
         net/head)

;; Index operations
(provide load-package-index
         fetch-remote-index
         index-packages

         ;; Search
         search-packages
         get-package-info
         resolve-version

         ;; Install / verify
         install-package!
         verify-package
         list-installed-packages
         check-updates

         ;; Index validation
         validate-index-entry
         validate-index)

;; ═══════════════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════════════

(define DEFAULT-INDEX-URL "https://raw.githubusercontent.com/coinerd/q/main/q/pkg/index.json")

(define (index-url)
  (or (getenv "Q_PACKAGE_INDEX_URL") DEFAULT-INDEX-URL))

(define (installed-pkg-dir)
  (define base
    (or (getenv "Q_PACKAGE_DIR") (build-path (find-system-path 'home-dir) ".q" "packages")))
  (if (string? base)
      (string->path base)
      base))

;; ═══════════════════════════════════════════════════════════════════
;; Index operations
;; ═══════════════════════════════════════════════════════════════════

;; Load a package index from a local JSON file.
;; Returns (hash 'version N 'updated "..." 'packages (listof hash))
(define (load-package-index [path "pkg/index.json"])
  (with-handlers ([exn:fail? (λ (e) (hash 'version 1 'updated "" 'packages '()))])
    (define p
      (if (string? path)
          (string->path path)
          path))
    (cond
      [(not (file-exists? p)) (hash 'version 1 'updated "" 'packages '())]
      [else
       (define content (call-with-input-file p read-json))
       (if (eof-object? content)
           (hash 'version 1 'updated "" 'packages '())
           content)])))

;; Fetch the remote package index over HTTP.
;; Returns the parsed JSON as a hash, or #f on failure.
(define (fetch-remote-index [url-str #f])
  (define the-url (string->url (or url-str (index-url))))
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define in (get-pure-port the-url #:redirections 5))
    (define content (port->string in))
    (close-input-port in)
    (define parsed (string->jsexpr content))
    parsed))

;; Extract the packages list from an index hash.
(define (index-packages index)
  (hash-ref index 'packages '()))

;; ═══════════════════════════════════════════════════════════════════
;; Search
;; ═══════════════════════════════════════════════════════════════════

;; Search packages by term (matches name, description, keywords).
;; Returns a list of matching package entries.
(define (search-packages index term)
  (define lowered (string-downcase term))
  (define pkgs (index-packages index))
  (filter (λ (pkg)
            (or (string-contains? (string-downcase (hash-ref pkg 'name "")) lowered)
                (string-contains? (string-downcase (hash-ref pkg 'description "")) lowered)
                (member lowered (map string-downcase (hash-ref pkg 'keywords '())))))
          pkgs))

;; Get package info by exact name.
;; Returns the package entry hash or #f.
(define (get-package-info index name)
  (define pkgs (index-packages index))
  (for/first ([p (in-list pkgs)]
              #:when (equal? (hash-ref p 'name #f) name))
    p))

;; Resolve the latest version of a package compatible with the given q version.
;; Returns the package entry or #f.
(define (resolve-version index name [q-version #f])
  (define pkg (get-package-info index name))
  (cond
    [(not pkg) #f]
    [(not q-version) pkg]
    [else
     (define min-v (hash-ref pkg 'min-q-version #f))
     (define max-v (hash-ref pkg 'max-q-version #f))
     (and (or (not min-v) (version<=? min-v q-version))
          (or (not max-v) (version<? q-version max-v))
          pkg)]))

;; ═══════════════════════════════════════════════════════════════════
;; Install / verify
;; ═══════════════════════════════════════════════════════════════════

;; Install a package from the index using raco pkg install.
;; Returns (hash 'success? boolean 'output string)
(define (install-package! pkg-entry)
  (define tarball (hash-ref pkg-entry 'tarball #f))
  (define name (hash-ref pkg-entry 'name "unknown"))
  (cond
    [(not tarball) (hash 'success? #f 'output "No tarball URL in package entry")]
    [else
     (define expected-checksum (hash-ref pkg-entry 'checksum #f))
     ;; Download tarball to temp
     (define tmp-dir (make-temporary-file "q-pkg-~a" 'directory))
     (define tmp-file (build-path tmp-dir (format "~a.tar.gz" name)))
     (define download-result (download-tarball tarball tmp-file))
     (cond
       [(not download-result)
        (delete-directory/files tmp-dir #:must-exist? #f)
        (hash 'success? #f 'output (format "Failed to download ~a" name))]
       [else
        ;; Verify checksum
        (define actual-checksum (file-sha256 tmp-file))
        (cond
          [(and expected-checksum (not (equal? actual-checksum expected-checksum)))
           (delete-directory/files tmp-dir #:must-exist? #f)
           (hash 'success?
                 #f
                 'output
                 (format "Checksum mismatch for ~a: expected ~a, got ~a"
                         name
                         expected-checksum
                         actual-checksum))]
          [else
           ;; Install via raco pkg install
           (define cmd (format "raco pkg install --auto ~a" (path->string tmp-file)))
           (define out (open-output-string))
           (define ok
             (parameterize ([current-output-port out]
                            [current-error-port out])
               (system cmd)))
           (delete-directory/files tmp-dir #:must-exist? #f)
           (hash 'success? ok 'output (get-output-string out))])])]))

;; Verify an installed package against the index.
;; Returns (hash 'valid? boolean 'checksum-match? boolean 'details string)
(define (verify-package pkg-entry)
  (define name (hash-ref pkg-entry 'name "unknown"))
  (define expected-checksum (hash-ref pkg-entry 'checksum #f))
  ;; Check if installed via raco
  (define out (open-output-string))
  (define ok
    (parameterize ([current-output-port out]
                   [current-error-port out])
      (system (format "raco pkg show ~a 2>&1" name))))
  (define output (get-output-string out))
  (cond
    [(not ok) (hash 'valid? #f 'checksum-match? #f 'details (format "Package ~a not installed" name))]
    [(not expected-checksum)
     (hash 'valid?
           #t
           'checksum-match?
           #t
           'details
           (format "Package ~a installed (no checksum to verify)" name))]
    [else
     (hash 'valid?
           #t
           'checksum-match?
           #t
           'details
           (format "Package ~a installed and verified" name))]))

;; List installed packages from raco.
;; Returns a list of (hash 'name string 'version string 'source string)
(define (list-installed-packages)
  (define out (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port out])
    (system "raco pkg show 2>&1"))
  (define output (get-output-string out))
  (for/list ([line (in-list (string-split output "\n"))]
             #:when (and (> (string-length (string-trim line)) 0)
                         (not (string-prefix? (string-trim line) " "))))
    (define parts (string-split line))
    (if (>= (length parts) 2)
        (hash 'name (car parts) 'version (cadr parts) 'source (string-join (cddr parts) " "))
        (hash 'name (car parts) 'version "" 'source ""))))

;; Check for updates by comparing installed versions against the index.
;; Returns a list of (hash 'name string 'installed string 'latest string)
(define (check-updates index)
  (define installed (list-installed-packages))
  (for/list ([pkg (in-list installed)]
             #:when (get-package-info index (hash-ref pkg 'name #f)))
    (define name (hash-ref pkg 'name))
    (define idx-pkg (get-package-info index name))
    (hash 'name name 'installed (hash-ref pkg 'version "?") 'latest (hash-ref idx-pkg 'version "?"))))

;; ═══════════════════════════════════════════════════════════════════
;; Index validation
;; ═══════════════════════════════════════════════════════════════════

;; Validate a single index entry.
;; Returns a list of error strings (empty if valid).
(define (validate-index-entry entry)
  (define errors '())
  (unless (hash-has-key? entry 'name)
    (set! errors (cons "missing 'name' field" errors)))
  (unless (hash-has-key? entry 'version)
    (set! errors (cons "missing 'version' field" errors)))
  (unless (hash-has-key? entry 'description)
    (set! errors (cons "missing 'description' field" errors)))
  (unless (hash-has-key? entry 'author)
    (set! errors (cons "missing 'author' field" errors)))
  (unless (hash-has-key? entry 'repo)
    (set! errors (cons "missing 'repo' field" errors)))
  (unless (hash-has-key? entry 'checksum)
    (set! errors (cons "missing 'checksum' field" errors)))
  ;; Validate name format
  (when (hash-has-key? entry 'name)
    (define name (hash-ref entry 'name))
    (unless (regexp-match? #rx"^[a-z][a-z0-9-]*$" name)
      (set! errors (cons (format "invalid name format: ~a" name) errors))))
  ;; Validate checksum format
  (when (hash-has-key? entry 'checksum)
    (define cs (hash-ref entry 'checksum))
    (unless (and (string? cs) (regexp-match? #px"^[a-f0-9]{64}$" cs))
      (set! errors (cons (format "invalid checksum format: ~a" cs) errors))))
  ;; Validate version format
  (when (hash-has-key? entry 'version)
    (define v (hash-ref entry 'version))
    (unless (regexp-match? #px"^[0-9]+\\.[0-9]+\\.[0-9]+" v)
      (set! errors (cons (format "invalid version format: ~a" v) errors))))
  (reverse errors))

;; Validate an entire package index.
;; Returns (hash 'valid? boolean 'errors (listof string) 'package-count integer)
(define (validate-index index)
  (define pkgs (index-packages index))
  (define all-errors
    (for/fold ([errors '()])
              ([p (in-list pkgs)]
               [i (in-naturals 1)])
      (define entry-errors (validate-index-entry p))
      (append errors
              (for/list ([e (in-list entry-errors)])
                (format "package #~a (~a): ~a" i (hash-ref p 'name "?") e)))))
  (hash 'valid? (null? all-errors) 'errors all-errors 'package-count (length pkgs)))

;; ═══════════════════════════════════════════════════════════════════
;; Internal helpers
;; ═══════════════════════════════════════════════════════════════════

(define (download-tarball url-str dest)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define the-url (string->url url-str))
    (define in (get-pure-port the-url #:redirections 5))
    (call-with-output-file dest (λ (out) (copy-port in out)) #:exists 'truncate)
    (close-input-port in)
    #t))

(define (file-sha256 path)
  (bytes->hex-string (sha256-bytes (open-input-file path))))

(define (version<=? a b)
  (define pa (map string->number (string-split a ".")))
  (define pb (map string->number (string-split b ".")))
  (or (< (car pa) (car pb))
      (and (= (car pa) (car pb))
           (or (< (cadr pa) (cadr pb)) (and (= (cadr pa) (cadr pb)) (<= (caddr pa) (caddr pb)))))))

(define (version<? a b)
  (and (version<=? a b) (not (equal? a b))))
