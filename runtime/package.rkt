#lang racket/base

;; runtime/package.rkt — q package manager: install/remove/list from local path
;;
;; Provides:
;;   - qpm-package struct (manifest, install-dir, status)
;;   - current-packages-dir parameter
;;   - list-packages, package-installed?, install-package-from-dir,
;;     remove-package, package-info

(require racket/contract
         racket/list
         racket/file
         racket/port
         racket/string
         "../extensions/manifest.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide
 (struct-out qpm-package)
 (contract-out
  [current-packages-dir     (parameter/c path?)]
  [list-packages            (-> (listof qpm-package?))]
  [package-installed?       (-> string? boolean?)]
  [install-package-from-dir (-> path-string? (or/c qpm-package? string?))]
  [install-package-from-git (-> string? (or/c qpm-package? string?))]
  [remove-package           (-> string? boolean?)]
  [package-info             (-> string? (or/c qpm-package? #f))]))

;; ============================================================
;; Struct
;; ============================================================

(struct qpm-package (manifest install-dir status) #:transparent)
;; manifest    : qpm-manifest?
;; install-dir : path?   — where package is installed
;; status      : (or/c 'installed 'disabled 'error)

;; ============================================================
;; Parameter
;; ============================================================

(define current-packages-dir
  (make-parameter (build-path (find-system-path 'home-dir) ".q" "packages")))

;; ============================================================
;; Internal helpers
;; ============================================================

;; Read a qpm-package from an installed directory, or #f
(define (read-installed-package dir)
  (define qpm-path (build-path dir "qpm.json"))
  (define m (read-qpm-manifest qpm-path))
  (and m (qpm-package m dir 'installed)))

;; Ensure the packages directory exists
(define (ensure-packages-dir!)
  (define d (current-packages-dir))
  (unless (directory-exists? d)
    (make-directory* d)))

;; Get the parent directory of a file path
(define (parent-dir file-path)
  (let-values ([(base name must-be-dir?) (split-path file-path)])
    base))

;; ============================================================
;; list-packages : -> (listof qpm-package?)
;; ============================================================

(define (list-packages)
  (ensure-packages-dir!)
  (define base (current-packages-dir))
  (define dirs
    (filter directory-exists?
            (for/list ([p (in-list (directory-list base))])
              (build-path base p))))
  (define pkgs (filter-map read-installed-package dirs))
  ;; Sort by package name
  (sort pkgs string<? #:key (λ (p) (qpm-manifest-name (qpm-package-manifest p)))))

;; ============================================================
;; package-installed? : string? -> boolean?
;; ============================================================

(define (package-installed? name)
  (define dir (build-path (current-packages-dir) name))
  (and (directory-exists? dir)
       (file-exists? (build-path dir "qpm.json"))))

;; ============================================================
;; install-package-from-dir : path-string? -> (or/c qpm-package? string?)
;; ============================================================

(define (install-package-from-dir source-dir)
  (let/ec return
    (define qpm-path (build-path source-dir "qpm.json"))
    ;; Check qpm.json exists
    (unless (file-exists? qpm-path)
      (return "error: no qpm.json found in source directory"))
    ;; Read manifest
    (define m (read-qpm-manifest qpm-path))
    (unless m
      (return "error: failed to parse qpm.json"))
    ;; Validate
    (define-values (valid? errors) (validate-manifest m))
    (unless valid?
      (return (string-append "error: invalid manifest — "
                             (string-join errors "; "))))
    ;; Create target directory
    (ensure-packages-dir!)
    (define target-dir (build-path (current-packages-dir)
                                   (qpm-manifest-name m)))
    (when (directory-exists? target-dir)
      (delete-directory/files target-dir))
    (make-directory* target-dir)
    ;; Copy listed files
    (for ([rel (in-list (qpm-manifest-files m))])
      (define src-file (build-path source-dir rel))
      (define dst-file (build-path target-dir rel))
      (when (file-exists? src-file)
        ;; Ensure parent directory exists
        (define dst-parent (parent-dir dst-file))
        (unless (directory-exists? dst-parent)
          (make-directory* dst-parent))
        (copy-file src-file dst-file #t)))
    ;; Copy qpm.json itself
    (copy-file qpm-path (build-path target-dir "qpm.json") #t)
    ;; Return package
    (qpm-package m target-dir 'installed)))

;; ============================================================
;; remove-package : string? -> boolean?
;; ============================================================

(define (remove-package name)
  (define dir (build-path (current-packages-dir) name))
  (if (directory-exists? dir)
      (begin (delete-directory/files dir) #t)
      #f))

;; ============================================================
;; package-info : string? -> (or/c qpm-package? #f)
;; ============================================================

(define (package-info name)
  (define dir (build-path (current-packages-dir) name))
  (if (directory-exists? dir)
      (read-installed-package dir)
      #f))

;; ============================================================
;; #1191: Git-based package installation
;; ============================================================

;; Install a package from a git repository.
;; repo-spec : string? — "git:repo-url" or "git:repo-url@tag"
;; Returns (or/c qpm-package? string?) — package or error message.
(define (install-package-from-git repo-spec)
  (let/ec return
    (define git-prefix "git:")
    (unless (string-prefix? repo-spec git-prefix)
      (return "error: repo spec must start with 'git:'"))
    (define rest (substring repo-spec (string-length git-prefix)))
    ;; Split repo-url and optional @tag
    (define parts (regexp-match #rx"^(.+?)(?:@(.+))?$" rest))
    (unless parts
      (return "error: invalid git repo spec"))
    (define repo-url (cadr parts))
    (define tag (caddr parts))
    ;; Clone to temp dir
    (define tmp-dir (make-temporary-file "q-pkg-clone-~a" 'directory))
    (with-handlers ([exn:fail? (lambda (e)
                                 (with-handlers ([exn:fail? void])
                                   (delete-directory/files tmp-dir))
                                 (return (format "error: git clone failed — ~a" (exn-message e))))])
      (define clone-args (if tag
                            (list "git" "clone" "--branch" tag "--depth" "1" repo-url (path->string tmp-dir))
                            (list "git" "clone" "--depth" "1" repo-url (path->string tmp-dir))))
      (define-values (sp out in err)
        (apply subprocess #f #f #f #f clone-args))
      (subprocess-wait sp)
      (close-output-port in)
      (close-input-port out)
      (close-input-port err)
      (unless (= (subprocess-status sp) 0)
        (delete-directory/files tmp-dir #:must-exist? #f)
        (return (format "error: git clone exited with status ~a" (subprocess-status sp)))))
    ;; Delegate to dir-based install
    (begin0
      (install-package-from-dir tmp-dir)
      (with-handlers ([exn:fail? void])
        (delete-directory/files tmp-dir)))))
