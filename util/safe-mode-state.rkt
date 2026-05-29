#lang racket/base

;; util/safe-mode-state.rkt — safe-mode parameters, struct, and pure predicates
;;
;; ARCH-02 (v0.22.0): Extracted from runtime/safe-mode.rkt to eliminate
;; the upward import in util/safe-mode-predicates.rkt.  This is a util/
;; leaf — it has NO upward imports (only racket/base and os-environment).
;;
;; Both runtime/safe-mode.rkt and util/safe-mode-predicates.rkt import
;; from here.

(require racket/string
         racket/contract)

;; ============================================================
;; Parameters
;; ============================================================

(define current-safe-mode (make-parameter #f))
(define current-safe-mode-config (make-parameter #f))
(define project-root (make-parameter (current-directory)))

;; ============================================================
;; Per-session config struct
;; ============================================================

(struct safe-mode-config (active allowed-tools allowed-paths locked project-root-path)
  #:transparent)

(define (make-safe-mode-config #:active [active #t]
                               #:allowed-tools [allowed-tools #f]
                               #:allowed-paths [allowed-paths #f]
                               #:locked [locked #f]
                               #:project-root [proj-root (current-directory)])
  (safe-mode-config active (or allowed-tools 'default) (or allowed-paths 'default) locked proj-root))

;; ============================================================
;; Constants
;; ============================================================

(define blocked-tools '("bash" "edit" "write" "firecrawl"))

;; ============================================================
;; Predicates
;; ============================================================

(define (safe-mode?)
  (define cfg (current-safe-mode-config))
  (cond
    [(safe-mode-config? cfg) (safe-mode-config-active cfg)]
    [else (or (current-safe-mode)
              (let ([v (getenv "Q_SAFE_MODE")])
                (and v (string=? v "1"))))]))

(define (allowed-tool? tool-name)
  (define cfg (current-safe-mode-config))
  (cond
    [(and (safe-mode-config? cfg)
          (not (eq? (safe-mode-config-allowed-tools cfg) 'default)))
     (define allowed (safe-mode-config-allowed-tools cfg))
     (and (not (member tool-name blocked-tools string=?))
          (not (string-prefix? tool-name "extension:"))
          (or (not (list? allowed)) (member tool-name allowed string=?)))]
    [(safe-mode?)
     (and (not (member tool-name blocked-tools string=?))
          (not (string-prefix? tool-name "extension:")))]
    [else #t]))

(define (allowed-path? path)
  (cond
    [(not (safe-mode?)) #t]
    [else
     (define cfg (current-safe-mode-config))
     (define root-path
       (cond
         [(and (safe-mode-config? cfg) (safe-mode-config-project-root-path cfg))
          (safe-mode-config-project-root-path cfg)]
         [else (project-root)]))
     (define allowed-paths (and (safe-mode-config? cfg)
                                (safe-mode-config-allowed-paths cfg)))
     (cond
       [(and (list? allowed-paths) (not (eq? allowed-paths 'default)))
        (for/or ([ap (in-list allowed-paths)])
          (define resolved-path
            (with-handlers ([exn:fail? (λ (_) #f)])
              (resolve-path path)))
          (define resolved-ap
            (with-handlers ([exn:fail? (λ (_) #f)])
              (resolve-path ap)))
          (and resolved-path
               resolved-ap
               (let* ([path-str (path->string
                                 (if (complete-path? resolved-path)
                                     resolved-path
                                     (path->complete-path resolved-path)))]
                      [ap-str (path->string
                               (if (complete-path? resolved-ap)
                                   resolved-ap
                                   (path->complete-path resolved-ap)))]
                      [ap-prefix (if (string-suffix? ap-str "/")
                                     ap-str
                                     (string-append ap-str "/"))])
                 (or (string=? path-str ap-str)
                     (string-prefix? path-str ap-prefix)))))]
       [else
        (and (with-handlers ([exn:fail? (λ (_) #f)])
               (resolve-path root-path))
             (with-handlers ([exn:fail? (λ (_) #f)])
               (resolve-path path))
             (let* ([resolved-root (resolve-path root-path)]
                    [resolved-path (resolve-path path)]
                    [root-str (path->string
                               (if (complete-path? resolved-root)
                                   resolved-root
                                   (path->complete-path resolved-root)))]
                    [path-str (path->string
                               (if (complete-path? resolved-path)
                                   resolved-path
                                   (path->complete-path resolved-path)))]
                    [root-prefix (if (string-suffix? root-str "/")
                                     root-str
                                     (string-append root-str "/"))])
               (or (string=? path-str root-str)
                   (string-prefix? path-str root-prefix))))])]))

;; ============================================================
;; Introspection helpers
;; ============================================================

(define (safe-mode-project-root)
  (define cfg (current-safe-mode-config))
  (define root-path
    (cond
      [(and (safe-mode-config? cfg) (safe-mode-config-project-root-path cfg))
       (safe-mode-config-project-root-path cfg)]
      [else (project-root)]))
  (path->string root-path))

(define (trust-level)
  (if (safe-mode?) 'restricted 'full))

;; ============================================================
;; Provide
;; ============================================================

(provide
 ;; Parameters
 current-safe-mode
 current-safe-mode-config
 project-root

 ;; Struct type + predicate
 safe-mode-config
 safe-mode-config?

 ;; Struct accessors (contracted)
 (contract-out
  [make-safe-mode-config (->* ()
                               (#:active boolean?
                                #:allowed-tools (or/c #f (listof string?))
                                #:allowed-paths (or/c #f (listof (or/c path? string?)))
                                #:locked boolean?
                                #:project-root (or/c path? string?))
                               safe-mode-config?)]
  [safe-mode-config-active (-> safe-mode-config? boolean?)]
  [safe-mode-config-allowed-tools (-> safe-mode-config? any/c)]
  [safe-mode-config-allowed-paths (-> safe-mode-config? any/c)]
  [safe-mode-config-locked (-> safe-mode-config? boolean?)]
  [safe-mode-config-project-root-path (-> safe-mode-config? (or/c path? #f))])

 ;; Constants
 blocked-tools

 ;; Predicates
 safe-mode?
 allowed-tool?
 allowed-path?

 ;; Introspection
 safe-mode-project-root
 trust-level)
