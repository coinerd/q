#lang racket/base

;; util/config-reader.rkt — reads and merges config (with deliberate bugs)

(require racket/file
         racket/port
         racket/string
         racket/match
         json)

(provide load-config
         merge-configs
         get-provider-config
         resolve-model
         config->model-registry)

;; Works correctly
(define (load-config path)
  (if (file-exists? path)
      (with-handlers ([exn:fail? (lambda (e) (hasheq))])
        (call-with-input-file path read-json))
      (hasheq)))

;; BUG: when both have the same key, project should override global,
;; but this reverses the merge order (project loses to global)
(define (merge-configs global-cfg project-cfg)
  (for/fold ([acc project-cfg])
            ([(k v) (in-hash global-cfg)])    ;; BUG: global iterates over project
    (hash-set acc k v)))

;; Works correctly
(define (get-provider-config cfg provider-name)
  (define providers (hash-ref cfg 'providers (hasheq)))
  (hash-ref providers provider-name #f))

;; BUG: doesn't handle "provider/model" format — always returns #f for compound names
(define (resolve-model cfg model-name)
  (define parts (string-split (or model-name "") "/"))
  (if (= (length parts) 2)
      #f   ;; BUG: should resolve provider/model but returns #f
      (let* ([providers (hash-ref cfg 'providers (hasheq))]
             [default-prov (hash-ref cfg 'default-provider #f)])
        (and default-prov
             (hash-ref providers (string->symbol default-prov) #f)))))

;; Works correctly (no bug)
(define (config->model-registry cfg)
  (define providers (hash-ref cfg 'providers (hasheq)))
  (for/hash ([(name prov) (in-hash providers)])
    (values name prov)))
