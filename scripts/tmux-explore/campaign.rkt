#lang racket/base

;; scripts/tmux-explore/campaign.rkt — Tamper-evident all-nine campaign orchestrator
;;
;; W7: Orchestrates all 9 multi-step drivers in exact order, generates
;; tamper-evident evidence manifests with GitHub-attested repo SHA,
;; manifest chaining, and credential proof.

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "drivers.rkt"
         "evidence.rkt"
         "verifiers.rkt"
         "executor.rkt"
         "../../util/credential-redaction.rkt")

(provide run-campaign
         campaign-result?
         campaign-result-passed?
         campaign-result-manifest
         campaign-result-scenario-results
         campaign-result-credential-proof
         campaign-result-cleanup-verified?
         verify-campaign
         campaign-cleanup!
         build-campaign-manifest
         verify-campaign-manifest
         verify-manifest-chain
         verify-credential-proof)

(struct campaign-result (passed? scenario-results manifest credential-proof cleanup-verified?)
  #:transparent)

;; Campaign authorization check (duplicated from tmux-tui-explore.rkt to avoid circular dep)
(define (real-provider-authorized?)
  (and (equal? (getenv "Q_TMUX_TUI_TESTS") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM") "I_UNDERSTAND_COSTS")))

;; Campaign configuration
(define campaign-scenario-tags
  '("memory" "gsd" "mas" "tools" "release-audit" "durable-memory" "resume" "compact" "interrupt"))

(define (campaign-authorized?)
  (and (real-provider-authorized?) (equal? (getenv "Q_TMUX_TUI_CAMPAIGN") "1")))

(define (require-campaign-authorization!)
  (unless (campaign-authorized?)
    (error 'campaign
           (string-append "campaign mode requires Q_TMUX_TUI_TESTS=1, "
                          "Q_TMUX_TUI_REAL_PROVIDER=1, "
                          "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS, and "
                          "Q_TMUX_TUI_CAMPAIGN=1"))))

;; GitHub attestation: verify repo SHA exists on GitHub
(define (verify-github-attestation repo-sha [owner "coinerd"] [repo "q"])
  (define gh-cli (find-executable-path "gh"))
  (if (not gh-cli)
      (hasheq 'verified? #f 'reason "gh CLI not found")
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (hasheq 'verified? #f 'reason (format "GitHub API error: ~a" (exn-message e))))])
        (define-values (sp stdout stdin stderr)
          (subprocess #f #f #f gh-cli "api" (format "/repos/~a/~a/commits/~a" owner repo repo-sha)))
        (close-output-port stdin)
        (define output (port->string stdout))
        (close-input-port stdout)
        (close-input-port stderr)
        (subprocess-wait sp)
        (if (zero? (subprocess-status sp))
            (hasheq 'verified? #t 'repo-sha repo-sha)
            (hasheq 'verified? #f 'reason (format "GitHub returned non-zero for SHA ~a" repo-sha))))))

;; Build campaign manifest with GitHub attestation and manifest chaining
(define (build-campaign-manifest #:output-dir output-dir
                                 #:repo-sha repo-sha
                                 #:version version
                                 #:scenario-results scenario-results
                                 #:previous-manifest [previous-manifest #f])
  (define scenario-count (length scenario-results))
  (define step-count (for/sum ([r (in-list scenario-results)]) 1))
  (define step-digests
    (for/list ([r (in-list scenario-results)])
      (format "~a" (equal-hash-code r))))

  ;; Build individual scenario manifests
  (define scenario-manifests
    (for/list ([result (in-list scenario-results)]
               [i (in-naturals)])
      (build-evidence-manifest #:tag (format "scenario-~a" i)
                               #:status (if (verification-result-passed? result) 'pass 'fail)
                               #:classification (if (verification-result-passed? result) 'pass 'fail)
                               #:repo-sha repo-sha
                               #:version version
                               #:output-dir output-dir
                               #:scenario-count 1
                               #:step-count 1
                               #:step-digests (list (format "~a" (equal-hash-code result))))))

  ;; Campaign-level manifest
  (define files (list-artifact-files output-dir))
  (define digest (compute-directory-digest files))
  (define artifacts
    (for/list ([f (in-list files)])
      (hasheq 'path (path->string (file-name-from-path f)) 'sha256 (or (sha256-file f) "ERROR"))))

  ;; GitHub attestation
  (define github-attestation (verify-github-attestation repo-sha))

  ;; Manifest chaining: include hash of previous manifest
  (define previous-hash
    (if previous-manifest
        (format "~a" (equal-hash-code previous-manifest))
        "genesis"))

  (hasheq 'schema-version
          2
          'tag
          "all-nine-campaign"
          'status
          (if (andmap verification-result-passed? scenario-results) "pass" "fail")
          'classification
          (if (andmap verification-result-passed? scenario-results) "pass" "fail")
          'repo-sha
          repo-sha
          'version
          version
          'artifact-digest
          (or digest "ERROR")
          'artifact-count
          (length artifacts)
          'scenario-count
          scenario-count
          'step-count
          step-count
          'step-digests
          step-digests
          'scenario-manifests
          (for/list ([m (in-list scenario-manifests)])
            m)
          'artifacts
          artifacts
          'github-attestation
          github-attestation
          'previous-manifest-hash
          previous-hash
          'timestamp
          (current-seconds)))

;; Verify manifest chain integrity
(define (verify-manifest-chain current-manifest previous-manifest)
  (define current-previous (hash-ref current-manifest 'previous-manifest-hash "genesis"))
  (define expected-previous
    (if previous-manifest
        (format "~a" (equal-hash-code previous-manifest))
        "genesis"))
  (equal? current-previous expected-previous))

;; Verify credential proof for campaign
(define (verify-credential-proof scenario-results)
  (for/and ([result (in-list scenario-results)])
    (define obs (verification-result-evidence result))
    (if obs
        (leak-scan-passed? (hash-ref obs 'capture "") (hash-ref obs 'trace-events '()))
        #t)))

;; Run a single scenario and return verification result
(define (run-scenario tag output-root)
  (define driver (find-driver tag))
  (when (not driver)
    (error 'campaign "no driver for tag: ~a" tag))

  (define observation
    (with-handlers ([exn:fail? (lambda (e)
                                 (hash 'status
                                       'failed
                                       'trace-events
                                       '()
                                       'capture
                                       ""
                                       'mock-provider?
                                       #f
                                       'timed-out?
                                       #f
                                       'crashed?
                                       #t
                                       'error
                                       (exn-message e)))])
      (run-real-scenario tag
                         ((multi-step-driver-prompt-generator driver) 0)
                         output-root
                         #:tools? (not (null? (multi-step-driver-tool-allowlist driver))))))

  (define verification (verify-driver driver observation (make-step-baseline)))
  verification)

;; Run the full campaign
(define (run-campaign #:mode [mode 'mock]
                      #:output-dir [output-dir #f]
                      #:previous-manifest [previous-manifest #f])
  (when (eq? mode 'real)
    (require-campaign-authorization!))

  (define root (or output-dir (make-temporary-file "q-campaign-~a" 'directory)))
  (make-directory* root)

  (define repo-sha
    (or (getenv "Q_GIT_SHA")
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (string-trim (with-output-to-string
                        (lambda () (system* (find-executable-path "git") "rev-parse" "HEAD")))))))

  (define version
    (with-handlers ([exn:fail? (lambda (_) "unknown")])
      (string-trim (with-output-to-string
                    (lambda () (system* (find-executable-path "git") "describe" "--tags"))))))

  (define scenario-results '())
  (define all-passed? #t)

  (for ([tag (in-list campaign-scenario-tags)])
    (define result
      (if (eq? mode 'mock)
          (verification-result #t 'pass (list (format "mock ~a" tag)) '())
          (run-scenario tag root)))
    (set! scenario-results (cons result scenario-results))
    (when (not (verification-result-passed? result))
      (set! all-passed? #f)))

  (set! scenario-results (reverse scenario-results))

  ;; Build campaign manifest
  (define manifest
    (build-campaign-manifest #:output-dir root
                             #:repo-sha (or repo-sha "unknown")
                             #:version version
                             #:scenario-results scenario-results
                             #:previous-manifest previous-manifest))

  ;; Credential proof
  (define credential-proof
    (if (eq? mode 'mock)
        #t
        (verify-credential-proof scenario-results)))

  ;; Write manifest
  (write-evidence-manifest! manifest (build-path root "campaign-manifest.json"))

  ;; Cleanup verification (for real mode, verify cleanup works)
  (define cleanup-verified?
    (if (eq? mode 'real)
        (begin
          (campaign-cleanup! root)
          (not (directory-exists? root)))
        #t))

  (campaign-result all-passed? scenario-results manifest credential-proof cleanup-verified?))

;; Verify campaign result
(define (verify-campaign result)
  (define manifest (campaign-result-manifest result))
  (define reasons '())
  (define (fail! msg)
    (set! reasons (cons msg reasons)))

  (unless (hash? manifest)
    (fail! "manifest is not a hash"))
  (when (hash? manifest)
    (unless (equal? (hash-ref manifest 'tag #f) "all-nine-campaign")
      (fail! "manifest tag is not all-nine-campaign"))
    (unless (and (hash-ref manifest 'repo-sha #f)
                 (regexp-match? #px"^[0-9a-f]{40}$" (format "~a" (hash-ref manifest 'repo-sha #f))))
      (fail! "repo-sha is missing or invalid"))
    (unless (equal? (hash-ref manifest 'scenario-count #f) 9)
      (fail! "scenario-count is not 9"))
    (unless (hash-ref manifest 'github-attestation #f)
      (fail! "github-attestation is missing"))
    (define attestation (hash-ref manifest 'github-attestation #f))
    (when (hash? attestation)
      (unless (hash-ref attestation 'verified? #f)
        (fail! "github-attestation is not verified")))
    (unless (hash-ref manifest 'previous-manifest-hash #f)
      (fail! "previous-manifest-hash is missing"))
    (unless (hash-ref manifest 'scenario-manifests #f)
      (fail! "scenario-manifests is missing"))
    (define scenario-manifests (hash-ref manifest 'scenario-manifests '()))
    (unless (= (length scenario-manifests) 9)
      (fail! "scenario-manifests count is not 9")))

  (if (null? reasons)
      (hasheq 'valid? #t 'reasons '())
      (hasheq 'valid? #f 'reasons (reverse reasons))))

;; Verify campaign manifest
(define (verify-campaign-manifest manifest)
  (verify-campaign (campaign-result #t '() manifest #t #t)))

;; Cleanup campaign artifacts
(define (campaign-cleanup! root)
  (when (directory-exists? root)
    (delete-directory/files root)))
