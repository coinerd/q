#lang racket/base

;; scripts/ci/observed-environment.rkt — CL5 production CI host observation.
;;
;; Records the host that is ACTUALLY executing the production fast-suite proof
;; bundle producer. The values are read from the running process, its
;; environment, and the checked-out tree; no configured OS or runner image is
;; substituted for an observation. This is observational only: the claim's
;; existing environment fields are deliberately NOT changed, so proof-bundle
;; reuse and required checks keep their current behavior.
;;
;; Usage:
;;   racket scripts/ci/observed-environment.rkt --out PATH
;;
;; Required environment variables (fail closed when absent):
;;   ImageOS, ImageVersion, RUNNER_OS, RUNNER_ARCH, RUNNER_NAME,
;;   GITHUB_SHA, GITHUB_WORKFLOW_SHA, CL5_OBSERVED_RUNS_ON_LABEL,
;;   CL5_OBSERVED_WORKFLOW_BLOB_SHA
;;
;; The tree SHA is derived from the checked-out HEAD with `git rev-parse
;; HEAD^{tree}`; the workflow SHA comes from `github.workflow_sha`.

(require json
         racket/cmdline
         racket/file
         racket/future
         racket/string
         racket/system
         "../../util/json/checksum.rkt")

(provide observed-environment-schema
         observed-environment
         parse-os-release
         find-racket-executable
         git-tree-sha)

(define observed-environment-schema "q.observed-ci-environment/1")
(define sha-rx #px"^[0-9a-f]{40}$")
(define cpu-model-rx #px"(?m:^model name\\s*:\\s*(.+)$)")
(define mem-total-rx #px"(?m:^MemTotal:\\s*([0-9]+)\\s*kB)")

(define (env-ref name)
  (define raw (environment-variables-ref (current-environment-variables) (string->bytes/utf-8 name)))
  (define value (and raw (string-trim (bytes->string/utf-8 raw))))
  (unless (and value (not (string=? value "")))
    (error 'observed-environment "missing required environment value: ~a" name))
  value)

(define (required-string value label)
  (unless (and (string? value) (not (string=? (string-trim value) "")))
    (error 'observed-environment "empty required observation: ~a" label))
  value)

(define (required-sha value label)
  (unless (and (string? value) (regexp-match? sha-rx value))
    (error 'observed-environment "malformed 40-hex SHA for ~a: ~s" label value))
  value)

(define (strip-quotes s)
  (define n (string-length s))
  (cond
    [(and (>= n 2) (char=? (string-ref s 0) #\") (char=? (string-ref s (sub1 n)) #\"))
     (substring s 1 (sub1 n))]
    [(and (>= n 2) (char=? (string-ref s 0) #\') (char=? (string-ref s (sub1 n)) #\'))
     (substring s 1 (sub1 n))]
    [else s]))

(define (parse-os-release text)
  (for/fold ([acc (hash)]) ([line (in-list (string-split text "\n"))])
    (define m (regexp-match #px"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$" line))
    (if m
        (hash-set acc (cadr m) (strip-quotes (string-trim (caddr m))))
        acc)))

(define (hash-required h key label)
  (define value (hash-ref h key #f))
  (unless (and (string? value) (not (string=? (string-trim value) "")))
    (error 'observed-environment "missing ~a in os-release" label))
  value)

(define (read-observation-file root relative label)
  (define path (build-path root relative))
  (unless (file-exists? path)
    (error 'observed-environment "missing observation source ~a: ~a" label path))
  (file->string path))

(define (parse-cpu-model text)
  (define m (regexp-match cpu-model-rx text))
  (unless m
    (error 'observed-environment "no model name in cpuinfo"))
  (string-trim (cadr m)))

(define (parse-memory-kb text)
  (define m (regexp-match mem-total-rx text))
  (unless m
    (error 'observed-environment "no MemTotal in meminfo"))
  (define n (string->number (cadr m)))
  (unless (and (exact-integer? n) (positive? n))
    (error 'observed-environment "malformed MemTotal: ~s" (cadr m)))
  n)

(define (resolve-racket-executable candidate)
  (define path (or candidate (find-executable-path "racket")))
  (unless (and path (file-exists? path))
    (error 'observed-environment "racket executable not found: ~s" path))
  (with-handlers ([exn:fail? (lambda (_) (path->complete-path path))])
    (resolve-path path)))

(define (available-cores)
  (define n (processor-count))
  (unless (and (exact-integer? n) (positive? n))
    (error 'observed-environment "invalid processor-count: ~s" n))
  n)

(define (observed-environment #:root [root "/"]
                              #:tree-sha tree-sha
                              #:racket-executable [racket-executable #f])
  ;; Every value below is either read from the running environment or derived
  ;; from files in `root`. No value is taken from a workflow assertion.
  (define os-text (read-observation-file root "etc/os-release" "os-release"))
  (define os (parse-os-release os-text))
  (define kernel-release
    (string-trim (read-observation-file root "proc/sys/kernel/osrelease" "kernel release")))
  (define kernel-version (string-trim (read-observation-file root "proc/version" "kernel version")))
  (define cpu-text (read-observation-file root "proc/cpuinfo" "cpuinfo"))
  (define memory-text (read-observation-file root "proc/meminfo" "meminfo"))
  (define exe (resolve-racket-executable racket-executable))
  (define cores (available-cores))
  (hasheq 'schema
          observed-environment-schema
          'os_release
          (hasheq 'id
                  (hash-required os "ID" "ID")
                  'version_id
                  (hash-required os "VERSION_ID" "VERSION_ID")
                  'pretty_name
                  (hash-required os "PRETTY_NAME" "PRETTY_NAME"))
          'kernel
          (hasheq 'release
                  (required-string kernel-release "kernel release")
                  'version
                  (required-string kernel-version "kernel version"))
          'runner
          (hasheq 'declared_runs_on_label
                  (env-ref "CL5_OBSERVED_RUNS_ON_LABEL")
                  'image_os
                  (env-ref "ImageOS")
                  'image_version
                  (env-ref "ImageVersion")
                  'runner_os
                  (env-ref "RUNNER_OS")
                  'runner_arch
                  (env-ref "RUNNER_ARCH")
                  'runner_name
                  (env-ref "RUNNER_NAME"))
          'cpu
          (hasheq 'architecture
                  (symbol->string (system-type 'arch))
                  'model
                  (parse-cpu-model cpu-text)
                  'available_cores
                  cores)
          'memory
          (hasheq 'total_kb (parse-memory-kb memory-text))
          'racket
          (hasheq 'version
                  (version)
                  'executable_path
                  (path->string exe)
                  'executable_sha256
                  (string-append "sha256:" (sha256-file exe)))
          'revision
          (hasheq 'commit_sha
                  (required-sha (env-ref "GITHUB_SHA") "GITHUB_SHA")
                  'tree_sha
                  (required-sha tree-sha "HEAD tree")
                  'workflow_sha
                  (required-sha (env-ref "GITHUB_WORKFLOW_SHA") "GITHUB_WORKFLOW_SHA")
                  'workflow_blob_sha
                  (required-sha (env-ref "CL5_OBSERVED_WORKFLOW_BLOB_SHA")
                                "CL5_OBSERVED_WORKFLOW_BLOB_SHA"))))

(define (find-racket-executable)
  (resolve-racket-executable #f))

(define (git-tree-sha)
  (define git
    (or (find-executable-path "git") (error 'observed-environment "git executable not found")))
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out])
      (system*/exit-code git "rev-parse" "HEAD^{tree}")))
  (unless (zero? code)
    (error 'observed-environment "git rev-parse HEAD^{tree} failed with ~a" code))
  (required-sha (string-trim (get-output-string out)) "HEAD tree"))

(module+ main
  (define out-path #f)
  (command-line #:program "observed-environment.rkt"
                #:once-each
                [("--out") path "Write the observed environment JSON to PATH" (set! out-path path)])
  (unless out-path
    (error 'observed-environment "--out is required"))
  (define observation
    (observed-environment #:tree-sha (git-tree-sha) #:racket-executable (find-racket-executable)))
  (call-with-output-file out-path
                         (lambda (out)
                           (write-json observation out)
                           (newline out))
                         #:exists 'replace))
