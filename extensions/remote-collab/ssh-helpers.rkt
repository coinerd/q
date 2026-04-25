#lang racket/base

;; extensions/remote-collab/ssh-helpers.rkt — SSH command execution
;;
;; Provides SSH-based remote command execution via subprocess.
;; Includes host validation to prevent injection via malformed host strings.

(require racket/contract
         racket/string
         racket/port
         racket/match)

(provide ssh-execute
         ssh-execute-with-output
         default-ssh-options
         ssh-strict-mode
         valid-ssh-host?)

(define ssh-strict-mode (make-parameter 'accept-new))

;; Validate SSH host string to prevent injection
;; Accepts: user@host, [user@]hostname, [user@]ip-address
;; Rejects: shell metacharacters, spaces, quotes, backticks, etc.
(define (valid-ssh-host? host)
  (and (string? host)
       (non-empty-string? host)
       ;; Must match: optional user@ + hostname/ip (alphanumeric, dots, hyphens, brackets for IPv6)
       (regexp-match? #rx"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9.:\\-]+$|^[a-zA-Z0-9.:\\-]+$" host)
       ;; Must NOT contain shell metacharacters
       (not (regexp-match? #rx"[;&|`$(){}\\[\\]!'\"\\\\#]" host))))

;; Default SSH options for non-interactive execution
;; Uses 'accept-new' by default (first connection auto-accepts, verifies on subsequent).
;; Override with (ssh-strict-mode 'no) only if you understand the MITM risk.
(define (default-ssh-options)
  (list*
   "-o"
   (case (ssh-strict-mode)
     [(no) "StrictHostKeyChecking=no"]
     [(accept-new) "StrictHostKeyChecking=accept-new"]
     [(yes) "StrictHostKeyChecking=yes"])
   '("-o" "UserKnownHostsFile ~/.ssh/known_hosts" "-o" "ConnectTimeout=10" "-o" "BatchMode=yes")))

;; Execute a command on a remote host via SSH.
;; Returns (values exit-code stdout-string stderr-string)
(define (ssh-execute host command #:options [opts (default-ssh-options)])
  (unless (valid-ssh-host? host)
    (error 'ssh-execute "Invalid SSH host: ~a" host))
  (define ssh-path (find-executable-path "ssh"))
  (define all-args (append opts (list host command)))
  (define-values (sp out-in in-out err-in) (apply subprocess #f #f #f ssh-path all-args))
  (subprocess-wait sp)
  (define stdout-str (port->string out-in))
  (define stderr-str (port->string err-in))
  (close-input-port out-in)
  (close-output-port in-out)
  (close-input-port err-in)
  (values (subprocess-status sp) stdout-str stderr-str))

;; Execute with combined output as a single string
(define (ssh-execute-with-output host command #:options [opts (default-ssh-options)])
  (define-values (ec out err) (ssh-execute host command #:options opts))
  (values ec
          (string-trim (string-append out
                                      (if (non-empty-string? err)
                                          (string-append "\n" err)
                                          "")))))
