#lang racket

;; browser/adapters/playwright-sidecar.rkt — Playwright sidecar adapter
;;
;; Manages a Node.js/Playwright sidecar process communicating
;; via JSONL over stdin/stdout. Implements the browser-adapter interface.
;;
;; Lifecycle:
;;   1. launch-sidecar! starts the Node.js process
;;   2. JSONL requests sent via stdin, responses read from stdout
;;   3. shutdown-sidecar! kills the process (with timeout)

(require racket/match
         racket/async-channel
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/random crypto-random-bytes)
         json
         "../adapter.rkt"
         "../types.rkt"
         "../../util/error/errors.rkt")

(provide playwright-sidecar-state?
         playwright-sidecar-state
         playwright-sidecar-state-process
         launch-sidecar!
         shutdown-sidecar!
         send-command!
         uuid-string
         make-playwright-adapter)

;; ---------------------------------------------------------------------------
;; Sidecar state struct
;; ---------------------------------------------------------------------------

(struct playwright-sidecar-state
        (process ; subprocess?
         stdin-port ; output-port (write to sidecar stdin)
         stdout-port ; input-port (read from sidecar stdout)
         pending-box ; (hash/c string? async-channel?) — pending responses
         [reader-thread #:mutable] ; thread?
         custodian ; custodian?
         [config #:mutable] ; sidecar config
         [heartbeat-thread #:mutable]) ; (or/c thread? #f) — F11 heartbeat
  #:transparent)

;; ---------------------------------------------------------------------------
;; Simple UUID generator (no external dependency)
;; ---------------------------------------------------------------------------

(define (uuid-string)
  (define bs (bytes->hex-string (crypto-random-bytes 16)))
  (string-append (substring bs 0 8)
                 "-"
                 (substring bs 8 12)
                 "-"
                 "4"
                 (substring bs 13 16)
                 "-"
                 (substring bs 16 20)
                 "-"
                 (substring bs 20 32)))

;; ---------------------------------------------------------------------------
;; Sidecar launch/shutdown
;; ---------------------------------------------------------------------------

(define (launch-sidecar! sidecar-path #:node-path [node-path "node"] #:timeout-ms [timeout-ms 10000])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (sp stdout-in stdin-out)
      (subprocess stdout-in stdin-out #f node-path sidecar-path))
    (define pending (make-hash))
    (define ready-ch (make-async-channel))
    (define state
      (playwright-sidecar-state sp
                                stdin-out
                                stdout-in
                                pending
                                #f
                                cust
                                (hasheq 'timeout-ms timeout-ms)
                                #f)) ; heartbeat-thread initially #f

    ;; Reader thread: reads JSONL lines from stdout, routes to pending channels
    (define reader
      (thread
       (lambda ()
         (define in (playwright-sidecar-state-stdout-port state))
         (let loop ()
           (define line (read-line in 'any))
           (cond
             [(eof-object? line)
              ;; stdin EOF — mark all pending as errors
              (for ([(id ch) (in-hash pending)])
                (async-channel-put ch
                                   (hasheq 'id
                                           id
                                           'success
                                           #f
                                           'error
                                           (hasheq 'code 'sidecar-crash 'message "Sidecar EOF"))))]
             [else
              (with-handlers ([exn:fail? void])
                (define resp (string->jsexpr line))
                (define id (hash-ref resp 'id #f))
                (when id
                  (define ch (hash-ref pending id #f))
                  (when ch
                    (hash-remove! pending id)
                    (async-channel-put ch resp))))
              (loop)])))))

    (set-playwright-sidecar-state-reader-thread! state reader)

    ;; F11: Start heartbeat thread
    (start-heartbeat! state)

    state))

;; ---------------------------------------------------------------------------
;; F11: Heartbeat thread — sends ping every 30s, kills custodian on failure
;; ---------------------------------------------------------------------------

(define heartbeat-interval-secs 30)

(define (start-heartbeat! state)
  (define cust (playwright-sidecar-state-custodian state))
  (define ht
    (thread (lambda ()
              (let loop ()
                (sleep heartbeat-interval-secs)
                (with-handlers ([exn:fail?
                                 (lambda (e)
                                   (log-warning "Browser sidecar heartbeat failed, killing custodian")
                                   (custodian-shutdown-all cust))])
                  (send-command! state "ping" (hasheq) #:timeout-ms 5000))
                (loop)))))
  (set-playwright-sidecar-state-heartbeat-thread! state ht))

(define (shutdown-sidecar! state #:timeout-ms [timeout-ms 5000])
  ;; F11: Kill heartbeat thread first
  (define ht (playwright-sidecar-state-heartbeat-thread state))
  (when (and ht (thread-running? ht))
    (kill-thread ht)
    (set-playwright-sidecar-state-heartbeat-thread! state #f))
  (define proc (playwright-sidecar-state-process state))
  (define cust (playwright-sidecar-state-custodian state))
  (define in (playwright-sidecar-state-stdout-port state))
  (define out (playwright-sidecar-state-stdin-port state))
  ;; Close stdin to signal graceful shutdown
  (with-handlers ([exn:fail? void])
    (close-output-port out))
  ;; Wait for process to exit (poll loop)
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (let loop ()
    (when (and (< (current-inexact-milliseconds) deadline) (eq? (subprocess-status proc) 'running))
      (sleep 0.1)
      (loop)))
  ;; Kill custodian (forcibly terminates everything)
  (custodian-shutdown-all cust))

;; ---------------------------------------------------------------------------
;; JSONL request/response
;; ---------------------------------------------------------------------------

(define (send-command! state type params #:timeout-ms [timeout-ms #f])
  (define id (uuid-string))
  (define ch (make-async-channel))
  (hash-set! (playwright-sidecar-state-pending-box state) id ch)
  (define out (playwright-sidecar-state-stdin-port state))
  (define req (hasheq 'id id 'type type 'params params))
  (write-string (jsexpr->string req) out)
  (newline out)
  (flush-output out)
  (define tmout (or timeout-ms (hash-ref (playwright-sidecar-state-config state) 'timeout-ms 10000)))
  (define result (sync/timeout (/ tmout 1000.0) ch))
  (hash-remove! (playwright-sidecar-state-pending-box state) id)
  (cond
    [(not result)
     (raise-browser-error 'timeout (format "Sidecar command '~a' timed out after ~ams" type tmout))]
    [(not (hash-ref result 'success #f))
     (define err (hash-ref result 'error (hasheq 'code 'unknown 'message "Unknown error")))
     (define code (hash-ref err 'code 'adapter-error))
     (raise-browser-error (match code
                            ["timeout" 'timeout]
                            ["sidecar-crash" 'sidecar-crash]
                            ["selector-not-found" 'adapter-error]
                            ["session-not-found" 'session-expired]
                            ["policy-violation" 'policy-violation]
                            [_ 'adapter-error])
                          (hash-ref err 'message "Adapter error"))]
    [else (hash-ref result 'data)]))

;; ---------------------------------------------------------------------------
;; Adapter interface
;; ---------------------------------------------------------------------------

(define (make-playwright-adapter sidecar-path
                                 #:node-path [node-path "node"]
                                 #:timeout-ms [timeout-ms 10000])
  (define state-box (box #f))

  (define (ensure-state)
    (or (unbox state-box)
        (let ([s (launch-sidecar! sidecar-path #:node-path node-path #:timeout-ms timeout-ms)])
          (set-box! state-box s)
          s)))

  ;; Helper: map action to sidecar command
  (define (action->command action)
    (match action
      [(browser-action-click selector button)
       (list "click" (hasheq 'selector selector 'button button))]
      [(browser-action-type selector text clear-first?)
       (list "type" (hasheq 'selector selector 'text text 'clearFirst clear-first?))]
      [(browser-action-press key modifiers) (list "press" (hasheq 'key key 'modifiers modifiers))]
      [(browser-action-scroll direction amount)
       (list "scroll" (hasheq 'direction direction 'amount amount))]
      [(browser-action-navigate url wait-until?)
       (list "navigate" (hasheq 'url url 'waitUntil wait-until?))]
      [(browser-action-extract selector extract-type)
       (list "extract" (hasheq 'selector selector 'extractType extract-type))]
      [(browser-action-screenshot selector full-page?)
       (list "screenshot" (hasheq 'selector selector 'fullPage full-page?))]
      ;; wait is just observe with a selector
      [(browser-action-wait selector timeout-ms) (list "observe" (hasheq 'selector selector))]
      [_ (error 'playwright-adapter "Unknown action: ~a" action)]))

  ;; Helper to build observation from sidecar response
  (define (data->obs data)
    (browser-observation (hash-ref data 'url "")
                         (hash-ref data 'title "")
                         (hash-ref data 'textContent "")
                         (hash-ref data 'visibleText "")
                         (hash-ref data 'domSummary #f)
                         (hash-ref data 'accessibilityTree #f)
                         #f ; screenshot-mime
                         #f ; screenshot-bytes
                         (hash-ref data 'consoleErrors '())
                         '() ; network-requests
                         (hash-ref data 'viewportSize (hasheq 'width 1280 'height 720))
                         #f)) ; metadata

  ;; Adapter callback implementations
  (define (adapter-open session-id target)
    (define st (ensure-state))
    (define data (send-command! st "navigate" (hasheq 'url target 'sessionId session-id)))
    (data->obs data))

  (define (adapter-close session-id)
    (define st (ensure-state))
    (send-command! st "close" (hasheq 'sessionId session-id))
    (void))

  (define (adapter-navigate session-id url)
    (define st (ensure-state))
    (define data (send-command! st "navigate" (hasheq 'url url 'sessionId session-id)))
    (data->obs data))

  (define (adapter-observe session-id selector)
    (define st (ensure-state))
    (define params (hasheq 'sessionId session-id 'selector (or selector #f)))
    (define data (send-command! st "extract" params))
    (data->obs data))

  (define (adapter-act session-id action)
    (define st (ensure-state))
    (define-values (cmd-type params) (apply values (action->command action)))
    (define data (send-command! st cmd-type (hash-set params 'sessionId session-id)))
    (data->obs data))

  (define (adapter-screenshot session-id selector full-page?)
    (define st (ensure-state))
    (define data
      (send-command! st
                     "screenshot"
                     (hasheq 'sessionId session-id 'selector (or selector #f) 'fullPage full-page?)))
    (browser-observation ""
                         ""
                         ""
                         ""
                         #f
                         #f
                         (hash-ref data 'mimeType "image/png")
                         (hash-ref data 'data "")
                         '()
                         '()
                         #f
                         #f))

  (make-browser-adapter #:open adapter-open
                        #:close adapter-close
                        #:navigate adapter-navigate
                        #:observe adapter-observe
                        #:act adapter-act
                        #:screenshot adapter-screenshot))
