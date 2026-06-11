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
         (only-in racket/base make-semaphore call-with-semaphore)
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/random crypto-random-bytes)
         json
         "../adapter.rkt"
         "../types.rkt"
         "../../util/error/errors.rkt")

(provide (struct-out playwright-sidecar-state)
         launch-sidecar!
         shutdown-sidecar!
         send-command!
         send-command-with-recovery!
         restart-sidecar!
         start-heartbeat!
         max-sidecar-restarts
         uuid-string
         make-playwright-adapter
         make-reader-body
         launch-sidecar-process!)

;; ---------------------------------------------------------------------------
;; Sidecar state struct
;; ---------------------------------------------------------------------------

(struct playwright-sidecar-state
        ([process #:mutable] ; subprocess?
         [stdin-port #:mutable] ; output-port (write to sidecar stdin)
         [stdout-port #:mutable] ; input-port (read from sidecar stdout)
         pending-box ; (hash/c string? async-channel?) — pending responses
         [reader-thread #:mutable] ; thread?
         [custodian #:mutable] ; custodian?
         [config #:mutable] ; sidecar config
         [heartbeat-thread #:mutable] ; (or/c thread? #f) — F11 heartbeat
         [dead? #:mutable]) ; #t after reader EOF (SEC-15)
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
;; Shared helpers (W2: eliminate duplication)
;; ---------------------------------------------------------------------------

;; DUP-01: Shared reader body for launch-sidecar! and restart-sidecar!
(define (make-reader-body state)
  (define pending (playwright-sidecar-state-pending-box state))
  (define cfg (playwright-sidecar-state-config state))
  (lambda ()
    (define in (playwright-sidecar-state-stdout-port state))
    (let loop ()
      (define line (read-line in 'any))
      (cond
        [(eof-object? line)
         ;; SEC-15: Mark sidecar as dead
         (set-playwright-sidecar-state-dead?! state #t)
         ;; stdin EOF — mark all pending as errors
         (define sema (hash-ref cfg 'pending-sema #f))
         (define fail-all
           (lambda ()
             (for ([(id ch) (in-hash pending)])
               (async-channel-put ch
                                  (hasheq 'id
                                          id
                                          'success
                                          #f
                                          'error
                                          (hasheq 'code 'sidecar-crash 'message "Sidecar EOF"))))))
         (if sema
             (call-with-semaphore sema fail-all)
             (fail-all))]
        [else
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning (format "Browser sidecar reader parse error: ~a"
                                                           (exn-message e))))])
           (define resp (string->jsexpr line))
           (define id (hash-ref resp 'id #f))
           (when id
             (define sema (hash-ref cfg 'pending-sema #f))
             (define deliver
               (lambda ()
                 (define ch (hash-ref pending id #f))
                 (when ch
                   (hash-remove! pending id)
                   (async-channel-put ch resp))))
             (if sema
                 (call-with-semaphore sema deliver)
                 (deliver))))
         (loop)]))))

;; DUP-02: Shared subprocess launch logic
(define (launch-sidecar-process! state sidecar-path node-path headless?)
  (define node-exe (or (find-executable-path node-path) node-path))
  (define cust (make-custodian))
  (set-playwright-sidecar-state-custodian! state cust)
  (parameterize ([current-custodian cust])
    (define-values (sp stdout-in stdin-out stderr-in)
      (subprocess #f
                  #f
                  #f
                  node-exe
                  sidecar-path
                  (format "--headless=~a" (if headless? "true" "false"))))
    ;; Drain stderr
    (thread (lambda ()
              (let loop ()
                (define line (read-line stderr-in 'any))
                (unless (eof-object? line)
                  (loop)))))
    (set-playwright-sidecar-state-process! state sp)
    (set-playwright-sidecar-state-stdin-port! state stdin-out)
    (set-playwright-sidecar-state-stdout-port! state stdout-in)
    cust))

;; ---------------------------------------------------------------------------
;; Sidecar launch/shutdown
;; ---------------------------------------------------------------------------

(define (launch-sidecar! sidecar-path
                         #:node-path [node-path "node"]
                         #:timeout-ms [timeout-ms 10000]
                         #:headless? [headless? #t])
  (define pending (make-hash))
  (define pending-sema (make-semaphore 1))
  (define state
    (playwright-sidecar-state #f ; process
                              #f ; stdin-port
                              #f ; stdout-port
                              pending
                              #f ; reader-thread
                              #f ; custodian
                              (hasheq 'timeout-ms
                                      timeout-ms
                                      'pending-sema
                                      pending-sema
                                      'restart-sema
                                      (make-semaphore 1)
                                      'sidecar-path
                                      sidecar-path
                                      'node-path
                                      node-path
                                      'headless?
                                      headless?
                                      'restart-count
                                      0)
                              #f ; heartbeat-thread
                              #f)) ; dead?
  ;; DUP-02: Launch subprocess
  (launch-sidecar-process! state sidecar-path node-path headless?)
  ;; DUP-01: Start reader thread
  (define reader (thread (make-reader-body state)))
  (set-playwright-sidecar-state-reader-thread! state reader)
  ;; F11: Start heartbeat thread
  (start-heartbeat! state)
  state)

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
                ;; SEC-05: Read current reader from state, not closure
                (define rt (playwright-sidecar-state-reader-thread state))
                (when (and rt (not (thread-running? rt)))
                  (log-warning "Browser sidecar reader thread died, killing custodian")
                  (custodian-shutdown-all cust)
                  (void))
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
  ;; C2 + SEC-08: Require pending semaphore; no unsafe fallback
  (define sema (hash-ref (playwright-sidecar-state-config state) 'pending-sema #f))
  (unless sema
    (raise-browser-error "Sidecar missing pending semaphore" 'adapter-error))
  (call-with-semaphore sema
                       (lambda () (hash-set! (playwright-sidecar-state-pending-box state) id ch)))
  ;; SEC-15: Detect dead sidecar immediately
  (when (playwright-sidecar-state-dead? state)
    (raise-browser-error "Sidecar is dead (reader EOF)" 'sidecar-crash))
  (define out (playwright-sidecar-state-stdin-port state))
  (define req (hasheq 'id id 'type type 'params params))
  ;; SEC-09: Wrap port write to convert exn:fail? to q-browser-error
  (with-handlers ([exn:fail? (lambda (e) (raise-browser-error "Sidecar port closed" 'sidecar-crash))])
    (write-string (jsexpr->string req) out)
    (newline out)
    (flush-output out))
  (define tmout (or timeout-ms (hash-ref (playwright-sidecar-state-config state) 'timeout-ms 10000)))
  (define result (sync/timeout (/ tmout 1000.0) ch))
  (call-with-semaphore sema
                       (lambda () (hash-remove! (playwright-sidecar-state-pending-box state) id)))
  (cond
    [(not result)
     (raise-browser-error (format "Sidecar command '~a' timed out after ~ams" type tmout) 'timeout)]
    [(not (hash-ref result 'success #f))
     (define err (hash-ref result 'error (hasheq 'code 'unknown 'message "Unknown error")))
     (define code (hash-ref err 'code 'adapter-error))
     (raise-browser-error (hash-ref err 'message "Adapter error")
                          (match code
                            ["timeout" 'timeout]
                            ["sidecar-crash" 'sidecar-crash]
                            ["selector-not-found" 'selector-not-found]
                            ["session-not-found" 'session-expired]
                            ["policy-violation" 'policy-violation]
                            [_ 'adapter-error]))]
    [else (hash-ref result 'data)]))

;; ---------------------------------------------------------------------------
;; Send command with auto-recovery (W0: Sidecar auto-restart on crash)
;; ---------------------------------------------------------------------------

(define max-sidecar-restarts 2)

(define (send-command-with-recovery! state
                                     type
                                     params
                                     #:timeout-ms [timeout-ms #f]
                                     #:max-retries [max-retries max-sidecar-restarts])
  ;; Retry with restart on sidecar-crash errors.
  ;; state must have a 'restart-count key in its config hash (or be a hash
  ;; for testing). Real usage passes playwright-sidecar-state.
  (let loop ([attempts 0])
    (with-handlers ([q-browser-error? (lambda (e)
                                        (if (and (eq? (q-browser-error-category e) 'sidecar-crash)
                                                 (< attempts max-retries))
                                            (begin
                                              (restart-sidecar! state)
                                              (loop (add1 attempts)))
                                            (raise e)))])
      (send-command! state type params #:timeout-ms timeout-ms))))

(define (restart-sidecar! state)
  ;; SEC-01: Guard concurrent restart with semaphore
  (define cfg (playwright-sidecar-state-config state))
  (define restart-sema (hash-ref cfg 'restart-sema #f))
  (define (do-restart)
    (with-handlers ([exn:fail? void])
      (shutdown-sidecar! state #:timeout-ms 3000))
    ;; C1: Actually relaunch the sidecar process
    (define sidecar-path (hash-ref cfg 'sidecar-path #f))
    (when sidecar-path
      (with-handlers ([exn:fail? (lambda (e)
                                   ;; SEC-04: Clear state on launch failure for clear error messages
                                   (set-playwright-sidecar-state-process! state #f)
                                   (set-playwright-sidecar-state-stdin-port! state #f)
                                   (set-playwright-sidecar-state-stdout-port! state #f)
                                   (set-playwright-sidecar-state-reader-thread! state #f)
                                   (raise-browser-error (format "Sidecar restart failed: ~a"
                                                                (exn-message e))
                                                        'sidecar-crash))])
        (define node-path (hash-ref cfg 'node-path "node"))
        (define headless? (hash-ref cfg 'headless? #t))
        ;; SEC-04: Validate paths before subprocess creation
        (unless (file-exists? sidecar-path)
          (error (format "Sidecar script not found: ~a" sidecar-path)))
        ;; DUP-02: Launch subprocess
        (launch-sidecar-process! state sidecar-path node-path headless?)
        ;; DUP-01: Restart reader thread
        (define reader (thread (make-reader-body state)))
        (set-playwright-sidecar-state-reader-thread! state reader)
        ;; SEC-15: Clear dead flag after successful restart
        (set-playwright-sidecar-state-dead?! state #f)
        ;; Restart heartbeat
        (start-heartbeat! state)))
    ;; H1: Always increment restart count (even without sidecar-path)
    (set-playwright-sidecar-state-config!
     state
     (hash-set cfg 'restart-count (add1 (hash-ref cfg 'restart-count 0))))
    ;; SEC-10: Ping-based readiness probe instead of fixed sleep
    (with-handlers ([exn:fail? void])
      (send-command! state "ping" (hasheq) #:timeout-ms 5000)))
  (if restart-sema
      (call-with-semaphore restart-sema do-restart)
      (do-restart)))

;; ---------------------------------------------------------------------------
;; Adapter interface
;; ---------------------------------------------------------------------------

(define (make-playwright-adapter sidecar-path
                                 #:node-path [node-path "node"]
                                 #:timeout-ms [timeout-ms 10000]
                                 #:headless? [headless? #t])
  (define state-box (box #f))
  (define launch-sema (make-semaphore 1))

  (define (ensure-state)
    ;; SEC-02: Double-checked locking prevents TOCTOU race
    (or (unbox state-box)
        (call-with-semaphore launch-sema
                             (lambda ()
                               (or (unbox state-box)
                                   (let ([s (launch-sidecar! sidecar-path
                                                             #:node-path node-path
                                                             #:timeout-ms timeout-ms
                                                             #:headless? headless?)])
                                     (set-box! state-box s)
                                     s))))))

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
      [_ (raise-browser-error (format "Unknown action: ~a" action) 'adapter-error)]))

  ;; Helper to build observation from sidecar response
  ;; H8: Normalize string-keyed hashes from sidecar JSON to symbol-keyed
  (define (normalize-interactive-elements elems)
    (for/list ([e (in-list elems)])
      (cond
        [(hash? e)
         (for/hasheq ([(k v) (in-hash e)])
           (values (if (string? k)
                       (string->symbol k)
                       k)
                   v))]
        [else e])))

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
                         (normalize-interactive-elements (hash-ref data 'interactiveElements '()))
                         #f)) ; metadata

  ;; Adapter callback implementations
  (define (adapter-open session-id target)
    (define st (ensure-state))
    (define data
      (send-command! st "navigate" (hasheq 'url target 'sessionId session-id) #:timeout-ms 30000))
    (data->obs data))

  (define (adapter-close session-id)
    (define st (ensure-state))
    (send-command! st "close" (hasheq 'sessionId session-id))
    (void))

  (define (adapter-navigate session-id url)
    (define st (ensure-state))
    (define data
      (send-command! st "navigate" (hasheq 'url url 'sessionId session-id) #:timeout-ms 30000))
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
                         #f ; viewport-size
                         '() ; interactive-elements
                         #f)) ; metadata

  (make-browser-adapter #:open adapter-open
                        #:close adapter-close
                        #:navigate adapter-navigate
                        #:observe adapter-observe
                        #:act adapter-act
                        #:screenshot adapter-screenshot))
