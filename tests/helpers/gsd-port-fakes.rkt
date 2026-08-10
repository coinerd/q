#lang racket/base

(require racket/list
         racket/match
         "../../extensions/gsd/effect-ports.rkt"
         "../../extensions/gsd/github-port.rkt")

(provide make-fake-gsd-effect-ports
         fake-gsd-state?
         fake-gsd-state-calls
         fake-gsd-state-events
         fake-gsd-advance-clock!
         fake-gsd-set-git-response!
         fake-gsd-set-process-result!
         make-fake-github-adapter
         fake-github-state?
         fake-github-call-count
         fake-github-seed-issue!
         fake-github-seed-release!
         fake-github-seed-pr!
         fake-github-seed-merged-pr!)

;; ============================================================
;; GitHub fake adapter (v0.99.90 W4)
;; ============================================================

;; In-memory deterministic fake standing in for live GitHub in standard
;; tests. Records every adapter call so tests can assert "no duplicate
;; external effect" (call count stays 1 on retry).
(struct fake-github-state (issues releases prs call-log issue-counter release-counter)
  #:mutable
  #:transparent)

(define (fake-github-log! state op)
  (set-fake-github-state-call-log! state (append (fake-github-state-call-log state) (list op))))

(define (make-fake-github-adapter)
  (define state (fake-github-state (make-hash) (make-hash) (make-hash) '() 0 0))
  (values (make-github-adapter
           ;; create-issue! -> issue number as string
           (lambda (params)
             (fake-github-log! state 'create-issue!)
             (define n (add1 (fake-github-state-issue-counter state)))
             (set-fake-github-state-issue-counter! state n)
             (define key (hash-ref params 'dedup-key (hash-ref params 'title "issue")))
             (hash-set! (fake-github-state-issues state) key (number->string n))
             (number->string n))
           ;; close-issue!
           (lambda (_number)
             (fake-github-log! state 'close-issue!)
             (void))
           ;; set-board-field!
           (lambda (_params)
             (fake-github-log! state 'set-board-field!)
             (void))
           ;; merge-pr! -> merge sha
           (lambda (params)
             (fake-github-log! state 'merge-pr!)
             (define n (hash-ref params 'pull-number))
             (define sha (format "sha-merge-~a" n))
             (hash-set! (fake-github-state-prs state)
                        n
                        (list 'merged #t 'head-sha (hash-ref params 'expected-sha "") 'merge-sha sha))
             sha)
           ;; create-release! -> release id
           (lambda (params)
             (fake-github-log! state 'create-release!)
             (define n (add1 (fake-github-state-release-counter state)))
             (set-fake-github-state-release-counter! state n)
             (define id (format "rel-~a" n))
             (hash-set! (fake-github-state-releases state) (hash-ref params 'tag) id)
             id)
           ;; get-pr -> (list 'merged <bool> 'head-sha <str> 'merge-sha <str>) or #f
           (lambda (number) (hash-ref (fake-github-state-prs state) number #f))
           ;; get-issue -> (list 'state <sym> 'number <str>) or #f
           (lambda (number)
             (for/or ([(k v) (in-hash (fake-github-state-issues state))])
               (and (equal? v (number->string number))
                    (list 'state 'open 'number (number->string number)))))
           ;; find-issue-by-key
           (lambda (key) (hash-ref (fake-github-state-issues state) key #f))
           ;; find-release-by-tag
           (lambda (tag) (hash-ref (fake-github-state-releases state) tag #f)))
          state))

(define (fake-github-call-count state op)
  (length (filter (lambda (x) (eq? x op)) (fake-github-state-call-log state))))

(define (fake-github-seed-issue! state key id)
  (hash-set! (fake-github-state-issues state) key id))

(define (fake-github-seed-release! state tag id)
  (hash-set! (fake-github-state-releases state) tag id))

(define (fake-github-seed-pr! state number head-sha)
  (hash-set! (fake-github-state-prs state) number (list 'merged #f 'head-sha head-sha)))

(define (fake-github-seed-merged-pr! state number head-sha merge-sha)
  (hash-set! (fake-github-state-prs state)
             number
             (list 'merged #t 'head-sha head-sha 'merge-sha merge-sha)))

;; ============================================================
;; Aggregate fake
;; ============================================================

(struct fake-gsd-state
        (files locks calls events seconds milliseconds git-root git-response process-result)
  #:mutable
  #:transparent)

(define (record! state call)
  (set-fake-gsd-state-calls! state (append (fake-gsd-state-calls state) (list call))))

(define (path-key path)
  (path->string (if (path? path)
                    path
                    (string->path path))))

(define (make-fake-gsd-effect-ports #:seconds [seconds 1700000000]
                                    #:milliseconds [milliseconds 1700000000123]
                                    #:git-root [git-root "/repo"]
                                    #:git-response [git-response "abc123 change"])
  (define state
    (fake-gsd-state (make-hash)
                    (make-hash)
                    '()
                    '()
                    seconds
                    milliseconds
                    git-root
                    git-response
                    (gsd-process-result 0 #"ok\n" #"")))
  (define fs
    (gsd-filesystem-port
     (lambda (path)
       (record! state (list 'kind (path-key path)))
       (match (hash-ref (fake-gsd-state-files state) (path-key path) #f)
         [(cons 'file _) 'file]
         ['directory 'directory]
         [_ #f]))
     (lambda (path)
       (record! state (list 'read (path-key path)))
       (match (hash-ref (fake-gsd-state-files state) (path-key path) #f)
         [(cons 'file bytes) bytes]
         [_ (error 'fake-filesystem "not a file: ~a" path)]))
     (lambda (path bytes)
       (record! state (list 'write (path-key path) bytes))
       (hash-set! (fake-gsd-state-files state) (path-key path) (cons 'file bytes)))
     (lambda (from to)
       (record! state (list 'rename (path-key from) (path-key to)))
       (define value (hash-ref (fake-gsd-state-files state) (path-key from)))
       (hash-remove! (fake-gsd-state-files state) (path-key from))
       (hash-set! (fake-gsd-state-files state) (path-key to) value))
     (lambda (path)
       (record! state (list 'delete (path-key path)))
       (hash-remove! (fake-gsd-state-files state) (path-key path)))
     (lambda (path)
       (record! state (list 'mkdir (path-key path)))
       (hash-set! (fake-gsd-state-files state) (path-key path) 'directory))
     (lambda (path)
       (record! state (list 'list (path-key path)))
       (sort (hash-keys (fake-gsd-state-files state)) string<?))
     (lambda (path)
       (record! state (list 'acquire-lock (path-key path)))
       (and (not (hash-ref (fake-gsd-state-locks state) (path-key path) #f))
            (let ([token (gensym 'fake-lock)])
              (hash-set! (fake-gsd-state-locks state) (path-key path) token)
              token)))
     (lambda (path token)
       (record! state (list 'release-lock (path-key path) token))
       (when (eq? token (hash-ref (fake-gsd-state-locks state) (path-key path) #f))
         (hash-remove! (fake-gsd-state-locks state) (path-key path))))))
  (define git
    (gsd-git-port (lambda (start)
                    (record! state (list 'git-find-root (path-key start)))
                    (fake-gsd-state-git-root state))
                  (lambda (root files)
                    (record! state (list 'git-head-summary (path-key root) files))
                    (fake-gsd-state-git-response state))))
  (define clock
    (gsd-clock-port (lambda () (fake-gsd-state-seconds state))
                    (lambda () (fake-gsd-state-milliseconds state))))
  (define process
    (gsd-process-port (lambda (program args cwd)
                        (record! state (list 'process-run program args (path-key cwd)))
                        (fake-gsd-state-process-result state))
                      (lambda () (record! state '(stop-worker)))))
  (define github (make-dry-run-github-port))
  (define (event-sink event payload)
    (record! state (list 'event event payload))
    (set-fake-gsd-state-events! state
                                (append (fake-gsd-state-events state) (list (cons event payload)))))
  (values (gsd-effect-ports fs git clock process github event-sink) state))

(define (fake-gsd-advance-clock! state milliseconds)
  (set-fake-gsd-state-milliseconds! state (+ (fake-gsd-state-milliseconds state) milliseconds))
  (set-fake-gsd-state-seconds! state (+ (fake-gsd-state-seconds state) (quotient milliseconds 1000))))

(define (fake-gsd-set-git-response! state response)
  (set-fake-gsd-state-git-response! state response))

(define (fake-gsd-set-process-result! state result)
  (set-fake-gsd-state-process-result! state result))
