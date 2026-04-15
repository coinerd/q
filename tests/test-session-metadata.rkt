#lang racket

;; tests/test-session-metadata.rkt — tests for Session Metadata, Labeling (#710-#712)
;;
;; Covers:
;;   - #710: Session naming for display
;;   - #711: Entry labeling for marking important session points
;;   - #712: Parent feature

(require rackunit
         racket/file
         racket/path
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-metadata.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-session-dir)
  (define dir (make-temporary-file "session-meta-test-~a" 'directory))
  dir)

(define (make-session-log dir)
  (define log-path (build-path dir "session.jsonl"))
  (write-session-version-header! log-path)
  log-path)

(define (append-user-msg! log-path text)
  (define msg (make-message (format "msg-~a" (current-inexact-milliseconds))
                            #f 'user 'text
                            (list (make-text-part text))
                            (current-seconds) (hasheq)))
  (append-entry! log-path msg)
  msg)

;; ============================================================
;; #710: Session naming
;; ============================================================

(test-case "set-session-name! / get-session-name: basic round-trip"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-session-name! log-path "My First Session")
  (check-equal? (get-session-name log-path) "My First Session")
  (delete-directory/files dir))

(test-case "get-session-name: returns #f when no name set"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (check-false (get-session-name log-path))
  (delete-directory/files dir))

(test-case "set-session-name!: overwrites previous name"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-session-name! log-path "First Name")
  (set-session-name! log-path "Updated Name")
  (check-equal? (get-session-name log-path) "Updated Name")
  (delete-directory/files dir))

(test-case "set-session-name!: persists to log"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-session-name! log-path "Persistent Name")
  ;; Re-load log to verify persistence
  (define entries (load-session-log log-path))
  (define name-entries (filter session-name-entry? entries))
  (check-equal? (length name-entries) 1)
  (check-equal? (hash-ref (message-meta (car name-entries)) 'name) "Persistent Name")
  (delete-directory/files dir))

(test-case "session-name-entry?: recognizes name entries"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-session-name! log-path "Test")
  (define entries (load-session-log log-path))
  (define name-entry (findf session-name-entry? entries))
  (check-true (message? name-entry))
  (delete-directory/files dir))

(test-case "session-name-entry?: returns #f for regular messages"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (append-user-msg! log-path "hello")
  (define entries (load-session-log log-path))
  (define user-entries (filter (lambda (e) (and (message? e)
                                                  (eq? (message-kind e) 'text)))
                                entries))
  (check-true (andmap (lambda (e) (not (session-name-entry? e))) user-entries))
  (delete-directory/files dir))

;; ============================================================
;; #711: Entry labeling
;; ============================================================

(test-case "set-entry-label! / get-entry-label: checkpoint label"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (define msg (append-user-msg! log-path "important message"))
  (define msg-id (format "msg-~a" (current-inexact-milliseconds)))
  ;; Use a known ID
  (set-entry-label! log-path "test-msg-1" 'checkpoint)
  (define entries (load-session-log log-path))
  (check-equal? (get-entry-label entries "test-msg-1") 'checkpoint)
  (delete-directory/files dir))

(test-case "set-entry-label! / get-entry-label: milestone label"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-entry-label! log-path "test-msg-2" 'milestone)
  (define entries (load-session-log log-path))
  (check-equal? (get-entry-label entries "test-msg-2") 'milestone)
  (delete-directory/files dir))

(test-case "set-entry-label! / get-entry-label: branch-point label"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-entry-label! log-path "test-msg-3" 'branch-point)
  (define entries (load-session-log log-path))
  (check-equal? (get-entry-label entries "test-msg-3") 'branch-point)
  (delete-directory/files dir))

(test-case "set-entry-label!: with description"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-entry-label! log-path "test-msg-4" 'checkpoint
                     #:description "Before refactoring")
  (define entries (load-session-log log-path))
  (define label-entries (filter entry-label? entries))
  (check-equal? (length label-entries) 1)
  (check-equal? (hash-ref (message-meta (car label-entries)) 'description)
                "Before refactoring")
  (delete-directory/files dir))

(test-case "set-entry-label!: rejects invalid label type"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (check-exn exn:fail:contract?
    (lambda () (set-entry-label! log-path "msg-1" 'invalid-type)))
  (delete-directory/files dir))

(test-case "get-entry-label: returns #f for unlabeled entry"
  (define entries '())
  (check-false (get-entry-label entries "nonexistent")))

(test-case "entry-label?: recognizes label entries"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-entry-label! log-path "msg-1" 'checkpoint)
  (define entries (load-session-log log-path))
  (define labels (filter entry-label? entries))
  (check-equal? (length labels) 1)
  (delete-directory/files dir))

(test-case "list-labeled-entries: returns all labels"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))
  (set-entry-label! log-path "msg-1" 'checkpoint)
  (set-entry-label! log-path "msg-2" 'milestone)
  (set-entry-label! log-path "msg-3" 'branch-point)
  (define entries (load-session-log log-path))
  (define labels (list-labeled-entries entries))
  (check-equal? (length labels) 3)
  (define label-types (map cdr labels))
  (check-true (and (member 'checkpoint label-types) #t))
  (check-true (and (member 'milestone label-types) #t))
  (check-true (and (member 'branch-point label-types) #t))
  (delete-directory/files dir))

(test-case "label-type?: validates label types"
  (check-true (label-type? 'checkpoint))
  (check-true (label-type? 'milestone))
  (check-true (label-type? 'branch-point))
  (check-false (label-type? 'invalid))
  (check-false (label-type? "checkpoint")))

;; ============================================================
;; #712: Integration
;; ============================================================

(test-case "integration: name and label session"
  (define dir (make-temp-session-dir))
  (define log-path (make-session-log dir))

  ;; Set name
  (set-session-name! log-path "Integration Test Session")

  ;; Add some messages
  (append-user-msg! log-path "message 1")
  (append-user-msg! log-path "message 2")

  ;; Label an important point
  (set-entry-label! log-path "msg-1" 'milestone
                     #:description "Halfway point")

  ;; Verify name
  (check-equal? (get-session-name log-path) "Integration Test Session")

  ;; Verify label
  (define entries (load-session-log log-path))
  (check-equal? (get-entry-label entries "msg-1") 'milestone)
  (check-equal? (length (list-labeled-entries entries)) 1)

  ;; Update name
  (set-session-name! log-path "Updated Session")
  (check-equal? (get-session-name log-path) "Updated Session")

  (delete-directory/files dir))
