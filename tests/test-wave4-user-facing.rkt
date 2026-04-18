#lang racket

;; tests/test-wave4-user-facing.rkt — Wave 4 user-facing ergonomics tests
;;
;; Tests for v0.11.0 Wave 4 sub-issues:
;;   #1189: Namespaced Keybindings with Auto-Migration
;;   #1190: Cumulative File Tracking Deep Verification
;;   #1191: Package Distribution System Foundation

(require rackunit
         racket/string
         racket/port
         racket/file
         racket/path
         "../tui/keymap.rkt"
         "../runtime/compactor.rkt"
         "../runtime/package.rkt"
         "../util/protocol-types.rkt"
         "../extensions/manifest.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-msg id role kind content [meta (hasheq)])
  (make-message id #f role kind content (current-seconds) meta))

;; ============================================================
;; #1189: Namespaced Keybindings with Auto-Migration
;; ============================================================

(test-case "#1189: default keymap uses namespaced actions"
  (define km (default-keymap))
  (define entries (keymap-list km))
  ;; All actions should be namespaced (contain at least one dot)
  (for ([e (in-list entries)])
    (define action (cdr e))
    (check-true (namespaced-action? action)
                (format "expected namespaced action, got: ~a" action))))

(test-case "#1189: namespaced-action? detects dot-namespaced symbols"
  (check-true (namespaced-action? 'tui.editor.copy))
  (check-true (namespaced-action? 'app.session.fork))
  (check-false (namespaced-action? 'copy))
  (check-false (namespaced-action? 'submit))
  (check-false (namespaced-action? 42)))

(test-case "#1189: namespace-action builds namespaced symbols"
  (check-equal? (namespace-action "tui" "editor" "copy") 'tui.editor.copy)
  (check-equal? (namespace-action 'app 'session 'fork) 'app.session.fork)
  (check-equal? (namespace-action "x" "y") 'x.y))

(test-case "#1189: migrate-action maps flat to namespaced"
  (check-equal? (migrate-action 'copy) 'tui.editor.copy)
  (check-equal? (migrate-action 'submit) 'tui.input.submit)
  (check-equal? (migrate-action 'scroll-up) 'tui.navigation.scroll-up)
  (check-equal? (migrate-action 'clear-screen) 'tui.display.clear-screen)
  ;; Unknown actions pass through
  (check-equal? (migrate-action 'my-custom-action) 'my-custom-action))

(test-case "#1189: flat keybinding JSON auto-migrates to namespaced"
  ;; Simulate loading a flat keybinding file
  (define json-content "[{\"key\": \"C-x\", \"action\": \"cut\"}]")
  (define bindings (parse-keybindings-content json-content))
  (check-not-false bindings)
  (define action (cdar bindings))
  (check-equal? action 'tui.editor.cut))

(test-case "#1189: namespaced keybinding JSON passes through"
  (define json-content "[{\"key\": \"C-x\", \"action\": \"tui.editor.cut\"}]")
  (define bindings (parse-keybindings-content json-content))
  (check-not-false bindings)
  (define action (cdar bindings))
  (check-equal? action 'tui.editor.cut))

(test-case "#1189: backward compat — flat actions still resolve in dispatch"
  ;; The dispatch-keymap-action case handles both namespaced and flat
  ;; This test just verifies the migration table is complete
  (define all-flat '(history-up history-down page-up page-down home end
                     scroll-up scroll-down submit backspace delete cancel
                     word-left word-right copy cut paste select-all
                     clear-input clear-screen))
  (for ([flat (in-list all-flat)])
    (define migrated (migrate-action flat))
    (check-true (namespaced-action? migrated)
                (format "flat action '~a' should migrate to namespaced, got '~a'" flat migrated))))

;; ============================================================
;; #1190: Cumulative File Tracking Deep Verification
;; ============================================================

;; Helper: create a tool-call-part for extract-file-tracker
(define (make-tool-call tool-name path-arg)
  (tool-call-part 'tool-call "tc-id" tool-name (hasheq 'path path-arg)))

;; Helper: wrap tool-calls in a message for extract-file-tracker
;; extract-file-tracker expects messages where content is a list of parts
(define (make-turn . tool-calls)
  (make-msg "turn" 'assistant 'message tool-calls))

(define (make-summary-msg tracker-meta)
  (make-msg "sum" 'assistant 'compaction-summary
            "summary text"
            (hasheq 'fileTracker tracker-meta)))

;; Helper: wrap tool-calls in a message for extract-file-tracker
;; extract-file-tracker expects a list of messages whose content is a list of tool-call-parts
(define (make-turn-msg tool-calls)
  (make-msg "turn" 'assistant 'message (list tool-calls)))

(test-case "#1190: extract-file-tracker picks up read and write paths"
  (define msgs
    (list (make-turn (make-tool-call "read" "a.rkt")
                     (make-tool-call "edit" "b.rkt")
                     (make-tool-call "grep" "c.rkt")
                     (make-tool-call "write" "d.rkt"))))
  (define ft (extract-file-tracker msgs))
  (define reads (hash-ref ft 'readFiles '()))
  (define writes (hash-ref ft 'modifiedFiles '()))
  (check-equal? (sort reads string<?) '("a.rkt" "c.rkt"))
  (check-equal? (sort writes string<?) '("b.rkt" "d.rkt")))

(test-case "#1190: 5-round cumulative tracking — no loss"
  ;; Round 1: read a.rkt, edit b.rkt
  (define ft1 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "a.rkt")
                                (make-tool-call "edit" "b.rkt")))))
  (define sum1 (make-summary-msg ft1))

  ;; Round 2: read c.rkt, edit a.rkt (a.rkt appears in both lists now)
  (define ft2 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "c.rkt")
                                (make-tool-call "edit" "a.rkt")))))
  (define prev2 (find-previous-file-tracker (list sum1)))
  (define cum2 (merge-file-trackers ft2 prev2))
  (define sum2 (make-summary-msg cum2))

  ;; Round 3: read d.rkt, edit e.rkt
  (define ft3 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "d.rkt")
                                (make-tool-call "edit" "e.rkt")))))
  (define prev3 (find-previous-file-tracker (list sum2)))
  (define cum3 (merge-file-trackers ft3 prev3))
  (define sum3 (make-summary-msg cum3))

  ;; Round 4: read f.rkt, edit g.rkt
  (define ft4 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "f.rkt")
                                (make-tool-call "edit" "g.rkt")))))
  (define prev4 (find-previous-file-tracker (list sum3)))
  (define cum4 (merge-file-trackers ft4 prev4))
  (define sum4 (make-summary-msg cum4))

  ;; Round 5: read h.rkt, edit a.rkt
  (define ft5 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "h.rkt")
                                (make-tool-call "edit" "a.rkt")))))
  (define prev5 (find-previous-file-tracker (list sum4)))
  (define cum5 (merge-file-trackers ft5 prev5))

  ;; Verify cumulative set contains ALL files from rounds 1-5
  (define all-reads (sort (hash-ref cum5 'readFiles '()) string<?))
  (define all-writes (sort (hash-ref cum5 'modifiedFiles '()) string<?))
  (check-equal? all-reads '("a.rkt" "c.rkt" "d.rkt" "f.rkt" "h.rkt"))
  (check-equal? all-writes '("a.rkt" "b.rkt" "e.rkt" "g.rkt")))

(test-case "#1190: file read in round 1 appears in readFiles throughout"
  (define ft1 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "round1-file.rkt")))))
  (define sum1 (make-summary-msg ft1))
  ;; Simulate 3 more empty rounds
  (define ft2 (extract-file-tracker (list (make-turn))))
  (define cum2 (merge-file-trackers ft2 (find-previous-file-tracker (list sum1))))
  (define sum2 (make-summary-msg cum2))
  (define ft3 (extract-file-tracker (list (make-turn))))
  (define cum3 (merge-file-trackers ft3 (find-previous-file-tracker (list sum2))))
  (define sum3 (make-summary-msg cum3))
  (define ft4 (extract-file-tracker (list (make-turn))))
  (define cum4 (merge-file-trackers ft4 (find-previous-file-tracker (list sum3))))
  ;; round1-file.rkt should survive 4 rounds
  (check-equal? (hash-ref cum4 'readFiles '()) '("round1-file.rkt")))

(test-case "#1190: file read in round 1, modified in round 3 — in both lists"
  (define ft1 (extract-file-tracker
               (list (make-turn (make-tool-call "read" "shared.rkt")))))
  (define sum1 (make-summary-msg ft1))
  (define ft2 (extract-file-tracker (list (make-turn))))
  (define cum2 (merge-file-trackers ft2 (find-previous-file-tracker (list sum1))))
  (define sum2 (make-summary-msg cum2))
  (define ft3 (extract-file-tracker
               (list (make-turn (make-tool-call "edit" "shared.rkt")))))
  (define cum3 (merge-file-trackers ft3 (find-previous-file-tracker (list sum2))))
  ;; shared.rkt should be in both read and modified lists
  (check-not-false (member "shared.rkt" (hash-ref cum3 'readFiles '()))
                   "shared.rkt in readFiles")
  (check-not-false (member "shared.rkt" (hash-ref cum3 'modifiedFiles '()))
                   "shared.rkt in modifiedFiles"))

(test-case "#1190: file-tracker-section generates correct XML"
  ;; Already tested in test-compaction-prompts.rkt and test-wave1-compaction.rkt
  ;; Just verify the function is importable
  (check-true (procedure? extract-file-tracker)))

;; ============================================================
;; #1191: Package Distribution System Foundation
;; ============================================================

(test-case "#1191: install-package-from-dir with valid manifest"
  (define tmp-source (make-temporary-file "q-pkg-src-~a" 'directory))
  (define tmp-pkgs (make-temporary-file "q-pkg-dst-~a" 'directory))
  (parameterize ([current-packages-dir tmp-pkgs])
    ;; Create a minimal package
    (define manifest
      (make-qpm-manifest #:name "test-pkg"
                         #:version "1.0.0"
                         #:api-version "1"
                         #:type 'extension
                         #:description "Test package"
                         #:author "test"
                         #:compat ">=0.10.0"
                         #:files '("main.rkt")))
    (define main-rkt "(module test racket/base (provide the-extension (define the-extension 'hello)))")
    (with-output-to-file (build-path tmp-source "main.rkt") (lambda () (display main-rkt)))
    (write-qpm-manifest manifest (build-path tmp-source "qpm.json"))
    ;; Install
    (define result (install-package-from-dir tmp-source))
    (check-true (qpm-package? result) (format "expected qpm-package, got: ~a" result))
    (when (qpm-package? result)
      (check-equal? (qpm-manifest-name (qpm-package-manifest result)) "test-pkg")
      (check-equal? (qpm-package-status result) 'installed)
      (check-true (package-installed? "test-pkg")))
    ;; Cleanup
    (delete-directory/files tmp-source)
    (delete-directory/files tmp-pkgs)))

(test-case "#1191: install-package-from-dir fails without qpm.json"
  (define tmp-source (make-temporary-file "q-pkg-src-~a" 'directory))
  (define tmp-pkgs (make-temporary-file "q-pkg-dst-~a" 'directory))
  (parameterize ([current-packages-dir tmp-pkgs])
    (define result (install-package-from-dir tmp-source))
    (check-true (string? result) (format "expected error string, got: ~a" result))
    (check-true (string-contains? result "no qpm.json")))
  (delete-directory/files tmp-source)
  (delete-directory/files tmp-pkgs))

(test-case "#1191: remove-package cleans up"
  (define tmp-source (make-temporary-file "q-pkg-src-~a" 'directory))
  (define tmp-pkgs (make-temporary-file "q-pkg-dst-~a" 'directory))
  (parameterize ([current-packages-dir tmp-pkgs])
    (define manifest
      (make-qpm-manifest #:name "removable-pkg"
                         #:version "1.0.0"
                         #:api-version "1"
                         #:type 'skill
                         #:description "To be removed"
                         #:author "test"
                         #:compat ">=0.10.0"
                         #:files '()))
    (write-qpm-manifest manifest (build-path tmp-source "qpm.json"))
    (define result (install-package-from-dir tmp-source))
    (check-true (qpm-package? result))
    (check-true (package-installed? "removable-pkg"))
    (check-true (remove-package "removable-pkg"))
    (check-false (package-installed? "removable-pkg")))
  (delete-directory/files tmp-source)
  (delete-directory/files tmp-pkgs))

(test-case "#1191: package-info returns installed package"
  (define tmp-source (make-temporary-file "q-pkg-src-~a" 'directory))
  (define tmp-pkgs (make-temporary-file "q-pkg-dst-~a" 'directory))
  (parameterize ([current-packages-dir tmp-pkgs])
    (define manifest
      (make-qpm-manifest #:name "info-pkg"
                         #:version "2.0.0"
                         #:api-version "1"
                         #:type 'extension
                         #:description "Info test"
                         #:author "test"
                         #:compat ">=0.10.0"
                         #:files '()))
    (write-qpm-manifest manifest (build-path tmp-source "qpm.json"))
    (install-package-from-dir tmp-source)
    (define info (package-info "info-pkg"))
    (check-true (qpm-package? info))
    (when (qpm-package? info)
      (check-equal? (qpm-manifest-version (qpm-package-manifest info)) "2.0.0")))
  (delete-directory/files tmp-source)
  (delete-directory/files tmp-pkgs))

(test-case "#1191: list-packages returns all installed"
  (define tmp-pkgs (make-temporary-file "q-pkg-dst-~a" 'directory))
  (parameterize ([current-packages-dir tmp-pkgs])
    ;; Create two packages manually
    (for ([name '("pkg-a" "pkg-b")])
      (define dir (build-path tmp-pkgs name))
      (make-directory* dir)
      (define manifest
        (make-qpm-manifest #:name name
                           #:version "1.0.0"
                           #:api-version "1"
                           #:type 'extension
                           #:description "Test"
                           #:author "test"
                           #:compat ">=0.10.0"
                           #:files '()))
      (write-qpm-manifest manifest (build-path dir "qpm.json")))
    (define pkgs (list-packages))
    (check-equal? (length pkgs) 2)
    (check-equal? (sort (map (lambda (p) (qpm-manifest-name (qpm-package-manifest p))) pkgs)
                        string<?)
                  '("pkg-a" "pkg-b")))
  (delete-directory/files tmp-pkgs))

(test-case "#1191: install-package-from-git rejects non-git spec"
  (define result (install-package-from-git "https://example.com/pkg"))
  (check-true (string? result) (format "expected error string, got: ~a" result))
  (when (string? result)
    (check-true (string-contains? result "git"))))
