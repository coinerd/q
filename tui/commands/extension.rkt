#lang racket/base

;; tui/commands/extension.rkt — extension management command handlers
;;
;; Extracted from commands.rkt (ARCH-06).
;; Handles /activate, /deactivate, /reload commands and helpers.

(require racket/string
         racket/list
         racket/file
         racket/path
         "../state.rkt"
         "../../agent/event-bus.rkt"
         "../../runtime/extension-catalog.rkt"
         "../../extensions/hooks.rkt"
         "../../extensions/loader.rkt"
         "../../extensions/api.rkt"
         "context.rkt")

(provide handle-activate-command
         handle-reload-command
         handle-deactivate-command)

;; ============================================================
;; Hot-load helper
;; ============================================================

;; Try to hot-load a newly activated extension into the running registry.
;; Returns a status entry (success or warning) or #f if no registry available.
(define (try-hot-load-extension cctx name target-dir)
  (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
  (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
  (define bus (cmd-ctx-event-bus cctx))
  (cond
    [(not ext-reg) #f]
    [else
     (define ext-path (build-path target-dir (string-append name ".rkt")))
     (cond
       [(not (file-exists? ext-path))
        (make-entry 'system
                    (format "  Warning: extension file not found for hot-load: ~a" ext-path)
                    (current-inexact-milliseconds)
                    (hash))]
       [else
        (with-handlers ([exn:fail? (λ (e)
                                     (log-debug "hot-load failed for ~a: ~a" name (exn-message e))
                                     (make-entry 'system
                                                 (format "  Warning: hot-load failed: ~a"
                                                         (exn-message e))
                                                 (current-inexact-milliseconds)
                                                 (hash)))])
          (define loaded? (load-extension! ext-reg ext-path #:event-bus bus))
          (if loaded?
              (make-entry 'system
                          (format "  Extension '~a' loaded into running session." name)
                          (current-inexact-milliseconds)
                          (hash))
              (make-entry 'error
                          (format "  Warning: extension '~a' could not be loaded." name)
                          (current-inexact-milliseconds)
                          (hash))))])]))

;; ============================================================
;; /activate command
;; ============================================================

;; handle-activate-command : cmd-ctx? -> 'continue
;; Supports:
;;   /activate              — show status (active + available)
;;   /activate --available  — list all known extensions
;;   /activate <name>       — activate extension in project dir
;;   /activate --global <name> — activate in ~/.q/extensions/
(define (handle-activate-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define project-dir (current-directory))
  (define entries
    (cond
      [else
       ;; Parse args from raw input text (after /activate)
       (define input (unbox (cmd-ctx-input-text-box cctx)))
       (define args-str (string-trim (regexp-replace #rx"^/activate\\s*" input "")))
       (define args (filter (λ (a) (not (string=? a ""))) (string-split args-str)))
       (cond
         ;; /activate --available
         [(member "--available" args) (list-available-entries)]
         ;; /activate --global <name>
         [(member "--global" args)
          (define name
            (for/or ([a (in-list args)]
                     #:when (not (string-prefix? a "--")))
              a))
          (cond
            [(not name) (list (make-entry 'error "Usage: /activate --global <name>" 0 (hash)))]
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define q-home (build-path (find-system-path 'home-dir) ".q"))
             (define target-dir (build-path q-home "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (activate-extension! name target-dir)
               (define hot-load-entry (try-hot-load-extension cctx name target-dir))
               (if hot-load-entry
                   (list (make-entry 'system
                                     (format "Extension '~a' activated globally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash))
                         hot-load-entry)
                   (list (make-entry 'system
                                     (format "Extension '~a' activated globally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash)))))])]
         ;; /activate <name> — activate in project-local dir
         [(and (pair? args) (not (string-prefix? (car args) "--")))
          (define name (car args))
          (cond
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define target-dir (build-path project-dir ".q" "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (activate-extension! name target-dir)
               (define hot-load-entry (try-hot-load-extension cctx name target-dir))
               (if hot-load-entry
                   (list (make-entry 'system
                                     (format "Extension '~a' activated locally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash))
                         hot-load-entry)
                   (list (make-entry 'system
                                     (format "Extension '~a' activated locally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash)))))])]
         ;; /activate with no args — show status
         [else (list-status-entries project-dir)])]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; ============================================================
;; /reload command
;; ============================================================

(define (handle-reload-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
  (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
  (cond
    [(not ext-reg)
     (define entry (make-entry 'error "[no extension registry available]" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    [else
     ;; Discover extension directories: global + project-local
     (define q-home (build-path (find-system-path 'home-dir) ".q"))
     (define project-dir (current-directory))
     (define global-ext-dir (build-path q-home "extensions"))
     (define local-ext-dir (build-path project-dir ".q" "extensions"))
     (define ext-paths (filter directory-exists? (list global-ext-dir local-ext-dir)))
     (cond
       [(null? ext-paths)
        (define entry (make-entry 'system "[no extension directories found]" 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
        (define loaded-names (reload-extensions! ext-reg ext-paths))
        (define n (length loaded-names))
        (define msg
          (if (zero? n)
              "[reload complete: no extensions loaded]"
              (format "[reload complete: ~a extension~a reloaded (~a)]"
                      n
                      (if (= n 1) "" "s")
                      (string-join loaded-names ", "))))
        (define entry (make-entry 'system msg 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue])]))

;; ============================================================
;; /deactivate command
;; ============================================================

(define (handle-deactivate-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define project-dir (current-directory))
  (define entries
    (cond
      [else
       ;; Parse args from raw input text (after /deactivate)
       (define input (unbox (cmd-ctx-input-text-box cctx)))
       (define args-str (string-trim (regexp-replace #rx"^/deactivate\\s*" input "")))
       (define args (filter (λ (a) (not (string=? a ""))) (string-split args-str)))
       (cond
         [(null? args)
          (list
           (make-entry 'error "Usage: /deactivate <name> or /deactivate --global <name>" 0 (hash)))]
         [(member "--global" args)
          (define name
            (for/or ([a (in-list args)]
                     #:when (not (string-prefix? a "--")))
              a))
          (cond
            [(not name) (list (make-entry 'error "Usage: /deactivate --global <name>" 0 (hash)))]
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define q-home (build-path (find-system-path 'home-dir) ".q"))
             (define target-dir (build-path q-home "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (deactivate-extension! name target-dir)
               (list (make-entry 'system
                                 (format "Extension '~a' deactivated globally (~a)" name target-dir)
                                 (current-inexact-milliseconds)
                                 (hash))))])]
         [else
          (define name (car args))
          (cond
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define target-dir (build-path project-dir ".q" "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (deactivate-extension! name target-dir)
               (list (make-entry 'system
                                 (format "Extension '~a' deactivated locally (~a)" name target-dir)
                                 (current-inexact-milliseconds)
                                 (hash))))])])]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; ============================================================
;; Private helpers
;; ============================================================

;; list-status-entries : path? -> (listof entry?)
(define (list-status-entries project-dir)
  (define local-dir (build-path project-dir ".q" "extensions"))
  (define global-dir (build-path (find-system-path 'home-dir) ".q" "extensions"))
  (define active-local
    (if (directory-exists? local-dir)
        (map path->string (directory-list local-dir))
        '()))
  (define active-global
    (if (directory-exists? global-dir)
        (map path->string (directory-list global-dir))
        '()))
  (define known (list-known-extensions))
  (define known-names (map ext-info-name known))
  (define active-names (append active-global active-local))
  (append (list (make-entry 'system "Extension Status:" 0 (hash)))
          (if (null? active-names)
              (list (make-entry 'system "  No extensions activated." 0 (hash)))
              (for/list ([n (in-list (remove-duplicates active-names))])
                (make-entry 'system (format "  ● ~a" n) 0 (hash))))
          (list (make-entry 'system "" 0 (hash))
                (make-entry 'system "Available extensions (use /activate <name>):" 0 (hash)))
          (for/list ([n (in-list known-names)])
            (make-entry 'system (format "  ○ ~a" n) 0 (hash)))
          (list (make-entry 'system "" 0 (hash))
                (make-entry
                 'system
                 "Use /activate <name> for project-local, /activate --global <name> for global."
                 0
                 (hash)))))

;; list-available-entries : -> (listof entry?)
(define (list-available-entries)
  (define known (list-known-extensions))
  (append
   (list (make-entry 'system "Available extensions:" 0 (hash)))
   (for/list ([e (in-list known)])
     (make-entry 'system (format "  ~a (~a)" (ext-info-name e) (ext-info-source-path e)) 0 (hash)))
   (list (make-entry 'system "" 0 (hash))
         (make-entry 'system "Use /activate <name> or /activate --global <name>" 0 (hash)))))
