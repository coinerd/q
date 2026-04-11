#lang racket/base

;; extensions/quarantine.rkt — extension lifecycle: disable, quarantine, restore
;;
;; Provides:
;;   - current-quarantine-dir : parameter for quarantine root directory
;;   - quarantine-state-file  : path to state.json inside quarantine dir
;;   - extension-state        : query current state of a named extension
;;   - disable-extension!     : mark extension as disabled (no file moves)
;;   - quarantine-extension!  : move extension dir to quarantine, mark quarantined
;;   - restore-extension!     : move extension back, mark active
;;   - list-quarantined       : list all quarantined extensions with metadata
;;   - format-extension-status : human-readable status string

(require racket/contract
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         json)

(provide
 current-quarantine-dir
 quarantine-state-file
 extension-state
 disable-extension!
 quarantine-extension!
 restore-extension!
 list-quarantined
 format-extension-status)

;; ============================================================
;; Parameters
;; ============================================================

(define current-quarantine-dir
  (make-parameter (build-path (find-system-path 'home-dir) ".q" "quarantine")))

;; ============================================================
;; quarantine-state-file : -> path?
;; ============================================================

(define (quarantine-state-file)
  (build-path (current-quarantine-dir) "state.json"))

;; ============================================================
;; Internal: read/write state file atomically
;; ============================================================
;; State structure (JSON):
;;   { "disabled": ["name1", "name2"],
;;     "quarantined": {
;;       "name3": { "original-path": "...", "quarantined-at": "..." }
;;     },
;;     "active": ["name4", "name5"] }
;;
;; Internally, quarantined keys are stored as symbols so json/write-json
;; can serialize them.  Extension names are always strings at the API level.

(define (read-state)
  (define sf (quarantine-state-file))
  (cond
    [(not (file-exists? sf))
     (hash 'disabled '() 'quarantined (hasheq) 'active '())]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "quarantine state corrupted, resetting: ~a"
                                                       (exn-message e)))
                                  (hash 'disabled '() 'quarantined (hasheq) 'active '()))])
       (define data (call-with-input-file sf read-json))
       (hash 'disabled (hash-ref data 'disabled '())
             'quarantined (hash-ref data 'quarantined (hasheq))
             'active (hash-ref data 'active '())))]))

(define (write-state! state)
  (define qdir (current-quarantine-dir))
  (unless (directory-exists? qdir)
    (make-directory* qdir))
  (define sf (quarantine-state-file))
  (define tmp (build-path qdir ".state.json.tmp"))
  (call-with-output-file tmp
    (lambda (p)
      (write-json state p)
      (newline p))
    #:exists 'truncate/replace)
  (rename-file-or-directory tmp sf #t))

;; Helper: convert string name to symbol for use as quarantined hash key
(define (name->sym n) (string->symbol n))
(define (sym->name s) (symbol->string s))

;; ============================================================
;; extension-state : string? -> (or/c 'active 'disabled 'quarantined 'unknown)
;; ============================================================

(define (extension-state name)
  (define state (read-state))
  (define disabled (hash-ref state 'disabled '()))
  (define quarantined (hash-ref state 'quarantined (hasheq)))
  (cond
    [(member name (if (list? disabled) disabled '())) 'disabled]
    [(hash-has-key? quarantined (name->sym name)) 'quarantined]
    [(member name (hash-ref state 'active '())) 'active]
    [else 'unknown]))

;; ============================================================
;; disable-extension! : string? -> void?
;; ============================================================

(define (disable-extension! name)
  (define state (read-state))
  (define disabled (hash-ref state 'disabled '()))
  (define quarantined (hash-ref state 'quarantined (hasheq)))
  (define new-disabled
    (if (member name disabled)
        disabled
        (append disabled (list name))))
  (define new-quarantined (hash-remove quarantined (name->sym name)))
  (define new-active
    (filter (lambda (n) (not (equal? n name)))
            (hash-ref state 'active '())))
  (write-state! (hash 'disabled new-disabled
                       'quarantined new-quarantined
                       'active new-active)))

;; ============================================================
;; quarantine-extension! : string? path-string? -> void?
;; ============================================================

(define (quarantine-extension! name src-path)
  (define state (read-state))
  (define disabled (hash-ref state 'disabled '()))
  (define quarantined (hash-ref state 'quarantined (hasheq)))
  (define qdir (current-quarantine-dir))
  (unless (directory-exists? qdir)
    (make-directory* qdir))
  (define dest (build-path qdir name))
  (when (directory-exists? src-path)
    (rename-file-or-directory src-path dest #t))
  (define new-disabled (filter (lambda (n) (not (equal? n name))) disabled))
  (define new-quarantined
    (hash-set quarantined (name->sym name)
              (hash 'original-path (path->string (path->complete-path src-path))
                    'quarantined-at (seconds->iso8601 (current-seconds)))))
  (define new-active
    (filter (lambda (n) (not (equal? n name)))
            (hash-ref state 'active '())))
  (write-state! (hash 'disabled new-disabled
                       'quarantined new-quarantined
                       'active new-active)))

;; ============================================================
;; restore-extension! : string? path-string? -> void?
;; ============================================================

(define (restore-extension! name dest-path)
  (define state (read-state))
  (define disabled (hash-ref state 'disabled '()))
  (define quarantined (hash-ref state 'quarantined (hasheq)))
  (define qdir (current-quarantine-dir))
  (define src (build-path qdir name))
  ;; If quarantined dir exists, move it back
  (when (directory-exists? src)
    (define dest-parent
      (let-values ([(base _name _must-be-dir?) (split-path dest-path)])
        base))
    (when (and dest-parent (not (directory-exists? dest-parent)))
      (make-directory* dest-parent))
    (rename-file-or-directory src dest-path #t))
  (define new-disabled (filter (lambda (n) (not (equal? n name))) disabled))
  (define new-quarantined (hash-remove quarantined (name->sym name)))
  (define active (hash-ref state 'active '()))
  (define new-active
    (if (member name active)
        active
        (append active (list name))))
  (write-state! (hash 'disabled new-disabled
                       'quarantined new-quarantined
                       'active new-active)))

;; ============================================================
;; list-quarantined : -> (listof hash?)
;; ============================================================

(define (list-quarantined)
  (define state (read-state))
  (define quarantined (hash-ref state 'quarantined (hasheq)))
  (for/list ([(sym-key meta) (in-hash quarantined)])
    (make-hash (list (cons 'name (sym->name sym-key))
                     (cons 'state "quarantined")
                     (cons 'original-path (hash-ref meta 'original-path ""))
                     (cons 'quarantined-at (hash-ref meta 'quarantined-at ""))))))

;; ============================================================
;; format-extension-status : string? -> string?
;; ============================================================

(define (format-extension-status name)
  (define st (extension-state name))
  (match st
    ['active   (format "~a: active" name)]
    ['disabled (format "~a: disabled" name)]
    ['quarantined
     (define state (read-state))
     (define quarantined (hash-ref state 'quarantined (hasheq)))
     (define meta (hash-ref quarantined (name->sym name) (hash)))
     (format "~a: quarantined (from ~a at ~a)"
             name
             (hash-ref meta 'original-path "?")
             (hash-ref meta 'quarantined-at "?"))]
    ['unknown  (format "~a: unknown" name)]))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (seconds->iso8601 secs)
  (define d (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (~r (date-month d)  #:min-width 2 #:pad-string "0")
          (~r (date-day d)    #:min-width 2 #:pad-string "0")
          (~r (date-hour d)   #:min-width 2 #:pad-string "0")
          (~r (date-minute d) #:min-width 2 #:pad-string "0")
          (~r (date-second d) #:min-width 2 #:pad-string "0")))
