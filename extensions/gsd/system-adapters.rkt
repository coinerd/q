#lang racket/base
;; STABILITY: internal

;; Concrete production adapters for extensions/gsd/effect-ports.rkt.
;; Construction is centralized by composition-root.rkt; GSD domain modules
;; consume port values rather than importing this effect shell directly.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "effect-ports.rkt"
         (only-in "../../sandbox/gateway-bridge.rkt" shutdown-worker!))

(provide make-system-filesystem-port
         make-system-git-port
         make-system-clock-port
         make-system-process-port)

(define (system-kind path)
  (cond
    [(file-exists? path) 'file]
    [(directory-exists? path) 'directory]
    [(link-exists? path) 'link]
    [else #f]))

(define (system-write-bytes! path content)
  (call-with-output-file path
                         (lambda (out)
                           (write-bytes content out)
                           (void))
                         #:exists 'truncate/replace))

(define (system-delete! path)
  (case (system-kind path)
    [(directory) (delete-directory/files path)]
    [(file link) (delete-file path)]
    [else (void)]))

(define (system-acquire-lock path)
  (define out (open-output-file path #:exists 'update))
  (if (port-try-file-lock? out 'exclusive)
      out
      (begin
        (close-output-port out)
        #f)))

(define (system-release-lock! _path token)
  (when (output-port? token)
    (port-file-unlock token)
    (close-output-port token)))

(define (make-system-filesystem-port)
  (gsd-filesystem-port system-kind
                       file->bytes
                       system-write-bytes!
                       (lambda (from to) (rename-file-or-directory from to #t))
                       system-delete!
                       (lambda (path) (make-directory* path))
                       (lambda (path) (sort (directory-list path #:build? #t) path<?))
                       system-acquire-lock
                       system-release-lock!))

(define (find-git-root start-dir)
  (define start-path (path->complete-path start-dir))
  (define (has-git? dir)
    (define marker (build-path dir ".git"))
    (or (directory-exists? marker) (file-exists? marker)))
  (define q-sub (build-path start-path "q"))
  (cond
    [(has-git? start-path) start-path]
    [(and (directory-exists? q-sub) (has-git? q-sub)) q-sub]
    [else
     (let loop ([dir start-path])
       (cond
         [(has-git? dir) dir]
         [else
          (define-values (parent _name _dir?) (split-path dir))
          (and (path? parent) (not (equal? parent dir)) (loop parent))]))]))

(define (run-system-process program args cwd)
  (parameterize ([current-directory cwd])
    (define executable (or (find-executable-path program) program))
    (define-values (sp out in err) (apply subprocess #f 'out 'in 'err executable args))
    (close-output-port in)
    (define stdout (port->bytes out))
    (define stderr (port->bytes err))
    (close-input-port out)
    (close-input-port err)
    (subprocess-wait sp)
    (gsd-process-result (subprocess-status sp) stdout stderr)))

(define (make-system-process-port)
  (gsd-process-port run-system-process shutdown-worker!))

(define (make-system-git-port process-port)
  (gsd-git-port
   find-git-root
   (lambda (root files)
     (if (null? files)
         ""
         (with-handlers ([exn:fail? (lambda (_) "")])
           (define result
             ((gsd-process-port-run process-port) "git"
                                                  (append '("show" "--stat" "--oneline" "HEAD" "--")
                                                          files)
                                                  root))
           (define trimmed (string-trim (bytes->string/utf-8 (gsd-process-result-stdout result))))
           (if (> (string-length trimmed) 2000)
               (string-append (substring trimmed 0 2000) "...")
               trimmed))))))

(define (make-system-clock-port)
  (gsd-clock-port current-seconds current-inexact-milliseconds))
