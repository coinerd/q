#lang racket/base

;; extensions/image-pipeline.rkt — Image processing pipeline (#5265, #5266)
;;
;; Subprocess-based image resizing for multimodal LLM input.
;; SECURITY: All subprocess calls use argv lists (never shell strings).
;; Tool paths resolved via find-executable-path. No shell injection possible.

(require racket/file
         racket/path
         racket/string
         racket/port
         racket/list
         racket/contract)

;; ═══════════════════════════════════════════════════════════════════
;; Configuration parameters
;; ═══════════════════════════════════════════════════════════════════

(define max-image-width (make-parameter 2048))
(define max-image-height (make-parameter 2048))
(define max-image-bytes (make-parameter (* 5 1024 1024)))

;; Timeout parameters (seconds) for subprocess operations
(define image-probe-timeout (make-parameter 5))
(define image-metadata-timeout (make-parameter 10))
(define image-resize-timeout (make-parameter 30))

;; Resize cache: (path, max-w, max-h, fmt) -> output-path
(define image-resize-cache (make-parameter (make-hash)))

;; Semaphore for thread-safe cache access
(define cache-semaphore (make-semaphore 1))

(define (cache-ref key)
  (call-with-semaphore cache-semaphore (lambda () (hash-ref (image-resize-cache) key #f))))

(define (cache-set! key val)
  (call-with-semaphore cache-semaphore (lambda () (hash-set! (image-resize-cache) key val))))

;; ═══════════════════════════════════════════════════════════════════
;; Utility helpers
;; ═══════════════════════════════════════════════════════════════════

(define (delete-file* p)
  (with-handlers ([exn:fail? void])
    (when (file-exists? p)
      (delete-file p))))

(define (make-temp-output-path src-path fmt)
  (define ext
    (case fmt
      [(png) ".png"]
      [(jpeg jpg) ".jpg"]
      [(webp) ".webp"]
      [(gif) ".gif"]
      [else
       (define orig-ext
         (filename-extension (if (string? src-path)
                                 (string->path src-path)
                                 src-path)))
       (if orig-ext
           (string-append "." (bytes->string/utf-8 orig-ext))
           ".png")]))
  (make-temporary-file (string-append "qimage~a" ext)))

(define (supported-image-file? path)
  (define ext
    (or (path-get-extension (if (string? path)
                                (string->path path)
                                path))
        #""))
  (and (member ext '(#".png" #".jpg" #".jpeg" #".gif" #".webp")) #t))

;; ═══════════════════════════════════════════════════════════════════
;; Safe subprocess execution (argv-based, no shell strings)
;; ═══════════════════════════════════════════════════════════════════

;; Run a subprocess with argv list. Returns (values success? stdout-bytes).
;; Captures stdout and stderr. Never passes through a shell.
;; Optional timeout in seconds — kills subprocess on expiry.
(define (run-argv cmd+args #:timeout [timeout-secs #f])
  (define cmd (car cmd+args))
  (define args (cdr cmd+args))
  (define-values (sp stdout-in stdin-out stderr-in) (apply subprocess #f #f #f cmd args))
  (close-output-port stdin-out)
  (define timeout-thread
    (and timeout-secs
         (thread (lambda ()
                   (sleep timeout-secs)
                   (with-handlers ([exn:fail? void])
                     (subprocess-kill sp #t))))))
  (subprocess-wait sp)
  (when timeout-thread
    (kill-thread timeout-thread))
  (define stdout-bytes (port->bytes stdout-in))
  (close-input-port stdout-in)
  (close-input-port stderr-in)
  (values (eq? (subprocess-status sp) 0) stdout-bytes))

;; Resolve tool name to absolute path, or #f if not found.
(define (resolve-tool tool-name)
  (find-executable-path tool-name))

;; ═══════════════════════════════════════════════════════════════════
;; Tool detection
;; ═══════════════════════════════════════════════════════════════════

(define (probe-tool tool-name version-flag)
  (define path (resolve-tool tool-name))
  (and path
       (let ()
         (define-values (ok _) (run-argv (list path version-flag) #:timeout (image-probe-timeout)))
         ok)))

(define (detect-image-tools)
  (define tools
    (list (cons 'imagemagick (probe-tool "convert" "--version"))
          (cons 'magick (probe-tool "magick" "--version"))
          (cons 'sips (probe-tool "sips" "--help"))
          (cons 'ffmpeg (probe-tool "ffmpeg" "-version"))))
  (map car (filter cdr tools)))

(define detected-tools #f)

(define (available-image-tools)
  (or detected-tools
      (begin
        (set! detected-tools (detect-image-tools))
        detected-tools)))

(define (reset-image-tools!)
  (set! detected-tools #f))

;; ═══════════════════════════════════════════════════════════════════
;; Image metadata (no shell strings, no pipelines)
;; ═══════════════════════════════════════════════════════════════════

(define (extract-dimensions p)
  (define tools (available-image-tools))
  (cond
    [(or (member 'imagemagick tools) (member 'magick tools)) (extract-dimensions-imagemagick p)]
    [(member 'sips tools) (extract-dimensions-sips p)]
    [(member 'ffmpeg tools) (extract-dimensions-ffmpeg p)]
    [else (cons 0 0)]))

(define (extract-dimensions-imagemagick p)
  (define identify-path (or (resolve-tool "identify") (resolve-tool "magick")))
  (cond
    [(not identify-path) (cons 0 0)]
    [else
     (define args
       (if (equal? (path->string (file-name-from-path identify-path)) "magick")
           (list identify-path "identify" "-format" "%w %h" (path->string p))
           (list identify-path "-format" "%w %h" (path->string p))))
     (define-values (ok out-bytes) (run-argv args #:timeout (image-metadata-timeout)))
     (define out (bytes->string/utf-8 out-bytes #\?))
     (define parts (string-split (string-trim out) " "))
     (if (and ok (>= (length parts) 2))
         (cons (or (string->number (car parts)) 0) (or (string->number (cadr parts)) 0))
         (cons 0 0))]))

(define (extract-dimensions-sips p)
  (define sips-path (resolve-tool "sips"))
  (cond
    [(not sips-path) (cons 0 0)]
    [else
     (define-values (ok out-bytes)
       (run-argv #:timeout (image-metadata-timeout)
                 (list sips-path "-g" "pixelWidth" "-g" "pixelHeight" (path->string p))))
     (define out (bytes->string/utf-8 out-bytes #\?))
     (define w-match (regexp-match #rx"pixelWidth:\\s*([0-9]+)" out))
     (define h-match (regexp-match #rx"pixelHeight:\\s*([0-9]+)" out))
     (cons (if w-match
               (string->number (cadr w-match))
               0)
           (if h-match
               (string->number (cadr h-match))
               0))]))

(define (extract-dimensions-ffmpeg p)
  ;; No shell pipeline — capture stderr (ffmpeg writes to stderr) and parse in Racket
  (define ffmpeg-path (resolve-tool "ffmpeg"))
  (cond
    [(not ffmpeg-path) (cons 0 0)]
    [else
     ;; ffmpeg -i writes probe info to stderr; we capture it
     (define-values (sp stdout-in stdin-out stderr-in)
       (subprocess #f #f #f ffmpeg-path "-i" (path->string p)))
     (close-output-port stdin-out)
     ;; Timeout thread kills ffmpeg if it hangs
     (define timeout-thread
       (thread (lambda ()
                 (sleep (image-metadata-timeout))
                 (with-handlers ([exn:fail? void])
                   (subprocess-kill sp #t)))))
     (subprocess-wait sp)
     (kill-thread timeout-thread)
     (define stderr-bytes (port->bytes stderr-in))
     (close-input-port stdout-in)
     (close-input-port stderr-in)
     ;; Parse first NxN dimension from stderr output (no grep/head pipeline)
     (define stderr-str (bytes->string/utf-8 stderr-bytes #\?))
     (define m (regexp-match #rx"([0-9]+)x([0-9]+)" stderr-str))
     (if m
         (cons (or (string->number (cadr m)) 0) (or (string->number (caddr m)) 0))
         (cons 0 0))]))

(define (image-metadata path)
  (define p
    (if (string? path)
        (string->path path)
        path))
  (unless (file-exists? p)
    (error 'image-metadata "File not found: ~a" p))
  (define size-bytes (file-size p))
  (define ext (or (path-get-extension p) #""))
  (define format
    (cond
      [(member ext '(#".png" #"png")) 'png]
      [(member ext '(#".jpg" #"jpg" #".jpeg" #"jpeg")) 'jpeg]
      [(member ext '(#".gif" #"gif")) 'gif]
      [(member ext '(#".webp" #"webp")) 'webp]
      [else 'unknown]))
  (define dims (extract-dimensions p))
  (hasheq 'width
          (car dims)
          'height
          (cdr dims)
          'format
          format
          'size-bytes
          size-bytes
          'path
          (path->string p)))

;; ═══════════════════════════════════════════════════════════════════
;; Image resizing (argv-based, no shell strings)
;; ═══════════════════════════════════════════════════════════════════

(define (resize-image path
                      #:max-width [max-w (max-image-width)]
                      #:max-height [max-h (max-image-height)]
                      #:output-format [out-fmt 'auto])
  (define p (if (string? path) path path))
  (unless (file-exists? p)
    (error 'resize-image "File not found: ~a" p))
  ;; Fail-closed: if no image tools available, don't attempt resize
  (define tools (available-image-tools))
  (when (null? tools)
    (error 'resize-image
           "No image processing tools available (install ImageMagick, sips, or ffmpeg)"))
  ;; Check cache first
  (define cache-key (list p max-w max-h out-fmt))
  (define cached (cache-ref cache-key))
  (cond
    [(and cached (file-exists? cached)) cached]
    [else
     (define meta (image-metadata p))
     (define w (hash-ref meta 'width 0))
     (define h (hash-ref meta 'height 0))
     (cond
       [(and (<= w max-w) (<= h max-h) (<= (hash-ref meta 'size-bytes 0) (max-image-bytes)))
        (cache-set! cache-key p)
        p]
       [else
        (define resized
          (cond
            [(or (member 'imagemagick tools) (member 'magick tools))
             (resize-imagemagick p max-w max-h out-fmt)]
            [(member 'sips tools) (resize-sips p max-w max-h)]
            [(member 'ffmpeg tools) (resize-ffmpeg p max-w max-h out-fmt)]
            [else p]))
        (when resized
          (cache-set! cache-key resized))
        resized])]))

(define (resize-imagemagick p max-w max-h fmt)
  (define convert-path (or (resolve-tool "convert") (resolve-tool "magick")))
  (cond
    [(not convert-path) #f]
    [else
     (define out-path (make-temp-output-path p fmt))
     (define resize-arg (format "~ax~a>" max-w max-h))
     (define args
       (if (equal? (path->string (file-name-from-path convert-path)) "magick")
           (list convert-path "convert" (path->string p) "-resize" resize-arg (path->string out-path))
           (list convert-path (path->string p) "-resize" resize-arg (path->string out-path))))
     (define-values (ok _) (run-argv args #:timeout (image-resize-timeout)))
     (if ok
         out-path
         (begin
           (delete-file* out-path)
           #f))]))

(define (resize-sips p max-w max-h)
  (define sips-path (resolve-tool "sips"))
  (cond
    [(not sips-path) #f]
    [else
     (define out-path (make-temp-output-path p 'png))
     (copy-file p out-path #t)
     (define-values (ok _)
       (run-argv
        #:timeout (image-resize-timeout)
        (list sips-path "--resampleHeightWidthMax" (number->string max-w) (path->string out-path))))
     (if ok
         out-path
         (begin
           (delete-file* out-path)
           #f))]))

(define (resize-ffmpeg p max-w max-h fmt)
  (define ffmpeg-path (resolve-tool "ffmpeg"))
  (cond
    [(not ffmpeg-path) #f]
    [else
     (define out-path (make-temp-output-path p fmt))
     (define scale-arg (format "~a:~a:force_original_aspect_ratio=decrease" max-w max-h))
     (define-values (ok _)
       (run-argv
        #:timeout (image-resize-timeout)
        (list ffmpeg-path "-y" "-i" (path->string p) "-vf" scale-arg (path->string out-path))))
     (if ok
         out-path
         (begin
           (delete-file* out-path)
           #f))]))

;; ═══════════════════════════════════════════════════════════════════
;; Token estimation
;; ═══════════════════════════════════════════════════════════════════

(define (estimate-image-tokens width height)
  (define tiles-x (max 1 (quotient width 512)))
  (define tiles-y (max 1 (quotient height 512)))
  (* tiles-x tiles-y 85))

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

(provide (contract-out
          [detect-image-tools (-> (listof symbol?))]
          [available-image-tools (-> (listof symbol?))]
          [reset-image-tools! (-> void?)]
          [image-metadata (-> (or/c path-string? path?) hash?)]
          [resize-image
           (->* ((or/c path-string? path?))
                (#:max-width exact-positive-integer?
                             #:max-height exact-positive-integer?
                             #:output-format (or/c 'auto 'png 'jpeg 'jpg 'webp 'gif))
                (or/c path-string? path? #f))]
          [estimate-image-tokens
           (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [supported-image-file? (-> path-string? boolean?)]
          [run-argv
           (->* ((cons/c path-string? (listof string?)))
                (#:timeout (or/c exact-positive-integer? #f))
                (values boolean? bytes?))]
          [resolve-tool (-> string? (or/c path? #f))]
          [extract-dimensions
           (-> path? (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))])
         max-image-width
         max-image-height
         max-image-bytes
         image-resize-cache
         make-temp-output-path
         image-probe-timeout
         image-metadata-timeout
         image-resize-timeout)
