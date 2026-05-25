#lang racket/base

;; extensions/image-pipeline.rkt — Image processing pipeline (#5265, #5266)
;;
;; Subprocess-based image resizing for multimodal LLM input.
;; Detects available system tools (ImageMagick, sips, ffmpeg),
;; resizes images to fit within provider limits, extracts metadata,
;; and caches resized results.

(require racket/file
         racket/path
         racket/string
         racket/system
         racket/port
         racket/list)

;; ═══════════════════════════════════════════════════════════════════
;; Configuration parameters
;; ═══════════════════════════════════════════════════════════════════

(define max-image-width (make-parameter 2048))
(define max-image-height (make-parameter 2048))
(define max-image-bytes (make-parameter (* 5 1024 1024)))

;; Resize cache: (path, max-w, max-h, fmt) -> output-path
(define image-resize-cache (make-parameter (make-hash)))

;; ═══════════════════════════════════════════════════════════════════
;; Utility helpers (defined first — used by later functions)
;; ═══════════════════════════════════════════════════════════════════

(define (shell-escape-path p)
  (define s
    (if (path? p)
        (path->string p)
        p))
  (string-append "'" (string-replace s "'" "'\\''") "'"))

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
  (member ext '(#".png" #".jpg" #".jpeg" #".gif" #".webp")))

;; ═══════════════════════════════════════════════════════════════════
;; Tool detection
;; ═══════════════════════════════════════════════════════════════════

(define (probe-tool tool-name version-flag)
  (define out (open-output-string))
  (define err (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port err])
    (system (format "~a ~a 2>/dev/null" tool-name version-flag))))

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
;; Image metadata
;; ═══════════════════════════════════════════════════════════════════

(define (extract-dimensions p)
  (define tools (available-image-tools))
  (cond
    [(or (member 'imagemagick tools) (member 'magick tools)) (extract-dimensions-imagemagick p)]
    [(member 'sips tools) (extract-dimensions-sips p)]
    [(member 'ffmpeg tools) (extract-dimensions-ffmpeg p)]
    [else (cons 0 0)]))

(define (extract-dimensions-imagemagick p)
  (define cmd (format "identify -format '%w %h' ~a 2>/dev/null" (shell-escape-path (path->string p))))
  (define out (with-output-to-string (lambda () (system cmd))))
  (define parts (string-split (string-trim out) " "))
  (if (>= (length parts) 2)
      (cons (string->number (car parts)) (string->number (cadr parts)))
      (cons 0 0)))

(define (extract-dimensions-sips p)
  (define cmd
    (format "sips -g pixelWidth -g pixelHeight ~a 2>/dev/null" (shell-escape-path (path->string p))))
  (define out (with-output-to-string (lambda () (system cmd))))
  (define w-match (regexp-match #rx"pixelWidth:\\s*([0-9]+)" out))
  (define h-match (regexp-match #rx"pixelHeight:\\s*([0-9]+)" out))
  (cons (if w-match
            (string->number (cadr w-match))
            0)
        (if h-match
            (string->number (cadr h-match))
            0)))

(define (extract-dimensions-ffmpeg p)
  (define cmd
    (format "ffmpeg -i ~a 2>&1 | grep -o '[0-9]\\+x[0-9]\\+' | head -1"
            (shell-escape-path (path->string p))))
  (define out (with-output-to-string (lambda () (system cmd))))
  (define m (regexp-match #rx"([0-9]+)x([0-9]+)" out))
  (if m
      (cons (string->number (cadr m)) (string->number (caddr m)))
      (cons 0 0)))

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
;; Image resizing
;; ═══════════════════════════════════════════════════════════════════

(define (resize-image path
                      #:max-width [max-w (max-image-width)]
                      #:max-height [max-h (max-image-height)]
                      #:output-format [out-fmt 'auto])
  (define p (if (string? path) path path))
  (unless (file-exists? p)
    (error 'resize-image "File not found: ~a" p))
  ;; Check cache first
  (define cache-key (list p max-w max-h out-fmt))
  (define cached (hash-ref (image-resize-cache) cache-key #f))
  (cond
    [(and cached (file-exists? cached)) cached]
    [else
     (define meta (image-metadata p))
     (define w (hash-ref meta 'width 0))
     (define h (hash-ref meta 'height 0))
     (cond
       [(and (<= w max-w) (<= h max-h) (<= (hash-ref meta 'size-bytes 0) (max-image-bytes)))
        (hash-set! (image-resize-cache) cache-key p)
        p]
       [else
        (define tools (available-image-tools))
        (define resized
          (cond
            [(or (member 'imagemagick tools) (member 'magick tools))
             (resize-imagemagick p max-w max-h out-fmt)]
            [(member 'sips tools) (resize-sips p max-w max-h)]
            [(member 'ffmpeg tools) (resize-ffmpeg p max-w max-h out-fmt)]
            [else p]))
        (when resized
          (hash-set! (image-resize-cache) cache-key resized))
        resized])]))

(define (resize-imagemagick p max-w max-h fmt)
  (define out-path (make-temp-output-path p fmt))
  (define cmd
    (format "convert ~a -resize ~ax~a\\> ~a 2>/dev/null"
            (shell-escape-path p)
            max-w
            max-h
            (shell-escape-path out-path)))
  (define ok (system cmd))
  (if ok
      out-path
      (begin
        (delete-file* out-path)
        #f)))

(define (resize-sips p max-w max-h)
  (define out-path (make-temp-output-path p 'png))
  (copy-file p out-path #t)
  (define cmd
    (format "sips --resampleHeightWidthMax ~a ~a 2>/dev/null" max-w (shell-escape-path out-path)))
  (define ok (system cmd))
  (if ok
      out-path
      (begin
        (delete-file* out-path)
        #f)))

(define (resize-ffmpeg p max-w max-h fmt)
  (define out-path (make-temp-output-path p fmt))
  (define cmd
    (format "ffmpeg -y -i ~a -vf 'scale=~a:~a:force_original_aspect_ratio=decrease' ~a 2>/dev/null"
            (shell-escape-path p)
            max-w
            max-h
            (shell-escape-path out-path)))
  (define ok (system cmd))
  (if ok
      out-path
      (begin
        (delete-file* out-path)
        #f)))

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

(provide detect-image-tools
         available-image-tools
         reset-image-tools!
         image-metadata
         resize-image
         estimate-image-tokens
         supported-image-file?
         max-image-width
         max-image-height
         max-image-bytes
         image-resize-cache
         shell-escape-path
         extract-dimensions
         make-temp-output-path)
