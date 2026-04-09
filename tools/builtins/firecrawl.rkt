#lang racket/base

(require racket/string
         racket/format
         racket/port
         json
         net/http-client
         net/url
         (only-in "../tool.rkt" make-success-result make-error-result))

(provide tool-firecrawl
         firecrawl-api-key       ;; for testing
         firecrawl-request       ;; for testing
         valid-action?           ;; for testing
         valid-formats?          ;; for testing
         truncate-string)

;; ============================================================
;; Configuration
;; ============================================================

(define DEFAULT-BASE-URL "https://api.firecrawl.dev/v1")
(define DEFAULT-CRAWL-TIMEOUT 30)  ;; seconds
(define DEFAULT-LIMIT 5)

;; Resolve API key: env var > config
(define (firecrawl-api-key)
  (define env-key (getenv "FIRECRAWL_API_KEY"))
  (if (and env-key (> (string-length env-key) 0))
      env-key
      (let ([cfg-path (build-path (find-system-path 'home-dir) ".q" "config.json")])
        (and (file-exists? cfg-path)
             (let ([cfg (with-handlers ([exn:fail? (lambda (_) #f)])
                          (call-with-input-file cfg-path
                            (lambda (port) (read-json port))))])
               (and (hash? cfg)
                    (let ([fc (hash-ref cfg 'firecrawl #f)])
                      (and (hash? fc)
                           (let ([k (hash-ref fc 'api-key #f)])
                             (and k (> (string-length k) 0) k))))))))))

(define (firecrawl-base-url)
  (or (getenv "FIRECRAWL_BASE_URL")
      DEFAULT-BASE-URL))

;; ============================================================
;; HTTP helpers
;; ============================================================

(define (check-status! status-line body-bytes)
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (define m (regexp-match #px"HTTP/[^ ]+ ([0-9]{3})" status-str))
  (when m
    (define code (string->number (cadr m)))
    (when (>= code 400)
      (define body-str
        (with-handlers ([exn:fail? (lambda (_) "<non-utf-8 body>")])
          (bytes->string/utf-8 body-bytes)))
      (error 'firecrawl
             "API request failed (~a): ~a"
             code
             (if (> (string-length body-str) 500)
                 (string-append (substring body-str 0 500) "...")
                 body-str)))))

(define (firecrawl-request method path [body #f])
  ;; Make an HTTP request to the Firecrawl API. Returns parsed JSON response.
  (define api-key (firecrawl-api-key))
  (unless (and api-key (> (string-length api-key) 0))
    (error 'firecrawl
           "No API key found. Set FIRECRAWL_API_KEY environment variable or add firecrawl.api-key to $HOME/.q/config.json"))
  (define url-str (string-append (string-trim (firecrawl-base-url) "/") path))
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define req-port (or (url-port uri) (if (equal? (url-scheme uri) "https") 443 80)))
  (define ssl? (equal? (url-scheme uri) "https"))
  (define path-str (url-path->string uri))
  (define headers
    (list (format "Authorization: Bearer ~a" api-key)
          "Content-Type: application/json"
          "Accept: application/json"))
  (define-values (status-line response-headers response-port)
    (if body
        (http-sendrecv host path-str
                       #:port req-port
                       #:ssl? ssl?
                       #:method method
                       #:headers headers
                       #:data (jsexpr->bytes body))
        (http-sendrecv host path-str
                       #:port req-port
                       #:ssl? ssl?
                       #:method method
                       #:headers headers)))
  (define response-body (port->bytes response-port))
  (check-status! status-line response-body)
  (bytes->jsexpr response-body))

(define (url-path->string uri)
  (string-append "/"
                 (string-join
                  (map (lambda (p) (path/param-path p))
                       (url-path uri))
                 "/")))

;; ============================================================
;; Action implementations
;; ============================================================

(define (do-search query formats limit only-main-content)
  (define body
    (make-hash
     (list (cons 'query query)
           (cons 'limit limit)
           (cons 'scrapeOptions
                 (make-hash
                  (list (cons 'formats (or formats '("markdown")))
                        (cons 'onlyMainContent only-main-content)))))))
  (define resp (firecrawl-request "POST" "/search" body))
  (define success? (hash-ref resp 'success #t))
  (if (not success?)
      (make-error-result
       (format "Search failed: ~a" (hash-ref resp 'error "unknown error")))
      (let* ([data (hash-ref resp 'data '())]
             [results (if (list? data) data '())]
             [formatted (format-search-results results)])
        (make-success-result
         (list (hasheq 'type "text" 'text formatted))
         (hasheq 'action "search"
                 'result-count (length results)
                 'query query)))))

(define (format-search-results results)
  (if (null? results)
      "No results found."
      (string-join
       (for/list ([r (in-list results)]
                  [i (in-naturals 1)])
         (define title (hash-ref r 'title "(no title)"))
         (define url (hash-ref r 'url ""))
         (define desc (hash-ref r 'description ""))
         (define content (hash-ref r 'markdown (hash-ref r 'content "")))
         (format "[~a] ~a\n~a\n~a\n~a"
                 i title url desc
                 (if (and content (not (string=? content "")))
                     (string-append "\n" (truncate-string content 10000))
                     "")))
       "\n\n---\n\n")))

(define (do-scrape url formats only-main-content)
  (define body
    (make-hash
     (list (cons 'url url)
           (cons 'formats (or formats '("markdown")))
           (cons 'onlyMainContent only-main-content))))
  (define resp (firecrawl-request "POST" "/scrape" body))
  (define success? (hash-ref resp 'success #t))
  (if (not success?)
      (make-error-result
       (format "Scrape failed: ~a" (hash-ref resp 'error "unknown error")))
      (let* ([data (hash-ref resp 'data (hasheq))]
             [markdown (hash-ref data 'markdown "")]
             [html (hash-ref data 'html "")]
             [metadata (hash-ref data 'metadata (hasheq))]
             [title (hash-ref metadata 'title "")]
             [desc (hash-ref metadata 'description "")]
             [chosen-content (cond
                               [(and formats (member "html" formats)) html]
                               [(and formats (member "rawHtml" formats))
                                (hash-ref data 'rawHtml "")]
                               [else markdown])])
        (make-success-result
         (list (hasheq 'type "text"
                       'text (format "Title: ~a\nURL: ~a\nDescription: ~a\n\n~a"
                                     title url desc
                                     (truncate-string chosen-content 50000))))
         (hasheq 'action "scrape"
                 'url url
                 'title title)))))

(define (do-crawl url formats limit timeout-secs only-main-content)
  ;; Start crawl job
  (define body
    (make-hash
     (list (cons 'url url)
           (cons 'limit limit)
           (cons 'scrapeOptions
                 (make-hash
                  (list (cons 'formats (or formats '("markdown")))
                        (cons 'onlyMainContent only-main-content)))))))
  (define start-resp (firecrawl-request "POST" "/crawl" body))
  (define job-id (hash-ref start-resp 'id #f))
  (unless job-id
    (error 'firecrawl "Crawl did not return a job ID: ~a" start-resp))
  ;; Poll for completion
  (define deadline (+ (current-seconds) timeout-secs))
  (define results (poll-crawl-status job-id deadline))
  (define formatted (format-crawl-results results))
  (make-success-result
   (list (hasheq 'type "text" 'text formatted))
   (hasheq 'action "crawl"
           'url url
           'job-id job-id
           'result-count (length results))))

(define (poll-crawl-status job-id deadline)
  (when (> (current-seconds) deadline)
    '())
  (sleep 2)
  (define resp
    (with-handlers ([exn:fail? (lambda (_) (hasheq))])
      (firecrawl-request "GET" (format "/crawl/~a" job-id))))
  (define status (hash-ref resp 'status "unknown"))
  (cond
    [(equal? status "completed")
     (hash-ref resp 'data '())]
    [(equal? status "failed")
     '()]
    [(> (current-seconds) deadline)
     (hash-ref resp 'data '())]  ;; partial results
    [else
     (poll-crawl-status job-id deadline)]))

(define (format-crawl-results results)
  (if (null? results)
      "Crawl completed but no results were returned."
      (string-join
       (for/list ([r (in-list results)]
                  [i (in-naturals 1)])
         (define metadata (hash-ref r 'metadata (hasheq)))
         (define title (hash-ref metadata 'title "(no title)"))
         (define url (hash-ref metadata 'sourceURL (hash-ref metadata 'url "")))
         (define content (hash-ref r 'markdown (hash-ref r 'content "")))
         (format "[~a] ~a\n~a\n~a"
                 i title url
                 (if (and content (not (string=? content "")))
                     (string-append "\n" (truncate-string content 10000))
                     "")))
       "\n\n---\n\n")))

(define (do-map url)
  (define body (make-hash (list (cons 'url url))))
  (define resp (firecrawl-request "POST" "/map" body))
  (define success? (hash-ref resp 'success #t))
  (if (not success?)
      (make-error-result
       (format "Map failed: ~a" (hash-ref resp 'error "unknown error")))
      (let* ([links (hash-ref resp 'links '())]
             [formatted (string-join links "\n")])
        (make-success-result
         (list (hasheq 'type "text"
                       'text (format "Found ~a URLs at ~a:\n~a"
                                     (length links) url formatted)))
         (hasheq 'action "map"
                 'url url
                 'link-count (length links))))))

;; ============================================================
;; Helpers
;; ============================================================

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len) "\n...(truncated)")
      s))

(define (valid-action? s)
  (member s '("search" "scrape" "crawl" "map")))

(define (valid-formats? fmts)
  (and (list? fmts)
       (andmap (lambda (f) (member f '("markdown" "html" "rawHtml"))) fmts)))

;; ============================================================
;; Main tool function
;; ============================================================

(define (tool-firecrawl args [exec-ctx #f])
  (define action (hash-ref args 'action #f))

  ;; Validate action
  (cond
    [(not action)
     (make-error-result "Missing required parameter 'action'. Use: search, scrape, crawl, or map")]

    [(not (valid-action? action))
     (make-error-result
      (format "Invalid action '~a'. Use: search, scrape, crawl, or map" action))]

    ;; ---- SEARCH ----
    [(string=? action "search")
     (define query (hash-ref args 'query #f))
     (unless query
       (error 'firecrawl "search requires 'query' parameter"))
     (define formats (hash-ref args 'formats #f))
     (define limit (hash-ref args 'limit DEFAULT-LIMIT))
     (define only-main (hash-ref args 'onlyMainContent #t))
     (when (and formats (not (valid-formats? formats)))
       (error 'firecrawl "formats must be a list of: markdown, html, rawHtml"))
     (do-search query formats limit only-main)]

    ;; ---- SCRAPE ----
    [(string=? action "scrape")
     (define url (hash-ref args 'url #f))
     (unless url
       (error 'firecrawl "scrape requires 'url' parameter"))
     (define formats (hash-ref args 'formats #f))
     (define only-main (hash-ref args 'onlyMainContent #t))
     (when (and formats (not (valid-formats? formats)))
       (error 'firecrawl "formats must be a list of: markdown, html, rawHtml"))
     (do-scrape url formats only-main)]

    ;; ---- CRAWL ----
    [(string=? action "crawl")
     (define url (hash-ref args 'url #f))
     (unless url
       (error 'firecrawl "crawl requires 'url' parameter"))
     (define formats (hash-ref args 'formats #f))
     (define limit (hash-ref args 'limit DEFAULT-LIMIT))
     (define timeout (hash-ref args 'timeout DEFAULT-CRAWL-TIMEOUT))
     (define only-main (hash-ref args 'onlyMainContent #t))
     (when (and formats (not (valid-formats? formats)))
       (error 'firecrawl "formats must be a list of: markdown, html, rawHtml"))
     (do-crawl url formats limit timeout only-main)]

    ;; ---- MAP ----
    [(string=? action "map")
     (define url (hash-ref args 'url #f))
     (unless url
       (error 'firecrawl "map requires 'url' parameter"))
     (do-map url)]

    [else
     (make-error-result (format "Unhandled action: ~a" action))]))
