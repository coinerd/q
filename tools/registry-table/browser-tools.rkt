#lang racket/base

;; tools/registry-table/browser-tools.rkt — Browser + Firecrawl tool specs
;; STABILITY: internal

(require "spec.rkt"
         "../tool.rkt"
         "../builtins/firecrawl.rkt"
         "../builtins/browser-tools.rkt")

(provide browser-tool-specs)

;; ============================================================
;; Browser tool specs (firecrawl + 10 browser tools)
;; ============================================================

(define browser-tool-specs
  (list
   ;; firecrawl
   (tool-spec
    "firecrawl"
    (string-append "Search the web, scrape/crawl/map websites via Firecrawl. "
                   "Actions: search (web search with query), scrape (extract content from URL), "
                   "crawl (crawl multiple pages from URL), map (list all URLs at a site). "
                   "Returns markdown by default. Requires FIRECRAWL_API_KEY environment variable.")
    (hasheq
     'type
     "object"
     'required
     '("action")
     'properties
     (hasheq
      'action
      (hasheq 'type "string" 'description "Action to perform: search, scrape, crawl, or map")
      'query
      (hasheq 'type "string" 'description "Search query (for search action)")
      'url
      (hasheq 'type "string" 'description "URL to scrape, crawl, or map")
      'formats
      (hasheq 'type
              "array"
              'items
              (hasheq 'type "string")
              'description
              "Output formats: markdown (default), html, rawHtml")
      'limit
      (hasheq 'type "integer" 'description "Max results for search/crawl (default 5)")
      'onlyMainContent
      (hasheq 'type "boolean" 'description "Extract main content only (default true)")
      'timeout
      (hasheq 'type "integer" 'description "Timeout in seconds for crawl polling (default 30)")))
    tool-firecrawl
    #f)
   ;; browser_open (MEDIUM risk)
   (tool-spec "browser_open"
              "Open a browser session to a URL"
              (hasheq 'type
                      "object"
                      'required
                      '("url")
                      'properties
                      (hasheq 'url (hasheq 'type "string" 'description "URL to open")))
              handle-browser-open
              "Use browser_open to open web pages. Always close sessions when done.")
   ;; browser_observe (LOW risk)
   (tool-spec
    "browser_observe"
    "Observe current page state: URL, title, text content, console errors"
    (hasheq 'type
            "object"
            'required
            '("session-id")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'selector
                    (hasheq 'type "string" 'description "CSS selector to focus observation")))
    handle-browser-observe
    #f)
   ;; browser_click (HIGH risk)
   (tool-spec
    "browser_click"
    "Click an element on the page"
    (hasheq 'type
            "object"
            'required
            '("session-id" "selector")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'selector
                    (hasheq 'type "string" 'description "CSS selector of element to click")
                    'button
                    (hasheq 'type "string" 'description "Mouse button: left, right, middle")))
    handle-browser-click
    "Clicking may trigger side effects. Prefer observe before click.")
   ;; browser_type (HIGH risk)
   (tool-spec "browser_type"
              "Type text into an input element"
              (hasheq 'type
                      "object"
                      'required
                      '("session-id" "selector" "text")
                      'properties
                      (hasheq 'session-id
                              (hasheq 'type "string" 'description "Browser session ID")
                              'selector
                              (hasheq 'type "string" 'description "CSS selector of input element")
                              'text
                              (hasheq 'type "string" 'description "Text to type")
                              'clear-first?
                              (hasheq 'type "boolean" 'description "Clear existing text first")))
              handle-browser-type
              "Typing submits form data. Verify the correct input before typing.")
   ;; browser_press (HIGH risk)
   (tool-spec
    "browser_press"
    "Press a keyboard key"
    (hasheq 'type
            "object"
            'required
            '("session-id" "key")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'key
                    (hasheq 'type "string" 'description "Key name (e.g. Enter, Tab, Escape)")
                    'modifiers
                    (hasheq 'type "array" 'description "Modifier keys (ctrl, alt, shift, meta)")))
    handle-browser-press
    "Pressing keys may trigger form submissions or page navigation.")
   ;; browser_extract (LOW risk)
   (tool-spec
    "browser_extract"
    "Extract structured data from a page element"
    (hasheq
     'type
     "object"
     'required
     '("session-id" "selector")
     'properties
     (hasheq 'session-id
             (hasheq 'type "string" 'description "Browser session ID")
             'selector
             (hasheq 'type "string" 'description "CSS selector of element to extract from")
             'extract-type
             (hasheq 'type "string" 'description "Extraction type: text, html, accessibility")))
    handle-browser-extract
    #f)
   ;; browser_screenshot (LOW risk)
   (tool-spec
    "browser_screenshot"
    "Take a screenshot of the current page"
    (hasheq
     'type
     "object"
     'required
     '("session-id")
     'properties
     (hasheq 'session-id
             (hasheq 'type "string" 'description "Browser session ID")
             'selector
             (hasheq 'type "string" 'description "CSS selector to screenshot specific element")))
    handle-browser-screenshot
    #f)
   ;; browser_scroll (LOW risk)
   (tool-spec
    "browser_scroll"
    "Scroll the page"
    (hasheq 'type
            "object"
            'required
            '("session-id")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'direction
                    (hasheq 'type "string" 'description "Scroll direction: up or down")
                    'amount
                    (hasheq 'type "integer" 'description "Number of viewport heights to scroll")))
    handle-browser-scroll
    #f)
   ;; browser_close (LOW risk)
   (tool-spec "browser_close"
              "Close a browser session"
              (hasheq 'type
                      "object"
                      'required
                      '("session-id")
                      'properties
                      (hasheq 'session-id
                              (hasheq 'type "string" 'description "Browser session ID to close")))
              handle-browser-close
              "Always close browser sessions when done to free resources.")
   ;; browser_check_local_app (MEDIUM risk — read-only navigation)
   (tool-spec "browser_check_local_app"
              "Quick health check for a local web app: open, screenshot, extract, close."
              (hasheq 'type
                      "object"
                      'required
                      '("url")
                      'properties
                      (hasheq 'url
                              (hasheq 'type "string" 'description "Local app URL")
                              'timeout_ms
                              (hasheq 'type "integer" 'description "Timeout ms")
                              'selector
                              (hasheq 'type "string" 'description "CSS selector")
                              'screenshot
                              (hasheq 'type "boolean" 'description "Capture screenshot")))
              handle-browser-check-local-app
              "Quick local app health check.")))
