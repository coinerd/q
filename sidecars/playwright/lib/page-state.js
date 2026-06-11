// lib/page-state.js — Extract page state for observation
//
// Returns: { url, title, textContent, visibleText, domSummary,
//            accessibilityTree, consoleErrors, viewportSize }


// ---------------------------------------------------------------------------
// q-id injection: annotate interactive elements for LLM targeting
// ---------------------------------------------------------------------------

async function injectQIds(page) {
  return page.evaluate(() => {
    const INTERACTIVE_SELECTOR =
      'a, button, input, select, textarea, details, summary, [contenteditable], ' +
      '[role="button"], [role="link"], [role="tab"], [role="menuitem"], [role="option"], ' +
      '[role="checkbox"], [role="radio"], [role="switch"], [onclick]';
    const MAX_ELEMENTS = 200;
    const elements = Array.from(document.querySelectorAll(INTERACTIVE_SELECTOR));
    // M9: Track if we hit the cap
    const truncated = elements.length > MAX_ELEMENTS;
    const selected = elements.slice(0, MAX_ELEMENTS);
    let counter = 0;
    const items = [];
    for (const el of selected) {
      const qId = String(counter++);
      el.setAttribute('q-id', qId);
      items.push({
        qId,
        tag: el.tagName.toLowerCase(),
        role: el.getAttribute('role') || null,
        text: (el.textContent || '').substring(0, 100).trim(),
        href: el.getAttribute('href') || null,
        placeholder: el.getAttribute('placeholder') || null,
        type: el.getAttribute('type') || null,
        ariaLabel: el.getAttribute('aria-label') || null
      });
    }
    // M9: Add truncation indicator
    if (truncated) {
      items.push({ qId: '__truncated__', tag: 'meta', text: `Elements truncated: ${elements.length} total, showing ${MAX_ELEMENTS}` });
    }
    return items;
  });
}

async function extract(page) {
  const [url, title, textContent, visibleText, domSummary, viewportSize] = await Promise.all([
    page.url(),
    page.title(),
    page.evaluate(() => document.body?.textContent || ''),
    page.evaluate(() => {
      // Get visible text only. The body can be null during navigation or for
      // unusual documents; return an empty observation instead of throwing.
      if (!document.body) return '';
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, {
        acceptNode(node) {
          const el = node.parentElement;
          if (!el) return NodeFilter.FILTER_REJECT;
          const style = getComputedStyle(el);
          if (style.display === 'none' || style.visibility === 'hidden') return NodeFilter.FILTER_REJECT;
          return NodeFilter.FILTER_ACCEPT;
        }
      });
      const texts = [];
      while (walker.nextNode()) {
        const t = walker.currentNode.textContent.trim();
        if (t) texts.push(t);
      }
      return texts.join(' ');
    }),
    page.evaluate(() => {
      // Simplified DOM summary: tag counts
      if (!document.body) return { tagCounts: {}, childCount: 0 };
      const tags = {};
      const MAX_DOM_ELEMENTS = 10000;
      let count = 0;
      document.querySelectorAll('*').forEach(el => {
        if (count++ > MAX_DOM_ELEMENTS) return; // SEC-13: bail-out for pathological pages
        const t = el.tagName.toLowerCase();
        tags[t] = (tags[t] || 0) + 1;
      });
      return { tagCounts: tags, childCount: document.body?.children.length || 0, scannedCount: count };
    }),
    page.viewportSize()
  ]);

  // Inject q-id annotations on interactive elements (W0: DOM tagging)
  const interactiveElements = await injectQIds(page);

  return {
    url,
    title,
    textContent: textContent.substring(0, 50000),
    visibleText: visibleText.substring(0, 50000),
    domSummary,
    interactiveElements,
    viewportSize,
    consoleErrors: [] // Filled by caller if console listener attached
  };
}

module.exports = { extract, injectQIds };
