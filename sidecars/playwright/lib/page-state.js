// lib/page-state.js — Extract page state for observation
//
// Returns: { url, title, textContent, visibleText, domSummary,
//            accessibilityTree, consoleErrors, viewportSize }

async function extract(page) {
  const [url, title, textContent, visibleText, domSummary, viewportSize] = await Promise.all([
    page.url(),
    page.title(),
    page.evaluate(() => document.body?.textContent || ''),
    page.evaluate(() => {
      // Get visible text only
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
      const tags = {};
      document.querySelectorAll('*').forEach(el => {
        const t = el.tagName.toLowerCase();
        tags[t] = (tags[t] || 0) + 1;
      });
      return { tagCounts: tags, childCount: document.body?.children.length || 0 };
    }),
    page.viewportSize()
  ]);

  return {
    url,
    title,
    textContent: textContent.substring(0, 50000),
    visibleText: visibleText.substring(0, 50000),
    domSummary,
    viewportSize,
    consoleErrors: [] // Filled by caller if console listener attached
  };
}

module.exports = { extract };
