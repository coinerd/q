// lib/screenshot.js — Screenshot capture with size limits
//
// Returns: { mimeType, data } where data is base64-encoded

const BASE64_ENCODINGS = {
  png: 'image/png',
  jpeg: 'image/jpeg'
};

async function capture(page, opts = {}) {
  const { selector, fullPage, maxBytes = 524288 } = opts;

  let buffer;
  if (selector) {
    const el = await page.$(selector);
    if (!el) throw Object.assign(new Error(`Selector not found: ${selector}`), { code: 'selector-not-found' });
    buffer = await el.screenshot({ type: 'png' });
  } else {
    buffer = await page.screenshot({ type: 'png', fullPage });
  }

  // Enforce size limit
  if (buffer.length > maxBytes) {
    // Try JPEG at reduced quality
    if (!selector) {
      buffer = await page.screenshot({ type: 'jpeg', fullPage, quality: 60 });
    }
    if (buffer.length > maxBytes) {
      // Scale down — use viewport-only
      buffer = await page.screenshot({ type: 'jpeg', fullPage: false, quality: 40 });
    }
  }

  return {
    mimeType: 'image/png',
    data: buffer.toString('base64')
  };
}

module.exports = { capture };
