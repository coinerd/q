#!/usr/bin/env node
// q-playwright-sidecar.js — JSONL stdin/stdout Playwright server
//
// Protocol:
//   Request:  {"id":"uuid","type":"command","params":{...}}
//   Response: {"id":"uuid","type":"command","success":true,"data":{...}}
//   Error:    {"id":"uuid","type":"command","success":false,"error":{"code":"...","message":"..."}}
//
// Commands: navigate, click, type, press, scroll, extract, screenshot, ping, close
//
// Graceful shutdown on stdin EOF.

const readline = require('readline');
const { chromium } = require('playwright');
const commands = require('./lib/commands');
const pageState = require('./lib/page-state');
const screenshot = require('./lib/screenshot');

let browser = null;
const pages = new Map(); // sessionId -> { page, url }

function parseHeadlessArg(argv) {
  const arg = argv.find(a => a.startsWith('--headless='));
  if (!arg) return true;
  const value = arg.slice('--headless='.length).toLowerCase();
  return !(value === 'false' || value === '0' || value === 'no');
}

const headless = parseHeadlessArg(process.argv.slice(2));

// ── Lifecycle ────────────────────────────────────────────────

async function ensureBrowser() {
  if (!browser || !browser.isConnected()) {
    browser = await chromium.launch({ headless });
  }
  return browser;
}

async function shutdown() {
  if (browser) {
    try { await browser.close(); } catch (_) {}
    browser = null;
  }
  pages.clear();
}

// ── Command dispatch ─────────────────────────────────────────

const HANDLERS = {
  ping: async (params) => ({
    browsersOpen: pages.size,
    headless,
    status: 'ok'
  }),

  navigate: async (params) => {
    const { url, sessionId, viewportWidth, viewportHeight } = params;
    const b = await ensureBrowser();
    const context = await b.newContext({
      viewport: { width: viewportWidth || 1280, height: viewportHeight || 720 }
    });
    const page = await context.newPage();

    // Collect console errors
    const consoleErrors = [];
    page.on('console', msg => {
      if (msg.type() === 'error') consoleErrors.push(msg.text());
    });

    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 30000 });
    const state = await pageState.extract(page);
    const id = sessionId || crypto.randomUUID();
    pages.set(id, { page, url });
    return { sessionId: id, ...state, consoleErrors };
  },

  click: async (params) => {
    const { sessionId, selector, button } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    await page.click(selector, { button: button || 'left', timeout: 10000 });
    const state = await pageState.extract(page);
    return state;
  },

  type: async (params) => {
    const { sessionId, selector, text, clearFirst } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    if (clearFirst) await page.fill(selector, '');
    await page.type(selector, text, { timeout: 10000 });
    const state = await pageState.extract(page);
    return state;
  },

  press: async (params) => {
    const { sessionId, key } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    await page.keyboard.press(key);
    const state = await pageState.extract(page);
    return state;
  },

  scroll: async (params) => {
    const { sessionId, direction, amount } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    const delta = (amount || 3) * 720;
    await page.mouse.wheel(0, direction === 'up' ? -delta : delta);
    const state = await pageState.extract(page);
    return state;
  },

  extract: async (params) => {
    const { sessionId, selector, extractType } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    if (selector) {
      const el = await page.$(selector);
      if (!el) throw commandError('selector-not-found', `Selector ${selector} not found`);
      const typ = extractType || 'text';
      if (typ === 'html') {
        return { html: await el.innerHTML() };
      } else if (typ === 'accessibility') {
        return { accessibility: JSON.stringify(await el.evaluate(el => {
          // Simple accessibility snapshot
          return { tag: el.tagName, role: el.getAttribute('role'), text: el.textContent };
        })) };
      }
      return { text: await el.textContent() };
    }
    const state = await pageState.extract(page);
    return state;
  },

  screenshot: async (params) => {
    const { sessionId, selector, fullPage, maxBytes } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    const page = entry.page;
    return await screenshot.capture(page, { selector, fullPage: !!fullPage, maxBytes: maxBytes || 524288 });
  },

  close: async (params) => {
    const { sessionId } = params;
    const entry = pages.get(sessionId);
    if (!entry) throw commandError('session-not-found', `Session ${sessionId} not found`);
    await entry.page.context().close();
    pages.delete(sessionId);
    return { sessionId, closed: true };
  }
};

function commandError(code, message) {
  const err = new Error(message);
  err.code = code;
  return err;
}

// ── JSONL protocol ───────────────────────────────────────────

async function handleRequest(req) {
  const { id, type, params } = req;
  try {
    const handler = HANDLERS[type];
    if (!handler) {
      respond(id, type, false, null, { code: 'unknown-command', message: `Unknown command: ${type}` });
      return;
    }
    const data = await handler(params || {});
    respond(id, type, true, data);
  } catch (err) {
    const code = err.code || mapErrorCode(err);
    respond(id, type, false, null, { code, message: err.message });
  }
}

function respond(id, type, success, data, error) {
  const resp = { id, type, success };
  if (success) resp.data = data;
  else resp.error = error;
  process.stdout.write(JSON.stringify(resp) + '\n');
}

// Map Playwright error types to q error codes
function mapErrorCode(err) {
  const msg = (err.message || '').toLowerCase();
  if (msg.includes('timeout') || err.name === 'TimeoutError') return 'timeout';
  if (msg.includes('net::err') || msg.includes('navigation')) return 'adapter-error';
  if (msg.includes('waiting for selector') || msg.includes('no element')) return 'selector-error';
  if (msg.includes('connection') || msg.includes('disconnected')) return 'sidecar-crash';
  if (msg.includes('permission')) return 'policy-violation';
  return 'adapter-error';
}

// ── Main loop ────────────────────────────────────────────────

const rl = readline.createInterface({ input: process.stdin });

rl.on('line', async (line) => {
  try {
    const req = JSON.parse(line.trim());
    if (req.id && req.type) {
      await handleRequest(req);
    }
  } catch (e) {
    // Malformed input — skip
  }
});

rl.on('close', async () => {
  await shutdown();
  process.exit(0);
});

// Handle process signals
process.on('SIGTERM', async () => { await shutdown(); process.exit(0); });
process.on('SIGINT', async () => { await shutdown(); process.exit(0); });

// Signal ready
process.stderr.write('q-playwright-sidecar ready\n');
