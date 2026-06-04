// lib/commands.js — Playwright command implementations
//
// Re-exports core command logic for direct use in tests.
// The main dispatch lives in q-playwright-sidecar.js.

// Command type constants for validation
const COMMAND_TYPES = [
  'navigate', 'click', 'type', 'press',
  'scroll', 'extract', 'screenshot', 'ping', 'close'
];

// Required parameters per command type
const REQUIRED_PARAMS = {
  navigate: ['url'],
  click: ['sessionId', 'selector'],
  type: ['sessionId', 'selector', 'text'],
  press: ['sessionId', 'key'],
  scroll: ['sessionId'],
  extract: ['sessionId'],
  screenshot: ['sessionId'],
  close: ['sessionId'],
  ping: []
};

function validateCommand(type, params) {
  if (!COMMAND_TYPES.includes(type)) {
    return { valid: false, error: `Unknown command: ${type}` };
  }
  const required = REQUIRED_PARAMS[type] || [];
  for (const key of required) {
    if (params[key] === undefined || params[key] === null) {
      return { valid: false, error: `Missing required parameter: ${key}` };
    }
  }
  return { valid: true };
}

module.exports = { COMMAND_TYPES, REQUIRED_PARAMS, validateCommand };
