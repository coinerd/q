// test/commands.test.js — Unit tests for command validation

const { describe, it } = require('node:test');
const assert = require('node:assert/strict');
const { validateCommand, COMMAND_TYPES } = require('../lib/commands');

describe('commands', () => {
  it('lists all 9 command types', () => {
    assert.equal(COMMAND_TYPES.length, 9);
    assert.ok(COMMAND_TYPES.includes('navigate'));
    assert.ok(COMMAND_TYPES.includes('ping'));
    assert.ok(COMMAND_TYPES.includes('close'));
  });

  it('validates navigate requires url', () => {
    assert.deepStrictEqual(validateCommand('navigate', {}), { valid: false, error: 'Missing required parameter: url' });
    assert.deepStrictEqual(validateCommand('navigate', { url: 'https://example.com' }), { valid: true });
  });

  it('validates click requires sessionId and selector', () => {
    assert.deepStrictEqual(validateCommand('click', {}), { valid: false, error: 'Missing required parameter: sessionId' });
    assert.deepStrictEqual(validateCommand('click', { sessionId: 'abc' }), { valid: false, error: 'Missing required parameter: selector' });
    assert.deepStrictEqual(validateCommand('click', { sessionId: 'abc', selector: '#btn' }), { valid: true });
  });

  it('validates type requires sessionId, selector, text', () => {
    assert.deepStrictEqual(validateCommand('type', { sessionId: 's', selector: '#i' }), { valid: false, error: 'Missing required parameter: text' });
    assert.deepStrictEqual(validateCommand('type', { sessionId: 's', selector: '#i', text: 'hi' }), { valid: true });
  });

  it('validates press requires sessionId and key', () => {
    assert.deepStrictEqual(validateCommand('press', { sessionId: 's' }), { valid: false, error: 'Missing required parameter: key' });
    assert.deepStrictEqual(validateCommand('press', { sessionId: 's', key: 'Enter' }), { valid: true });
  });

  it('validates ping requires nothing', () => {
    assert.deepStrictEqual(validateCommand('ping', {}), { valid: true });
  });

  it('rejects unknown commands', () => {
    assert.deepStrictEqual(validateCommand('fly', {}), { valid: false, error: 'Unknown command: fly' });
  });
});
