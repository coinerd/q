// test/screenshot.test.js — Unit tests for screenshot module

const { describe, it } = require('node:test');
const assert = require('node:assert/strict');

describe('screenshot', () => {
  it('exports capture function', () => {
    const ss = require('../lib/screenshot');
    assert.equal(typeof ss.capture, 'function');
  });
});
