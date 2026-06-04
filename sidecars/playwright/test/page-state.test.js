// test/page-state.test.js — Unit tests for page state extraction
//
// These test the helper functions, not actual browser interaction.

const { describe, it } = require('node:test');
const assert = require('node:assert/strict');

describe('page-state', () => {
  it('exports extract function', () => {
    const ps = require('../lib/page-state');
    assert.equal(typeof ps.extract, 'function');
  });
});
