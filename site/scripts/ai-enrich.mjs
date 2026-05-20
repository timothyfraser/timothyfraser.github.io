#!/usr/bin/env node
/**
 * ai-enrich.mjs — OPTIONAL, key-gated, graceful AI content pipeline.
 *
 * Run by Tim locally or via the manual GitHub Action. Writes
 * "suggested-*.json" / "blurbs.json" under content/derived/. The site does NOT
 * import these directly — they are review surfaces for Tim to pull from before
 * promoting into the canonical content files.
 *
 * Without ANTHROPIC_API_KEY this script exits 0 immediately and the site
 * still builds.
 */

import fs from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, '..');
const DERIVED = path.join(ROOT, 'content', 'derived');
const CACHE = path.join(DERIVED, '.ai-cache');

if (!process.env.ANTHROPIC_API_KEY) {
  console.log('[ai-enrich] No ANTHROPIC_API_KEY — skipping. Site will build from committed derived JSON.');
  process.exit(0);
}

fs.mkdirSync(DERIVED, { recursive: true });
fs.mkdirSync(CACHE, { recursive: true });

const MODEL = process.env.ANTHROPIC_MODEL || 'claude-opus-4-7';
const API_URL = 'https://api.anthropic.com/v1/messages';

const pubs = JSON.parse(fs.readFileSync(path.join(DERIVED, 'publications.json'), 'utf8'));

async function callClaude(system, user) {
  const key = createHash('sha256').update(MODEL + '||' + system + '||' + user).digest('hex');
  const cached = path.join(CACHE, key + '.json');
  if (fs.existsSync(cached)) {
    return JSON.parse(fs.readFileSync(cached, 'utf8'));
  }
  const r = await fetch(API_URL, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      'x-api-key': process.env.ANTHROPIC_API_KEY,
      'anthropic-version': '2023-06-01',
    },
    body: JSON.stringify({
      model: MODEL,
      max_tokens: 800,
      system,
      messages: [{ role: 'user', content: user }],
    }),
  });
  if (!r.ok) {
    console.error('[ai-enrich] API error', r.status, await r.text());
    process.exit(1);
  }
  const json = await r.json();
  const text = json?.content?.[0]?.text || '';
  fs.writeFileSync(cached, JSON.stringify({ text }));
  return { text };
}

const TOPICS = [
  'environment', 'transportation', 'disaster', 'social_capital',
  'social_infrastructure', 'networks', 'gis', 'polarization', 'energy', 'health',
];

async function suggestTags() {
  // Only suggest for rows with no topic flags set.
  const targets = pubs.filter(p => !p.topics || p.topics.length === 0);
  if (!targets.length) { console.log('[ai-enrich] suggestTags: nothing to do'); return; }

  const system = 'You classify academic publications by research topic. Reply only with a JSON object: {"topics": ["topic1", ...]}. Topics must be from this list: ' + TOPICS.join(', ') + '. If unsure, return an empty array.';
  const out = {};
  for (const p of targets) {
    const user = `Title: ${p.title}\nJournal: ${p.journal || ''}\nAuthors: ${p.authors.join(', ')}\nDescription: ${p.description || ''}`;
    try {
      const { text } = await callClaude(system, user);
      const m = text.match(/\{[\s\S]*\}/);
      if (!m) continue;
      const obj = JSON.parse(m[0]);
      out[p.id] = obj.topics || [];
    } catch (e) {
      console.warn('[ai-enrich] parse fail for', p.id, e.message);
    }
  }
  fs.writeFileSync(path.join(DERIVED, 'suggested-tags.json'), JSON.stringify(out, null, 2));
  console.log('[ai-enrich] wrote suggested-tags.json with', Object.keys(out).length, 'entries');
}

async function suggestBlurbs() {
  // Lay-language one-sentence summaries for featured (recent) papers.
  const recent = pubs
    .filter(p => p.type === 'paper' && p.year && p.year >= 2023)
    .slice(0, 12);
  const system = 'Write a single-sentence plain-language summary (<=28 words) of an academic paper. Reply with only the sentence — no quotes, no preamble.';
  const out = {};
  for (const p of recent) {
    const user = `Title: ${p.title}\nJournal: ${p.journal || ''}\nAuthors: ${p.authors.join(', ')}`;
    try {
      const { text } = await callClaude(system, user);
      out[p.id] = text.trim();
    } catch (e) {
      console.warn('[ai-enrich] blurb fail for', p.id, e.message);
    }
  }
  fs.writeFileSync(path.join(DERIVED, 'blurbs.json'), JSON.stringify(out, null, 2));
  console.log('[ai-enrich] wrote blurbs.json with', Object.keys(out).length, 'entries');
}

console.log('[ai-enrich] starting, model =', MODEL);
await suggestTags();
await suggestBlurbs();
console.log('[ai-enrich] done. Review suggestions under content/derived/ before promoting.');
