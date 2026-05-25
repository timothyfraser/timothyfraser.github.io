#!/usr/bin/env node
/**
 * build-content.mjs — Deterministic content pipeline.
 *
 * Reads CSV/JSON/MD files in `content/`, validates them, and writes
 * normalized JSON to `content/derived/`. NO API keys, NO network. Always
 * runs as `prebuild`.
 *
 * Outputs:
 *   content/derived/publications.json    — normalized + topic-tagged
 *   content/derived/press.json           — sorted desc + monthly buckets
 *   content/derived/coauthor-graph.json  — { nodes, edges, layout }
 *   content/derived/metrics-computed.json
 *   content/derived/build-info.json
 */

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import Papa from 'papaparse';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT = path.resolve(__dirname, '..');
const CONTENT = path.join(ROOT, 'content');
const DERIVED = path.join(CONTENT, 'derived');

fs.mkdirSync(DERIVED, { recursive: true });

const TIM = 'Timothy Fraser';
const TIM_KEYS = new Set([
  'timothy fraser', 'tim fraser', 't. fraser', 'fraser, timothy', 'fraser, t.', 'fraser',
]);

const TOPIC_KEYS = [
  ['environment', 'topic_environment'],
  ['transportation', 'topic_transportation'],
  ['disaster', 'topic_disaster'],
  ['resilience', 'topic_social_capital'],
  ['social_infrastructure', 'topic_social_infrastructure'],
  ['networks', 'topic_networks'],
  ['gis', 'topic_gis'],
  ['polarization', 'topic_polarization'],
  ['energy', 'topic_energy'],
  ['health', 'topic_health'],
];

const TOPIC_GROUPS = {
  emissions_env: ['topic_environment', 'topic_transportation', 'topic_energy'],
  resilience_health: ['topic_disaster', 'topic_social_capital', 'topic_social_infrastructure', 'topic_health'],
  polarization: ['topic_polarization'],
  networks_gis: ['topic_networks', 'topic_gis'],
};

const log = (...a) => console.log('[content]', ...a);
const die = (msg) => { console.error('[content] FATAL:', msg); process.exit(1); };

function readCsv(rel) {
  const file = path.join(CONTENT, rel);
  if (!fs.existsSync(file)) die(`Missing CSV: ${rel}`);
  const text = fs.readFileSync(file, 'utf8');
  const parsed = Papa.parse(text, { header: true, skipEmptyLines: true, dynamicTyping: false });
  if (parsed.errors.length) {
    for (const e of parsed.errors) console.warn('[content] CSV warn:', rel, e.message, 'row', e.row);
  }
  return parsed.data;
}

function readJson(rel, fallback) {
  const file = path.join(CONTENT, rel);
  if (!fs.existsSync(file)) {
    if (fallback !== undefined) return fallback;
    die(`Missing JSON: ${rel}`);
  }
  return JSON.parse(fs.readFileSync(file, 'utf8'));
}

function writeJson(rel, data) {
  const file = path.join(DERIVED, rel);
  fs.writeFileSync(file, JSON.stringify(data, null, 2));
  log('wrote', path.relative(ROOT, file));
}

// -------- author parsing --------
function normName(n) {
  return n.replace(/\s+/g, ' ').replace(/\.$/, '').trim();
}
// Collaborators who appear under several name forms in the CV are collapsed
// to one identity so the coauthor graph counts them once. Keyed by the
// lowercased, punctuation-stripped form authorKey() produces.
const AUTHOR_ALIASES = {
  'chapman': 'andrew j chapman',
  'andrew chapman': 'andrew j chapman',
};
const CANONICAL_NAME = {
  'andrew j chapman': 'Andrew J. Chapman',
};
function authorKey(n) {
  const base = normName(n).toLowerCase()
    .replace(/[.]/g, '')
    .replace(/\bdr\b\s*/g, '')
    .trim();
  return AUTHOR_ALIASES[base] || base;
}
function splitAuthors(raw) {
  if (!raw) return [];
  return raw
    // Fix inverted "Last, First" forms before the comma split treats the
    // comma as an author separator (otherwise "Chapman, Andrew" becomes two).
    .replace(/Chapman,\s*Andrew/gi, 'Andrew Chapman')
    .replace(/&/g, ',')
    .replace(/\band\b/gi, ',')
    .split(',')
    .map(s => normName(s))
    .filter(Boolean);
}
function isTim(name) {
  return TIM_KEYS.has(authorKey(name));
}

// -------- topic helpers --------
function parseBool(s) {
  if (s === true) return true;
  if (typeof s !== 'string') return false;
  return s.trim().toUpperCase() === 'TRUE';
}
function topicsOf(row) {
  const out = [];
  for (const [name, key] of TOPIC_KEYS) {
    if (parseBool(row[key])) out.push(name);
  }
  return out;
}

// Keyword-based topic inference from a publication's title (+ journal /
// description). Augments the hand-maintained topic_* flags so the topic
// distribution reflects what the titles actually say — e.g. transportation,
// energy, and health surface even where a flag was never set.
const TITLE_KEYWORDS = {
  transportation: [/transport/, /\btransit\b/, /congestion/, /vehicle/, /traffic/, /mobilit/, /commut/, /\bbus(es)?\b/, /bicycl/, /\bcycling\b/, /\bbike/, /pedestrian/, /\broads?\b/, /highway/, /\bcars?\b/, /driving/, /electric vehicle/, /\bmoves\b/],
  environment: [/environment/, /\bclimate/, /emission/, /\bcarbon\b/, /pollut/, /air quality/, /\bpm2\.?5\b/, /sustainab/, /greenhouse/, /\bco2\b/, /decarboni/],
  energy: [/\benerg/, /\bsolar\b/, /renewable/, /\bgrid\b/, /nuclear/, /photovoltaic/, /electricity/, /wind power/],
  disaster: [/disaster/, /hurricane/, /earthquake/, /\bflood/, /tsunami/, /\brecovery\b/, /\bhazard/, /evacuat/, /wildfire/, /typhoon/, /catastroph/, /katrina/, /fukushima/],
  resilience: [/resilien/, /social capital/, /\bcivic\b/, /communit/, /cohesion/, /collective action/, /\bvolunteer/, /neighbor/],
  social_infrastructure: [/social infrastructure/, /\blibrar/, /\bparks?\b/, /third place/, /public space/],
  health: [/\bhealth\b/, /mortality/, /\bcovid/, /pandemic/, /\bmental\b/, /well-?being/, /epidemi/, /disease/, /\bmedical\b/],
  polarization: [/polari[sz]/, /partisan/, /ideolog/, /\baffective\b/, /political divi/],
  networks: [/\bnetwork/, /coauthor/, /centrality/, /\bgraph\b/, /social ties/],
  gis: [/\bspatial\b/, /\bmapping\b/, /geographic/, /\bgis\b/, /geospatial/, /\bcensus\b/],
};
function topicsFromText(text) {
  if (!text) return [];
  const hay = text.toLowerCase();
  const out = [];
  for (const [topic, patterns] of Object.entries(TITLE_KEYWORDS)) {
    if (patterns.some(re => re.test(hay))) out.push(topic);
  }
  return out;
}
function dominantTopic(topics) {
  // Priority order — earlier = visual primacy on the network
  const order = ['transportation', 'environment', 'disaster', 'resilience', 'energy', 'health', 'polarization', 'networks', 'gis', 'social_infrastructure'];
  for (const t of order) if (topics.includes(t)) return t;
  return 'other';
}

// -------- normalize publications --------
function normalizePublications(rows) {
  return rows
    .filter(r => r.title && r.title.trim())
    .map((r, i) => {
      const authors = splitAuthors(r.authors);
      const flagged = topicsOf(r);
      const inferred = topicsFromText([r.title, r.journal, r.description].filter(Boolean).join(' . '));
      const topics = Array.from(new Set([...flagged, ...inferred]));
      return {
        id: `pub-${i}-${(r.title || '').slice(0, 40).replace(/\W+/g, '-').toLowerCase()}`,
        title: r.title.trim(),
        year: parseInt(r.year, 10) || null,
        journal: (r.journal || '').trim() || null,
        authors,
        doi: (r.DOI || '').trim() || null,
        link: (r.link || '').trim() || null,
        description: (r.description || '').trim() || null,
        type: (r.type || 'paper').trim().toLowerCase(),
        topics,
        dominant: dominantTopic(topics),
        volume: (r.volume || '').trim() || null,
        issue: (r.issue || '').trim() || null,
      };
    });
}

// -------- build coauthor graph --------
function buildCoauthorGraph(pubs) {
  // Only papers with Tim as author
  const tim = pubs.filter(p => p.authors.some(isTim));
  const nodeMap = new Map(); // key -> {id,label,weight,topics:Set}
  const edgeMap = new Map(); // "a||b" sorted -> {a,b,weight,topics}

  const addNode = (name, topic) => {
    const key = authorKey(name);
    if (!key) return null;
    if (!nodeMap.has(key)) {
      nodeMap.set(key, {
        id: key,
        label: isTim(name) ? TIM : (CANONICAL_NAME[key] || normName(name)),
        weight: 0,
        topics: new Set(),
        isCenter: isTim(name),
      });
    }
    const n = nodeMap.get(key);
    n.weight += 1;
    if (topic) n.topics.add(topic);
    return n.id;
  };
  const addEdge = (a, b, topic) => {
    if (!a || !b || a === b) return;
    const [x, y] = [a, b].sort();
    const key = `${x}||${y}`;
    if (!edgeMap.has(key)) edgeMap.set(key, { source: x, target: y, weight: 0, topics: new Set() });
    const e = edgeMap.get(key);
    e.weight += 1;
    if (topic) e.topics.add(topic);
  };

  for (const p of tim) {
    const topic = p.dominant;
    const ids = p.authors.map(a => addNode(a, topic)).filter(Boolean);
    // Edge every pair on a paper (keeps the graph dense around Tim)
    for (let i = 0; i < ids.length; i++) {
      for (let j = i + 1; j < ids.length; j++) addEdge(ids[i], ids[j], topic);
    }
  }

  const nodes = Array.from(nodeMap.values()).map(n => ({
    id: n.id,
    label: n.label,
    weight: n.weight,
    topic: dominantTopic(Array.from(n.topics)),
    isCenter: n.isCenter,
  }));
  const edges = Array.from(edgeMap.values()).map(e => ({
    source: e.source, target: e.target, weight: e.weight,
    topic: dominantTopic(Array.from(e.topics)),
  }));

  // Precomputed static layout (deterministic) for reduced-motion fallback.
  // Simple radial layout: center Tim, place others around by weight (deterministic seed).
  const center = nodes.find(n => n.isCenter);
  const others = nodes.filter(n => !n.isCenter);
  others.sort((a, b) => b.weight - a.weight);
  const layout = {};
  if (center) layout[center.id] = { x: 0, y: 0 };
  // Concentric rings by weight bracket
  const rings = [
    { r: 110, max: 6 },
    { r: 200, max: 14 },
    { r: 300, max: 30 },
    { r: 400, max: Infinity },
  ];
  let cursor = 0;
  for (const ring of rings) {
    const inRing = others.slice(cursor, cursor + ring.max);
    const n = inRing.length;
    inRing.forEach((node, i) => {
      const angle = (i / Math.max(n, 1)) * Math.PI * 2;
      layout[node.id] = { x: Math.cos(angle) * ring.r, y: Math.sin(angle) * ring.r };
    });
    cursor += ring.max;
    if (cursor >= others.length) break;
  }

  return { nodes, edges, layout };
}

// -------- press --------
function normalizePress(rows) {
  return rows
    .filter(r => r.title && r.title.trim())
    .map(r => {
      const yearN = parseInt(r.year, 10) || null;
      let parsedDate = null;
      if (r.date) {
        const d = new Date(r.date);
        if (!isNaN(d.getTime())) parsedDate = d.toISOString().slice(0, 10);
      }
      if (!parsedDate && yearN) parsedDate = `${yearN}-01-01`;
      return {
        type: (r.type || '').trim(),
        press_author: (r.press_author || '').trim() || null,
        title: r.title.trim(),
        outlet: (r.outlet || '').trim() || null,
        media_type: (r.media_type || '').trim() || null,
        link: (r.link || '').trim() || null,
        date_raw: (r.date || '').trim() || null,
        date: parsedDate,
        year: yearN,
      };
    })
    .sort((a, b) => (b.date || '').localeCompare(a.date || ''));
}

function pressBuckets(press) {
  const monthly = {};
  for (const p of press) {
    if (!p.date) continue;
    const key = p.date.slice(0, 7);
    monthly[key] = (monthly[key] || 0) + 1;
  }
  return monthly;
}

// -------- metrics --------
function computeMetrics(pubs, press, manual) {
  const byType = {};
  for (const p of pubs) byType[p.type] = (byType[p.type] || 0) + 1;
  const topicCounts = {};
  for (const [grp, keys] of Object.entries(TOPIC_GROUPS)) {
    topicCounts[grp] = pubs.filter(p => keys.some(k => p.topics.includes(k.replace(/^topic_/, '')))).length;
  }
  // Coauthors = unique authors across all pubs minus Tim
  const coauthors = new Set();
  for (const p of pubs) for (const a of p.authors) if (!isTim(a)) coauthors.add(authorKey(a));

  // Software / dashboards
  const software = pubs.filter(p => p.type === 'software').length;
  const peerReviewed = pubs.filter(p => p.type === 'paper').length;
  const chapters = pubs.filter(p => p.type === 'chapter').length;

  // Press
  const pressByType = {};
  for (const p of press) pressByType[p.type] = (pressByType[p.type] || 0) + 1;

  // Press in the last 12 months — count items with parsed date in window.
  // For determinism: use the most recent press date as the "now" anchor
  // rather than the wall clock, so the build is reproducible.
  let pressLast12 = 0;
  let pressByYear = {};
  if (press.length) {
    const maxDate = press
      .map(p => p.date)
      .filter(Boolean)
      .sort()
      .pop();
    const anchor = maxDate ? new Date(maxDate) : new Date();
    const cutoff = new Date(anchor);
    cutoff.setFullYear(cutoff.getFullYear() - 1);
    for (const p of press) {
      if (!p.date) continue;
      if (new Date(p.date) >= cutoff) pressLast12 += 1;
      const y = p.year;
      if (y) pressByYear[y] = (pressByYear[y] || 0) + 1;
    }
  }

  return {
    citations: manual.citations ?? null,
    h_index: manual.h_index ?? null,
    i10_index: manual.i10_index ?? null,
    as_of: manual.as_of ?? null,
    peer_reviewed: peerReviewed,
    chapters,
    software,
    coauthors: coauthors.size,
    press_total: press.length,
    press_last_12mo: pressLast12,
    press_by_year: pressByYear,
    press_by_type: pressByType,
    by_type: byType,
    topics: topicCounts,
  };
}

// -------- main --------
function main() {
  log('build start');
  const pubsRaw = readCsv('publications.csv');
  const pressRaw = readCsv('press.csv');
  const metricsManual = readJson('metrics.json', {});

  const pubs = normalizePublications(pubsRaw);
  log(`publications: ${pubs.length} rows`);
  if (!pubs.length) die('No publications parsed.');

  const graph = buildCoauthorGraph(pubs);
  log(`coauthor graph: ${graph.nodes.length} nodes, ${graph.edges.length} edges`);

  const press = normalizePress(pressRaw);
  log(`press: ${press.length} rows`);
  const buckets = pressBuckets(press);

  const metrics = computeMetrics(pubs, press, metricsManual);

  writeJson('publications.json', pubs);
  writeJson('coauthor-graph.json', graph);
  writeJson('press.json', { items: press, monthly: buckets });
  writeJson('metrics-computed.json', metrics);
  // Deterministic build-info — no per-run timestamp so the working tree
  // doesn't get dirtied on every dev/build. The footer surfaces
  // metrics.as_of (Tim-maintained) as the "last updated" signal instead.
  writeJson('build-info.json', {
    counts: {
      publications: pubs.length,
      press: press.length,
      coauthors: graph.nodes.length - 1,
    },
  });

  log('build OK');
}

main();
