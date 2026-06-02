/** Shared data loader for OD viz widgets (publications shelf, conveyor, press ribbon, scholar pulse). */

const VIZ_ROOT = new URL('../', import.meta.url);

export const DATA = {
  publications: new URL('data/publications.csv', VIZ_ROOT).href,
  software: new URL('data/software.csv', VIZ_ROOT).href,
  press: new URL('data/press.csv', VIZ_ROOT).href,
  metrics: new URL('data/metrics.json', VIZ_ROOT).href,
};

export const TOPIC_KEYS = [
  ['topic_environment', 'Environment', 'topic-environment'],
  ['topic_transportation', 'Transportation', 'topic-transportation'],
  ['topic_disaster', 'Disaster', 'topic-disaster'],
  ['topic_social_capital', 'Social capital', 'topic-social-capital'],
  ['topic_social_infrastructure', 'Social infrastructure', 'topic-social-infrastructure'],
  ['topic_networks', 'Networks', 'topic-networks'],
  ['topic_gis', 'GIS', 'topic-gis'],
  ['topic_polarization', 'Polarization', 'topic-polarization'],
  ['topic_energy', 'Energy', 'topic-energy'],
  ['topic_health', 'Health', 'topic-health'],
];

const TOPIC_LABEL = Object.fromEntries(TOPIC_KEYS.map(([key, label, token]) => [key, { label, token }]));

export function displayField(value) {
  if (value == null) return '-';
  const s = String(value).trim();
  if (!s || s.toUpperCase() === 'NA' || s.toUpperCase() === 'N/A') return '-';
  return s;
}

export function isTrue(value) {
  if (value === true || value === 1) return true;
  const s = String(value ?? '').trim().toUpperCase();
  return s === 'TRUE' || s === 'T' || s === '1' || s === 'YES';
}

export function topicsForPub(pub) {
  return TOPIC_KEYS.filter(([key]) => isTrue(pub[key])).map(([key, label, token]) => ({
    key: token,
    label,
    column: key,
  }));
}

export function primaryTopic(pub) {
  const topics = topicsForPub(pub);
  if (topics.length) return topics[0];
  return { key: 'topic-default', label: 'Other', column: null };
}

export function initEmbedShell() {
  const embed = new URLSearchParams(window.location.search).get('embed') === '1';
  if (!embed) return;
  document.documentElement.classList.add('od-embed');
  const style = document.createElement('style');
  style.textContent = `
    .od-embed .stripe { display: none; }
    .od-embed header,
    .od-embed .head { display: none; }
    .od-embed .wrap { padding-top: 12px; padding-bottom: 24px; }
    html.od-embed, html.od-embed body {
      min-height: 0 !important;
      height: auto !important;
      overflow: visible;
    }
  `;
  document.head.appendChild(style);
}

/** Tell parent iframe to resize (embed mode only). */
export function reportEmbedHeight() {
  if (new URLSearchParams(window.location.search).get('embed') !== '1') return;
  if (window.parent === window) return;
  requestAnimationFrame(() => {
    const height = Math.ceil(
      Math.max(
        document.documentElement.scrollHeight,
        document.body.scrollHeight,
        document.documentElement.getBoundingClientRect().height,
      ),
    );
    window.parent.postMessage({ type: 'od-viz-resize', height }, '*');
  });
}

export async function loadText(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${res.statusText} — ${url}`);
  return res.text();
}

export async function loadJSON(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${res.statusText} — ${url}`);
  return res.json();
}

function parseCsvLine(line) {
  const out = [];
  let cur = '';
  let inQuotes = false;
  for (let i = 0; i < line.length; i += 1) {
    const ch = line[i];
    if (inQuotes) {
      if (ch === '"') {
        if (line[i + 1] === '"') {
          cur += '"';
          i += 1;
        } else {
          inQuotes = false;
        }
      } else {
        cur += ch;
      }
    } else if (ch === '"') {
      inQuotes = true;
    } else if (ch === ',') {
      out.push(cur);
      cur = '';
    } else {
      cur += ch;
    }
  }
  out.push(cur);
  return out;
}

export function csvToObjects(text) {
  const lines = text.replace(/\r\n/g, '\n').replace(/\r/g, '\n').split('\n').filter(Boolean);
  if (!lines.length) return [];
  const headers = parseCsvLine(lines[0]);
  return lines.slice(1).map((line) => {
    const cells = parseCsvLine(line);
    const row = {};
    headers.forEach((h, i) => {
      row[h] = cells[i] ?? '';
    });
    return row;
  });
}

export { TOPIC_LABEL };
