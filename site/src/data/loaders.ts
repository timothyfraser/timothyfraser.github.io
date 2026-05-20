// Static imports of derived JSON + a CSV transform for software/students/sites.
import publicationsJson from '../../content/derived/publications.json';
import pressJson from '../../content/derived/press.json';
import coauthorJson from '../../content/derived/coauthor-graph.json';
import metricsJson from '../../content/derived/metrics-computed.json';
import buildInfo from '../../content/derived/build-info.json';

import siteCfg from '../../content/site.json';
import metricsManual from '../../content/metrics.json';
import projectsJson from '../../content/projects.json';
import teachingJson from '../../content/teaching.json';

// CSV files are loaded as raw text via ?raw and parsed at runtime — tiny cost,
// but keeps the edit path easy (just edit the CSV).
import softwareCsv from '../../content/software.csv?raw';
import studentsCsv from '../../content/students.csv?raw';
import sitesCsv from '../../content/sites.csv?raw';

import homeIntroMd from '../../content/pages/home-intro.md?raw';
import researchMd from '../../content/pages/research.md?raw';
import teachingMd from '../../content/pages/teaching.md?raw';

import type {
  Publication, PressItem, CoauthorGraph, MetricsComputed,
  ProjectItem, TeachingData, SoftwareItem, Student, SiteCfg, ResearchSite,
} from './types';

// --- CSV parser (minimal — handles quoted fields with embedded commas) ---
function parseCsv(text: string): Record<string, string>[] {
  const lines = text.replace(/\r\n/g, '\n').split('\n').filter(l => l.length);
  if (!lines.length) return [];
  const parseLine = (line: string): string[] => {
    const out: string[] = [];
    let cur = '';
    let inQ = false;
    for (let i = 0; i < line.length; i++) {
      const c = line[i];
      if (inQ) {
        if (c === '"' && line[i + 1] === '"') { cur += '"'; i++; }
        else if (c === '"') { inQ = false; }
        else cur += c;
      } else {
        if (c === ',') { out.push(cur); cur = ''; }
        else if (c === '"') { inQ = true; }
        else cur += c;
      }
    }
    out.push(cur);
    return out;
  };
  const headers = parseLine(lines[0]);
  return lines.slice(1).map(l => {
    const cells = parseLine(l);
    const o: Record<string, string> = {};
    headers.forEach((h, i) => o[h] = (cells[i] ?? '').trim());
    return o;
  });
}

export const publications = publicationsJson as Publication[];
export const press = pressJson as { items: PressItem[]; monthly: Record<string, number> };
export const coauthorGraph = coauthorJson as unknown as CoauthorGraph;
export const metricsComputed = metricsJson as MetricsComputed;
export const metrics = { ...metricsManual, ...metricsComputed } as MetricsComputed & typeof metricsManual;
export const buildMeta = buildInfo as { counts: Record<string, number> };

export const site = siteCfg as SiteCfg;
export const projects = projectsJson as ProjectItem[];
export const teaching = teachingJson as TeachingData;

export const software: SoftwareItem[] = parseCsv(softwareCsv).map(r => ({
  id: r.id,
  name: r.name,
  blurb: r.blurb,
  repo_url: r.repo_url || null,
  live_url: r.live_url || null,
  tags: (r.tags || '').split('|').map(s => s.trim()).filter(Boolean),
  year: r.year ? parseInt(r.year, 10) : null,
  status: r.status,
  featured: r.featured === 'true',
}));

export const students: Student[] = parseCsv(studentsCsv).map(r => ({
  name: r.name,
  level: r.level,
  team: r.team,
  institution: r.institution,
  outputs: r.outputs,
  current: r.current === 'true',
}));

export const researchSites: ResearchSite[] = parseCsv(sitesCsv).map(r => ({
  label: r.label,
  lat: parseFloat(r.lat),
  lng: parseFloat(r.lng),
  topic: r.topic,
  blurb: r.blurb,
  year: r.year ? parseInt(r.year, 10) : null,
}));

export const markdownPages = {
  home: homeIntroMd,
  research: researchMd,
  teaching: teachingMd,
};
