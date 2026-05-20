import { useRef, useState } from 'react';
import { researchSites } from '../data/loaders';
import type { ResearchSite } from '../data/types';

/**
 * Lightweight stylized map: two equirectangular insets, one for the Americas
 * and one for East Asia. No tile / topojson dependency — we draw a paper-
 * colored panel with country-less coordinate framing and plot points.
 *
 * The visual goal is editorial, not GIS-accurate. A more cartographically
 * faithful version can swap in d3-geo + topojson without changing the API.
 */

const TOPIC_COLOR: Record<string, string> = {
  environment: 'var(--topic-environment)',
  transportation: 'var(--topic-transportation)',
  disaster: 'var(--topic-disaster)',
  resilience: 'var(--topic-resilience)',
  social_infrastructure: 'var(--topic-resilience)',
  social_capital: 'var(--topic-resilience)',
  polarization: 'var(--topic-polarization)',
  energy: 'var(--topic-energy)',
  health: 'var(--topic-health)',
  networks: 'var(--topic-networks)',
  home: 'var(--cornell)',
};

interface PanelDef {
  label: string;
  // lon/lat bbox: [west, south, east, north]
  bbox: [number, number, number, number];
}

const PANELS: PanelDef[] = [
  { label: 'Americas', bbox: [-130, -45, -50, 55] },
  { label: 'East Asia · Japan', bbox: [110, 20, 150, 50] },
  { label: 'Other', bbox: [-30, -40, 60, 60] }, // Europe / Africa for South Africa, Paraguay outliers handled here
];

function classify(site: ResearchSite): number {
  if (site.lng < -30) return 0;
  if (site.lng > 100) return 1;
  return 2;
}

export default function ResearchMap({ height = 340 }: { height?: number }) {
  const [hover, setHover] = useState<{ site: ResearchSite; px: number; py: number; panel: number } | null>(null);
  const wrapRef = useRef<HTMLDivElement | null>(null);

  // Distribute panels by site count (drop empty)
  const counts = [0, 0, 0];
  for (const s of researchSites) counts[classify(s)]++;
  const activePanels = PANELS.map((p, i) => ({ ...p, idx: i, n: counts[i] })).filter(p => p.n > 0);

  return (
    <div className="research-map" ref={wrapRef} style={{ position: 'relative' }}>
      <div className="kicker" style={{ marginBottom: 12 }}>Research sites · {researchSites.length} locations</div>
      <div style={{ display: 'grid', gridTemplateColumns: `repeat(${activePanels.length}, minmax(0,1fr))`, gap: 12 }}>
        {activePanels.map(p => (
          <Panel
            key={p.idx}
            def={p}
            height={height}
            onHover={(site, px, py) => setHover(site ? { site, px, py, panel: p.idx } : null)}
          />
        ))}
      </div>

      {hover && (
        <div
          className="map-tooltip"
          style={{ left: hover.px, top: hover.py, position: 'absolute' }}
        >
          <strong>{hover.site.label}</strong>
          {hover.site.year ? <span> · {hover.site.year}</span> : null}
          <br />
          <span style={{ opacity: 0.8 }}>{hover.site.blurb}</span>
        </div>
      )}

      <details className="viz-fallback" style={{ marginTop: 14, border: '1px dashed var(--line)', padding: '12px 14px', borderRadius: 3 }}>
        <summary>Research sites (text)</summary>
        <ul style={{ margin: '10px 0 4px 18px' }}>
          {researchSites.map((s, i) => (
            <li key={i}><strong>{s.label}</strong> — {s.blurb}{s.year ? ` (${s.year})` : ''}</li>
          ))}
        </ul>
      </details>
    </div>
  );
}

interface PanelProps {
  def: PanelDef & { idx: number };
  height: number;
  onHover: (site: ResearchSite | null, px: number, py: number) => void;
}

function Panel({ def, height, onHover }: PanelProps) {
  const [w, s, e, n] = def.bbox;
  const width = 320;
  const aspect = (e - w) / (n - s);
  const useHeight = Math.min(height, width / aspect);
  const project = (lng: number, lat: number) => {
    const x = ((lng - w) / (e - w)) * width;
    const y = useHeight - ((lat - s) / (n - s)) * useHeight;
    return { x, y };
  };
  const ref = useRef<SVGSVGElement | null>(null);

  const sites = researchSites.filter(site => classify(site) === def.idx);

  return (
    <div style={{ position: 'relative' }}>
      <svg
        ref={ref}
        viewBox={`0 0 ${width} ${useHeight}`}
        preserveAspectRatio="xMidYMid meet"
        style={{ width: '100%', height: 'auto', background: 'var(--paper-3)', borderRadius: 3, border: '1px solid var(--line)' }}
        aria-label={`${def.label} research sites`}
        role="img"
      >
        {/* graticule */}
        {Array.from({ length: 6 }).map((_, i) => {
          const y = (useHeight / 6) * i;
          return <line key={`h${i}`} x1={0} y1={y} x2={width} y2={y} stroke="var(--line)" strokeOpacity={0.5} strokeDasharray="2 4" />;
        })}
        {Array.from({ length: 6 }).map((_, i) => {
          const x = (width / 6) * i;
          return <line key={`v${i}`} x1={x} y1={0} x2={x} y2={useHeight} stroke="var(--line)" strokeOpacity={0.5} strokeDasharray="2 4" />;
        })}

        {/* sites */}
        {sites.map((site, i) => {
          const { x, y } = project(site.lng, site.lat);
          return (
            <g key={i}>
              <circle
                cx={x}
                cy={y}
                r={site.topic === 'home' ? 7 : 5}
                fill={TOPIC_COLOR[site.topic] || 'var(--ink)'}
                stroke="var(--paper)"
                strokeWidth={1.4}
                style={{ cursor: 'pointer' }}
                onMouseEnter={(ev) => {
                  const rect = (ev.target as SVGElement).getBoundingClientRect();
                  const parent = (ev.target as SVGElement).ownerSVGElement!.parentElement!.parentElement!.getBoundingClientRect();
                  onHover(site, rect.left - parent.left + rect.width / 2, rect.top - parent.top);
                }}
                onMouseLeave={() => onHover(null, 0, 0)}
              />
            </g>
          );
        })}
      </svg>
      <div style={{ position: 'absolute', top: 6, left: 8, fontFamily: 'var(--font-mono)', fontSize: 9, letterSpacing: '0.16em', textTransform: 'uppercase', color: 'var(--muted)' }}>
        {def.label}
      </div>
    </div>
  );
}
