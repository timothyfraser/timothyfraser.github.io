import { useMemo, useRef, useState } from 'react';
import { press } from '../data/loaders';
import * as d3 from 'd3';
import type { PressItem } from '../data/types';

const TYPE_COLOR: Record<string, string> = {
  quoted_in: 'var(--signal)',
  cited_in: 'var(--cool)',
  interviewed: 'var(--hot)',
  op_ed: 'var(--cornell)',
  reviewed_in: 'var(--gold)',
};
const TYPE_LABEL: Record<string, string> = {
  quoted_in: 'Quoted in',
  cited_in: 'Cited in',
  interviewed: 'Interviewed on',
  op_ed: 'Op-Ed in',
  reviewed_in: 'Reviewed in',
};

export default function PressTimeline({ height = 160 }: { height?: number }) {
  const wrapRef = useRef<HTMLDivElement | null>(null);
  const [hover, setHover] = useState<{ item: PressItem; x: number; y: number } | null>(null);

  const { items, monthSpan, monthCounts, padX, padY, points, width } = useMemo(() => {
    const items = press.items.filter(p => p.date) as PressItem[];
    if (!items.length) return { items: [], monthSpan: [], monthCounts: {}, padX: 40, padY: 20, points: [], width: 800 };
    const minD = d3.min(items, p => new Date(p.date!))!;
    const maxD = d3.max(items, p => new Date(p.date!))!;
    const padX = 40;
    const padY = 20;
    const width = 900;
    const x = d3.scaleTime()
      .domain([d3.timeMonth.floor(minD), d3.timeMonth.ceil(maxD)])
      .range([padX, width - padX]);

    const counts: Record<string, number> = {};
    for (const p of items) {
      const k = p.date!.slice(0, 7);
      counts[k] = (counts[k] || 0) + 1;
    }
    const maxCount = Math.max(...Object.values(counts));
    const y = d3.scaleLinear().domain([0, maxCount]).range([height - padY, padY]);

    // Stack dots per month
    const stackMap = new Map<string, number>();
    const points = items.map(p => {
      const key = p.date!.slice(0, 7);
      const idx = stackMap.get(key) ?? 0;
      stackMap.set(key, idx + 1);
      const xPos = x(d3.timeMonth.floor(new Date(p.date!)));
      const yPos = y(idx + 1);
      return { item: p, x: xPos, y: yPos };
    });

    const monthSpan = d3.timeYear.range(d3.timeYear.floor(minD), d3.timeYear.ceil(maxD)).concat(d3.timeYear.ceil(maxD));
    return { items, monthSpan, monthCounts: counts, padX, padY, points, width };
  }, [height]);

  if (!items.length) return null;

  const xScale = d3.scaleTime()
    .domain([
      d3.timeMonth.floor(new Date(items[items.length - 1].date!)),
      d3.timeMonth.ceil(new Date(items[0].date!)),
    ])
    .range([40, 860]);

  return (
    <div className="timeline" ref={wrapRef} style={{ position: 'relative' }}>
      <div className="kicker" style={{ marginBottom: 12 }}>Press feed · {items.length} hits</div>
      <svg className="timeline-svg" viewBox={`0 0 ${width} ${height}`} role="img"
           aria-label={`Press coverage timeline showing ${items.length} press mentions over time, colored by type.`}>
        {/* Baseline */}
        <line x1={padX} y1={height - padY} x2={width - padX} y2={height - padY} stroke="var(--line)" />
        {/* Year ticks */}
        {monthSpan.map((d, i) => {
          const xp = xScale(d);
          return (
            <g key={i}>
              <line x1={xp} y1={height - padY} x2={xp} y2={height - padY + 6} stroke="var(--muted)" />
              <text x={xp} y={height - 2} fontSize={10} fontFamily="var(--font-mono)"
                    textAnchor="middle" fill="var(--muted)">
                {d.getFullYear()}
              </text>
            </g>
          );
        })}
        {/* Dots */}
        {points.map((p, i) => (
          <circle
            key={i}
            cx={p.x}
            cy={p.y}
            r={4}
            fill={TYPE_COLOR[p.item.type] || 'var(--muted)'}
            stroke="var(--paper)"
            strokeWidth={1.2}
            style={{ cursor: 'pointer' }}
            onMouseEnter={(e) => {
              const rect = wrapRef.current!.getBoundingClientRect();
              const svgRect = (e.target as SVGElement).getBoundingClientRect();
              setHover({
                item: p.item,
                x: svgRect.left - rect.left + svgRect.width / 2,
                y: svgRect.top - rect.top,
              });
            }}
            onMouseLeave={() => setHover(null)}
            onClick={() => p.item.link && window.open(p.item.link, '_blank', 'noopener,noreferrer')}
          />
        ))}
      </svg>

      {hover && (
        <div className="map-tooltip" style={{ left: hover.x, top: hover.y }}>
          <strong>{hover.item.outlet}</strong> · {hover.item.date_raw}<br />
          {(TYPE_LABEL[hover.item.type] || hover.item.type)} — {hover.item.title.slice(0, 80)}{hover.item.title.length > 80 ? '…' : ''}
        </div>
      )}

      <div className="timeline-legend">
        {Object.entries(TYPE_LABEL).map(([k, v]) => (
          <span key={k}>
            <span className="dot" style={{ display: 'inline-block', width: 8, height: 8, borderRadius: 50, background: TYPE_COLOR[k], marginRight: 6, verticalAlign: 'middle' }} />
            {v}
          </span>
        ))}
      </div>
    </div>
  );
}
