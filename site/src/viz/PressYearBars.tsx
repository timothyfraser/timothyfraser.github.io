import { useMemo, useRef, useState } from 'react';
import { press, metrics } from '../data/loaders';

/**
 * Per-year column chart of press mentions. Replaces the per-month dotgrid
 * (which gave too much horizontal space to sparse historical years and
 * collapsed the recent surge). Cornell-red bars; hover reveals that year's
 * outlets + titles.
 */
export default function PressYearBars({ height = 200 }: { height?: number }) {
  const ref = useRef<HTMLDivElement | null>(null);
  const [hover, setHover] = useState<number | null>(null);

  const { years, counts, max } = useMemo(() => {
    const byYear: Record<number, number> = (metrics.press_by_year as Record<string, number>) || {};
    // Ensure we have a contiguous year range from min to max present in the data
    const presentYears = Object.keys(byYear).map(Number).sort((a, b) => a - b);
    if (!presentYears.length) return { years: [], counts: [] as number[], max: 1 };
    const min = presentYears[0];
    const max = presentYears[presentYears.length - 1];
    const years: number[] = [];
    const counts: number[] = [];
    for (let y = min; y <= max; y++) {
      years.push(y);
      counts.push(byYear[y] || 0);
    }
    return { years, counts, max: Math.max(...counts, 1) };
  }, []);

  if (!years.length) return null;

  const width = 900;
  const padX = 40;
  const padY = 28;
  const innerW = width - padX * 2;
  const innerH = height - padY * 2;
  const bw = innerW / years.length;

  // Hits in the hovered year (for tooltip)
  const hoveredItems = useMemo(() => {
    if (hover === null) return [];
    return press.items.filter(p => p.year === hover).slice(0, 8);
  }, [hover]);

  return (
    <div className="timeline" ref={ref} style={{ position: 'relative' }}>
      <div className="eyebrow" style={{ marginBottom: 10 }}>Press mentions by year</div>
      <svg
        viewBox={`0 0 ${width} ${height}`}
        role="img"
        aria-label={`Press mentions per year from ${years[0]} to ${years[years.length - 1]}.`}
        style={{ width: '100%', height: 'auto', display: 'block' }}
      >
        {/* Baseline */}
        <line
          x1={padX} y1={height - padY}
          x2={width - padX} y2={height - padY}
          stroke="var(--line-2)"
        />

        {/* Bars */}
        {years.map((y, i) => {
          const c = counts[i];
          const h = c === 0 ? 0 : (c / max) * innerH;
          const x = padX + i * bw + bw * 0.18;
          const yPos = height - padY - h;
          const wBar = bw * 0.64;
          const isHover = hover === y;
          return (
            <g key={y}>
              <rect
                x={x}
                y={yPos}
                width={wBar}
                height={h}
                fill={c > 0 ? 'var(--accent)' : 'var(--line)'}
                opacity={hover === null || isHover ? 1 : 0.5}
                style={{ cursor: c > 0 ? 'pointer' : 'default', transition: 'opacity 0.12s ease' }}
                onMouseEnter={() => c > 0 && setHover(y)}
                onMouseLeave={() => setHover(null)}
                rx={1}
              />
              {/* Count label above the bar */}
              {c > 0 && (
                <text
                  x={x + wBar / 2}
                  y={yPos - 4}
                  textAnchor="middle"
                  fontSize={11}
                  fontFamily="var(--font-body)"
                  fontWeight={600}
                  fill="var(--ink)"
                >
                  {c}
                </text>
              )}
              {/* Year label */}
              <text
                x={x + wBar / 2}
                y={height - padY + 16}
                textAnchor="middle"
                fontSize={11}
                fontFamily="var(--font-body)"
                fill="var(--muted)"
              >
                {y}
              </text>
            </g>
          );
        })}
      </svg>

      {hover !== null && hoveredItems.length > 0 && (
        <div
          style={{
            position: 'absolute',
            right: 16, top: 12,
            maxWidth: 360,
            background: 'var(--ink)',
            color: 'var(--bg)',
            padding: '12px 14px',
            borderRadius: 4,
            fontSize: '0.82rem',
            lineHeight: 1.4,
            zIndex: 3,
            pointerEvents: 'none',
          }}
        >
          <strong style={{ fontFamily: 'var(--font-display)', fontSize: '0.95rem' }}>
            {hover} — {hoveredItems.length}{counts[years.indexOf(hover)] > hoveredItems.length ? '+' : ''} mention{hoveredItems.length === 1 ? '' : 's'}
          </strong>
          <ul style={{ margin: '6px 0 0 14px', padding: 0 }}>
            {hoveredItems.map((p, i) => (
              <li key={i} style={{ marginBottom: 3 }}>
                <em style={{ color: 'oklch(85% 0.04 28)' }}>{p.outlet}</em> — {p.title.slice(0, 60)}{p.title.length > 60 ? '…' : ''}
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
