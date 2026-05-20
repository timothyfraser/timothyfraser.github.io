import { useMemo, useState } from 'react';
import SectionMast from '../components/SectionMast';
import StatBox from '../components/StatBox';
import WNYCEmbed from '../components/WNYCEmbed';
import PressYearBars from '../viz/PressYearBars';
import Figure from '../components/Figure';
import '../viz/viz.css';
import { press, metrics } from '../data/loaders';

const TYPE_LABEL: Record<string, string> = {
  quoted_in: 'Quoted in',
  cited_in: 'Cited in',
  interviewed: 'Interviewed on',
  op_ed: 'Op-Ed in',
  reviewed_in: 'Reviewed in',
};

export default function Press() {
  const [filter, setFilter] = useState('');
  const [type, setType] = useState<string | null>(null);

  const items = useMemo(() => {
    return press.items.filter(p => {
      if (type && p.type !== type) return false;
      if (!filter) return true;
      const q = filter.toLowerCase();
      return (
        p.title.toLowerCase().includes(q) ||
        (p.outlet || '').toLowerCase().includes(q)
      );
    });
  }, [filter, type]);

  const groupedByYear = useMemo(() => {
    const m = new Map<number, typeof press.items>();
    for (const p of items) {
      const y = p.year || 0;
      if (!m.has(y)) m.set(y, []);
      m.get(y)!.push(p);
    }
    return Array.from(m.entries()).sort((a, b) => b[0] - a[0]);
  }, [items]);

  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Press"
        title="In the news"
        subhead="Quoted, cited, interviewed, and reviewed across academic and general-audience outlets. The late-2025 cluster was the congestion-pricing study."
      />

      {/* FEATURED WNYC */}
      <WNYCEmbed />

      {/* STAT BOXES */}
      <div className="grid-2" style={{ margin: '20px 0 28px' }}>
        <StatBox
          value={metrics.press_total}
          label="Total press mentions"
        />
        <StatBox
          value={metrics.press_last_12mo}
          label="Mentions in the last 12 months"
        />
      </div>

      {/* YEAR BARS — the recent spike now dominates */}
      <PressYearBars />

      {/* Interview — small, properly captioned */}
      <Figure
        src="/images/interview.png"
        alt="Tim interviewing an anti-nuclear activist in Kagoshima, Japan"
        caption="Tim interviewing an anti-nuclear activist in Kagoshima, Japan — Fulbright fieldwork, 2017."
        align="right"
      />

      <div className="prose" style={{ maxWidth: 'none' }}>
        <p>Below: full press feed, grouped by year. Filter or search to narrow.</p>
      </div>
      <div style={{ clear: 'both' }} />

      <div className="filter-bar">
        <input
          type="search"
          placeholder="Search title or outlet…"
          value={filter}
          onChange={e => setFilter(e.target.value)}
        />
        <button className={`chip ${!type ? 'active' : ''}`} onClick={() => setType(null)}>All types</button>
        {Object.entries(TYPE_LABEL).map(([k, v]) => (
          <button key={k} className={`chip ${type === k ? 'active' : ''}`} onClick={() => setType(k === type ? null : k)}>{v}</button>
        ))}
      </div>

      {groupedByYear.map(([year, list]) => (
        <section key={year} style={{ marginTop: 28 }}>
          <h3 style={{ fontFamily: 'var(--font-display)', fontSize: '1.15rem', fontWeight: 600, borderBottom: '1px solid var(--line)', paddingBottom: 6, marginBottom: 6 }}>
            {year || 'Undated'}
          </h3>
          {list.map((p, i) => (
            <div className="press-row" key={i}>
              <div className="date">{p.date_raw || p.year}</div>
              <div>
                <span className={`type-pill ${p.type}`}>{TYPE_LABEL[p.type] || p.type}</span>
                <span className="title">
                  {p.link ? <a href={p.link}>{p.title}</a> : p.title}
                </span>
                {p.press_author && (
                  <span style={{ color: 'var(--muted)', fontSize: '0.88rem' }}> — {p.press_author}</span>
                )}
              </div>
              <div className="outlet">{p.outlet}</div>
            </div>
          ))}
        </section>
      ))}

      {items.length === 0 && (
        <p style={{ color: 'var(--muted)', marginTop: 30 }}>No press matches that filter.</p>
      )}
    </div>
  );
}
