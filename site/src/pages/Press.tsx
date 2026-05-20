import { useMemo, useState } from 'react';
import SectionMast from '../components/SectionMast';
import PressTimeline from '../viz/PressTimeline';
import '../viz/viz.css';
import { press } from '../data/loaders';

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
        kicker={`Press · ${press.items.length} mentions`}
        title={<>In the <em>news</em>.</>}
        lede="Quoted, cited, interviewed, and reviewed. The congestion-pricing cluster in late 2025 was a particularly busy moment."
      />

      <PressTimeline />

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
        <section key={year} style={{ marginTop: 26 }}>
          <h3 style={{ fontFamily: 'var(--font-mono)', fontSize: '0.84rem', letterSpacing: '0.18em', textTransform: 'uppercase', color: 'var(--muted)', borderBottom: '2px solid var(--ink)', paddingBottom: 6 }}>
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
