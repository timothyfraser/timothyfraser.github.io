import { useMemo, useState } from 'react';
import StatBox from '../components/StatBox';
import WNYCEmbed from '../components/WNYCEmbed';
import Figure from '../components/Figure';
import PressMediaIcon from '../components/PressMediaIcon';
import '../viz/viz.css';
import { press, metrics, speaking } from '../data/loaders';

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

  // Speaking engagements are curated content (not press mentions), so they sit
  // in their own section above the feed and ignore the search/type filters.
  // Only entries flagged `featured` are rendered. Non-featured speaking entries
  // (e.g. the MARAMA invited talk) are intentionally NOT rendered right now —
  // they stay in site/content/speaking.json as the durable log of every talk.
  const featuredTalks = useMemo(() => speaking.filter(s => s.featured), []);

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
      <header className="press-hero reveal d1">
        <div className="press-hero-copy">
          <div className="eyebrow">Press</div>
          <h1>In the news</h1>
          <p className="subhead">
            Quoted, cited, interviewed, and reviewed across academic and general-audience outlets. The late-2025 cluster was the congestion-pricing study.
          </p>
        </div>
        <Figure
          src="/images/interview.png"
          alt="Tim interviewing an anti-nuclear activist in Kagoshima, Japan"
          caption="Tim interviewing an anti-nuclear activist in Kagoshima, Japan — Fulbright fieldwork, 2017."
        />
      </header>

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

      {/* FEATURED PRESS COVERAGE — marquee keynote + broadcast interview */}
      <section style={{ marginTop: 34 }}>
        <div className="section-head">
          <div className="eyebrow">Featured</div>
          <h2>Featured Press Coverage</h2>
          <p className="subhead">Marquee appearances — keynote stages and broadcast interviews on congestion pricing and air quality.</p>
        </div>

        {featuredTalks.map((s, i) => (
          <article className="card featured" key={`ft-${i}`}>
            <div
              style={{
                display: 'flex',
                gap: 20,
                padding: '20px 24px',
                flexWrap: 'wrap',
                alignItems: 'flex-start',
              }}
            >
              {s.image && (
                <figure style={{ margin: 0, flex: '0 0 200px', maxWidth: '100%' }}>
                  <img
                    src={s.image}
                    alt={s.image_alt || s.title}
                    loading="lazy"
                    style={{
                      width: '100%',
                      height: 130,
                      objectFit: 'cover',
                      display: 'block',
                      borderRadius: 'var(--radius-sm)',
                      border: '1px solid var(--line)',
                    }}
                  />
                  {s.image_credit && (
                    <figcaption
                      style={{
                        fontSize: '0.68rem',
                        fontStyle: 'italic',
                        color: 'var(--muted)',
                        marginTop: 5,
                      }}
                    >
                      {s.image_credit}
                    </figcaption>
                  )}
                </figure>
              )}
              <div style={{ flex: '1 1 320px', minWidth: 0 }}>
                <span className="badge accent">{s.role}</span>
                <h3 className="card-name" style={{ fontSize: '1.28rem', margin: '10px 0 0' }}>
                  {s.title}
                </h3>
                <div className="card-tag" style={{ marginTop: 6 }}>{s.venue}</div>
                <div
                  className="card-meta"
                  style={{ marginTop: 12, paddingTop: 10 }}
                >
                  {[s.date, s.location].filter(Boolean).join(' · ')}
                </div>
              </div>
            </div>
          </article>
        ))}

        {/* FEATURED WNYC — broadcast interview, sits directly below the keynote card */}
        <WNYCEmbed />
      </section>

      <div className="prose" style={{ maxWidth: 'none', marginTop: 34 }}>
        <p>Below: full press feed, grouped by year. Filter or search to narrow.</p>
      </div>

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
        <section key={year} className="press-year-group" style={{ marginTop: 28 }}>
          <h3 style={{ fontFamily: 'var(--font-display)', fontSize: '1.15rem', fontWeight: 600, borderBottom: '1px solid var(--line)', paddingBottom: 6, marginBottom: 6 }}>
            {year || 'Undated'}
          </h3>
          <div className="press-feed">
            {list.map((p, i) => (
              <div className="press-row press-row-rich" key={i}>
                <div className="date">{p.date_raw || p.year}</div>
                <PressMediaIcon outlet={p.outlet} mediaType={p.media_type} />
                <div className="press-main">
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
          </div>
        </section>
      ))}

      {items.length === 0 && (
        <p style={{ color: 'var(--muted)', marginTop: 30 }}>No press matches that filter.</p>
      )}
    </div>
  );
}
