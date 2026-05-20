import { useMemo, useState } from 'react';
import SectionMast from '../components/SectionMast';
import Card, { CardBody, CardHead } from '../components/Card';
import { software } from '../data/loaders';

export default function Software() {
  const [filter, setFilter] = useState('');
  const allTags = useMemo(() => {
    const s = new Set<string>();
    software.forEach(p => p.tags.forEach(t => s.add(t)));
    return Array.from(s).sort();
  }, []);
  const [tag, setTag] = useState<string | null>(null);

  const filtered = software.filter(p => {
    if (tag && !p.tags.includes(tag)) return false;
    if (!filter) return true;
    const q = filter.toLowerCase();
    return p.name.toLowerCase().includes(q) || p.blurb.toLowerCase().includes(q);
  });

  return (
    <div className="wrap">
      <SectionMast
        kicker="Software · R packages, dashboards, course books"
        title={<>Open <em>tooling</em> for emissions, networks, &amp; resilience.</>}
        lede="Every project ships an R package, a dashboard, or a textbook. Source is on GitHub; dashboards run live."
      />

      <div className="filter-bar">
        <input
          type="search"
          placeholder="Search software…"
          value={filter}
          onChange={e => setFilter(e.target.value)}
        />
        <button className={`chip ${!tag ? 'active' : ''}`} onClick={() => setTag(null)}>All</button>
        {allTags.map(t => (
          <button key={t} className={`chip ${tag === t ? 'active' : ''}`} onClick={() => setTag(t === tag ? null : t)}>{t}</button>
        ))}
      </div>

      <div className="grid-2">
        {filtered.map(s => (
          <Card key={s.id} accent={s.featured ? 'signal' : 'gold'}>
            <CardHead
              id={s.tags.slice(0, 3).join(' · ').toUpperCase()}
              name={s.name}
              tag={s.blurb}
              badge={s.featured ? <span className="badge signal">Featured</span> : <span className="badge">{s.status}</span>}
            />
            <CardBody>
              <div className="chips">
                {s.tags.map(t => <span className="chip" key={t}>{t}</span>)}
              </div>
              <div style={{ marginTop: 14, display: 'flex', gap: 8, flexWrap: 'wrap' }}>
                {s.live_url && <a className="btn" href={s.live_url}>Live →</a>}
                {s.repo_url && <a className="btn ghost" href={s.repo_url}>GitHub →</a>}
              </div>
            </CardBody>
          </Card>
        ))}
      </div>

      {filtered.length === 0 && (
        <p style={{ color: 'var(--muted)', marginTop: 20 }}>No software matches that filter.</p>
      )}
    </div>
  );
}
