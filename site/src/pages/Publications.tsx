import { useMemo, useState } from 'react';
import SectionMast from '../components/SectionMast';
import TopicBars from '../viz/TopicBars';
import '../viz/viz.css';
import { publications } from '../data/loaders';

const TYPE_LABELS: Record<string, string> = {
  paper: 'Peer-reviewed',
  chapter: 'Chapter',
  book: 'Book',
  report: 'Report',
  software: 'Software',
  'working paper': 'Working paper',
  'under review': 'Under review',
  'under revise and resubmit': 'Revise & resubmit',
};

const TOPIC_OPTIONS = [
  'environment','transportation','disaster','resilience','social_infrastructure','networks','gis','polarization','energy','health',
];

export default function Publications() {
  const [q, setQ] = useState('');
  const [type, setType] = useState<string | null>(null);
  const [topic, setTopic] = useState<string | null>(null);

  const types = useMemo(() => Array.from(new Set(publications.map(p => p.type))), []);

  const items = useMemo(() => {
    return publications.filter(p => {
      if (type && p.type !== type) return false;
      if (topic && !p.topics.includes(topic)) return false;
      if (!q) return true;
      const ql = q.toLowerCase();
      return (
        p.title.toLowerCase().includes(ql) ||
        p.authors.some(a => a.toLowerCase().includes(ql)) ||
        (p.journal || '').toLowerCase().includes(ql)
      );
    });
  }, [q, type, topic]);

  return (
    <div className="wrap">
      <SectionMast
        eyebrow={`Publications · ${publications.length} entries`}
        title="Papers, chapters, and software"
        subhead="Filter by type and topic. Click through for the DOI or the full PDF. The CV remains the canonical record."
      />

      <TopicBars />

      <div className="filter-bar">
        <input
          type="search"
          placeholder="Search title / author / journal…"
          value={q}
          onChange={e => setQ(e.target.value)}
        />
        <button className={`chip ${!type ? 'active' : ''}`} onClick={() => setType(null)}>All types</button>
        {types.map(t => (
          <button key={t} className={`chip ${type === t ? 'active' : ''}`} onClick={() => setType(t === type ? null : t)}>
            {TYPE_LABELS[t] || t}
          </button>
        ))}
      </div>
      <div className="filter-bar" style={{ marginTop: -10 }}>
        <button className={`chip ${!topic ? 'active' : ''}`} onClick={() => setTopic(null)}>All topics</button>
        {TOPIC_OPTIONS.map(t => (
          <button key={t} className={`chip ${topic === t ? 'active' : ''}`} onClick={() => setTopic(t === topic ? null : t)}>
            {t.replace(/_/g, ' ')}
          </button>
        ))}
      </div>

      <p style={{ margin: '8px 0 18px', fontSize: '0.88rem', color: 'var(--muted)' }}>
        Showing {items.length} of {publications.length}
      </p>

      {items.map(p => (
        <div className="pub-row" key={p.id}>
          <div className="pub-meta">{p.year ?? '—'} · {TYPE_LABELS[p.type] || p.type}</div>
          <div className="pub-title">
            {p.link ? <a href={p.link}>{p.title}</a> : p.title}
          </div>
          <div className="pub-authors" dangerouslySetInnerHTML={{
            __html: p.authors.map(a => a === 'Timothy Fraser' ? `<b>${a}</b>` : a).join(', '),
          }} />
          {p.journal && <div className="pub-journal">{p.journal}</div>}
          <div style={{ marginTop: 4 }}>
            {p.topics.map(t => <span className="topic-tag" key={t}>{t.replace(/_/g, ' ')}</span>)}
          </div>
        </div>
      ))}
    </div>
  );
}
