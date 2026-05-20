import { publications } from '../data/loaders';

const TOPIC_GROUPS: { label: string; topics: string[]; color: string }[] = [
  { label: 'Emissions / Environmental', topics: ['environment', 'transportation', 'energy'], color: 'var(--signal)' },
  { label: 'Resilience / Health',       topics: ['disaster', 'resilience', 'social_infrastructure', 'health'], color: 'var(--cool)' },
  { label: 'Polarization',              topics: ['polarization'], color: 'var(--gold)' },
  { label: 'Networks / GIS (method)',   topics: ['networks', 'gis'], color: 'var(--hot)' },
];

export default function TopicBars() {
  const counts = TOPIC_GROUPS.map(g => ({
    ...g,
    count: publications.filter(p => g.topics.some(t => p.topics.includes(t))).length,
  }));
  const max = Math.max(...counts.map(c => c.count), 1);

  return (
    <div className="topicbar-wrap" role="group" aria-label="Publications by research topic group">
      {counts.map(c => (
        <div className="topicbar-row" key={c.label}>
          <div>{c.label}</div>
          <div className="topicbar-track">
            <div
              className="topicbar-fill"
              style={{ width: `${(c.count / max) * 100}%`, background: c.color }}
            />
          </div>
          <div style={{ textAlign: 'right', color: 'var(--muted)' }}>{c.count}</div>
        </div>
      ))}
    </div>
  );
}
