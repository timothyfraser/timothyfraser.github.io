import { publications } from '../data/loaders';

// Per-topic counts, granular rather than collapsed into two mega-clusters.
// A publication can carry several topics, so the bars overlap by design —
// they answer "how many papers touch X?", not a partition of the corpus.
const TOPIC_META: { topic: string; label: string; color: string }[] = [
  { topic: 'environment', label: 'Environment / climate', color: 'var(--topic-environment)' },
  { topic: 'transportation', label: 'Transportation', color: 'var(--topic-transportation)' },
  { topic: 'disaster', label: 'Disaster & recovery', color: 'var(--topic-disaster)' },
  { topic: 'resilience', label: 'Resilience / social capital', color: 'var(--topic-resilience)' },
  { topic: 'social_infrastructure', label: 'Social infrastructure', color: 'oklch(55% 0.10 320)' },
  { topic: 'energy', label: 'Energy', color: 'var(--topic-energy)' },
  { topic: 'gis', label: 'GIS / spatial', color: 'oklch(55% 0.09 200)' },
  { topic: 'networks', label: 'Networks (method)', color: 'var(--topic-networks)' },
  { topic: 'health', label: 'Health', color: 'var(--topic-health)' },
  { topic: 'polarization', label: 'Polarization', color: 'var(--topic-polarization)' },
];

export default function TopicBars() {
  const counts = TOPIC_META
    .map(m => ({ ...m, count: publications.filter(p => p.topics.includes(m.topic)).length }))
    .filter(c => c.count > 0)
    .sort((a, b) => b.count - a.count);
  const max = Math.max(...counts.map(c => c.count), 1);

  return (
    <div className="topicbar-wrap" role="group" aria-label="Publications by research topic">
      {counts.map(c => (
        <div className="topicbar-row" key={c.topic}>
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
