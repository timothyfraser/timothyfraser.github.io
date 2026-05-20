import SectionMast from '../components/SectionMast';
import Markdown from '../components/Markdown';
import CoauthorNetwork from '../viz/CoauthorNetwork';
import ResearchMap from '../viz/ResearchMap';
import TopicBars from '../viz/TopicBars';
import '../viz/viz.css';
import { markdownPages, publications, projects } from '../data/loaders';
import Card, { CardBody, CardHead } from '../components/Card';

export default function Research() {
  const selected = publications
    .filter(p => p.type === 'paper' && p.year && p.year >= 2023)
    .slice(0, 8);

  const researchProjects = projects.filter(p => p.kind === 'research');

  return (
    <div className="wrap">
      <SectionMast
        kicker="Research · methods, mapping, mobility"
        title={<>How communities <em>adapt</em>.</>}
        lede="A computational social science of cities: networks, social infrastructure, mobility big data, and policy dashboards."
      />

      <div className="reveal d2">
        <Markdown>{markdownPages.research}</Markdown>
      </div>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 12 }}>Research footprint</div>
        <ResearchMap height={300} />
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 12 }}>Publications by topic</div>
        <TopicBars />
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 12 }}>Coauthorship</div>
        <CoauthorNetwork height={460} />
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 14 }}>Selected recent papers</div>
        {selected.map(p => (
          <div className="pub-row" key={p.id}>
            <div className="pub-meta">{p.year} · {p.type}</div>
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
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 16 }}>Research projects</div>
        <div className="grid-2">
          {researchProjects.map(p => (
            <Card key={p.id} accent={p.accent}>
              <CardHead
                id={p.category}
                name={p.name}
                tag={p.tagline}
                badge={<span className={`badge ${p.badge.variant}`}>{p.badge.label}</span>}
              />
              <CardBody>
                <p>{p.body}</p>
                {p.links.length > 0 && (
                  <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginTop: 8 }}>
                    {p.links.map(l => <a key={l.url} className="btn ghost" href={l.url}>{l.label} →</a>)}
                  </div>
                )}
              </CardBody>
            </Card>
          ))}
        </div>
      </section>
    </div>
  );
}
