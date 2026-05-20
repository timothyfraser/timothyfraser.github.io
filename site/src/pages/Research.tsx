import SectionMast from '../components/SectionMast';
import Markdown from '../components/Markdown';
import Figure from '../components/Figure';
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
        eyebrow="Research"
        title="Methods, mapping, and mobility"
        subhead="A computational social science of cities — networks, social infrastructure, mobility big-data, and policy dashboards."
      />

      <section className="section">
        <Figure
          src="/images/image_my_approach.png"
          alt="Diagram of Tim's mixed-methods research approach"
          caption="Mixed-methods toolkit: network statistics, GIS, and quasi-experiments applied to environmental and resilience policy."
          align="right"
        />
        <div className="prose">
          <Markdown>{markdownPages.research}</Markdown>
        </div>
        <div style={{ clear: 'both' }} />
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Selected figures</h2>
          <p className="subhead">A few visuals from recent and ongoing work.</p>
        </div>
        <div className="grid-2">
          <Figure
            src="/images/image_nyc_congestion.png"
            alt="NYC congestion pricing analysis"
            caption="NYC congestion pricing — PM2.5 changes after six months of the cordon (npj Clean Air, 2025)."
          />
          <Figure
            src="/images/image_social_infra_nyc.png"
            alt="NYC social infrastructure map"
            caption="Social-infrastructure density across NYC neighborhoods — part of the multi-city mapping series."
          />
          <Figure
            src="/images/feature_dashjapan.png"
            alt="Japan dashboard"
            caption="Japanese municipal social-capital & vulnerability dashboard — public, queryable, validated."
          />
          <Figure
            src="/images/feature_dashstat.png"
            alt="Statistics dashboard"
            caption="Visualizer dashboard for CAT-formatted MOVES outputs — emissions by county, scenario, and pollutant."
          />
        </div>
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Research footprint</h2>
          <p className="subhead">US- and Japan-anchored; project sites across both regions plus consulting work in Mexico, Paraguay, and South Africa.</p>
        </div>
        <ResearchMap height={300} />
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Publications by topic</h2>
          <p className="subhead">Counts across the recurring topic clusters in the publication record.</p>
        </div>
        <TopicBars />
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Coauthorship</h2>
          <p className="subhead">Every node is a collaborator; edges are shared papers. Color encodes dominant research topic.</p>
        </div>
        <CoauthorNetwork height={460} />
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Selected recent papers</h2>
          <p className="subhead">2023 onward — see the Publications page for the full filterable list.</p>
        </div>
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

      <section className="section">
        <div className="section-head">
          <h2>Active research projects</h2>
          <p className="subhead">Broader research projects beyond the recruiting MEng tracks.</p>
        </div>
        <div className="grid-2">
          {researchProjects.map(p => (
            <Card key={p.id}>
              <CardHead
                id={p.category}
                name={p.name}
                tag={p.tagline}
                badge={<span className="badge accent">{p.badge.label}</span>}
              />
              <CardBody>
                <p>{p.body}</p>
                {p.links.length > 0 && (
                  <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginTop: 10 }}>
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
