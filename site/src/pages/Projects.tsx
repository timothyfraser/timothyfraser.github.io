import SectionMast from '../components/SectionMast';
import Card, { CardBody, CardHead, CardFigure } from '../components/Card';
import { projects, site } from '../data/loaders';

const PROJECT_IMAGES: Record<string, { src: string; alt: string }> = {
  cpportal: { src: '/images/update_nyc.png', alt: 'NYC congestion-pricing analysis' },
  bikeshare: { src: '/images/update_bluebikes.png', alt: 'Bluebikes bikeshare mobility network, Boston' },
  cat: { src: '/images/dashboard_cat.PNG', alt: 'CAT dashboard' },
  'social-infrastructure': { src: '/images/image_social_infra_nyc.png', alt: 'Social infrastructure map' },
};

export default function Projects() {
  const meng = projects.filter(p => p.kind === 'meng');
  const research = projects.filter(p => p.kind === 'research');

  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Projects"
        title="Three MEng tracks, plus active research"
        subhead="A connected line of work on cities, air, and movement — from a single tolling cordon in Manhattan to bikeshare networks spanning the globe."
      />

      <section className="section">
        <div className="section-head">
          <h2>MEng — recruiting students</h2>
          <p className="subhead">Cornell MEng students welcome. Each project ships a paper plus a usable artifact (dashboard, API, or model).</p>
        </div>
        {meng.map(p => {
          const img = PROJECT_IMAGES[p.id];
          return (
            <Card key={p.id} featured>
              {img && <CardFigure src={img.src} alt={img.alt} />}
              <CardHead
                id={`Project ${p.n} · ${p.category}`}
                name={p.name}
                tag={p.tagline}
                badge={<span className="badge accent">{p.badge.label}</span>}
              />
              <CardBody>
                {p.question && <p className="q">{p.question}</p>}
                <p>{p.body}</p>

                {p.phase && (
                  <>
                    <div style={{ fontSize: '0.78rem', color: 'var(--muted)', marginTop: 10 }}>{p.phase.label}</div>
                    <div className="flow">
                      {p.phase.steps.map((s, i) => (
                        <span key={s}>
                          <span className={`step ${s === p.phase!.now ? 'now' : ''}`}>{s}</span>
                          {i < p.phase!.steps.length - 1 && <span className="arrow"> → </span>}
                        </span>
                      ))}
                    </div>
                  </>
                )}

                <div style={{ fontSize: '0.78rem', color: 'var(--muted)', marginTop: 8 }}>Core work</div>
                <div className="chips">
                  {p.skills.map(s => <span className="chip" key={s}>{s}</span>)}
                </div>

                {(p.links.length > 0 || p.recruiting) && (
                  <div style={{ marginTop: 18, display: 'flex', gap: 8, flexWrap: 'wrap' }}>
                    {p.recruiting && (
                      <a className="btn accent" href={`mailto:${site.email}?subject=MEng%20interest%20—%20${p.name}`}>
                        Email Tim →
                      </a>
                    )}
                    {p.links.map(l => <a key={l.url} className="btn ghost" href={l.url}>{l.label} →</a>)}
                  </div>
                )}
              </CardBody>
            </Card>
          );
        })}
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Active research projects</h2>
          <p className="subhead">Broader portfolio of ongoing research.</p>
        </div>
        <div className="grid-2">
          {research.map(p => {
            const img = PROJECT_IMAGES[p.id];
            return (
              <Card key={p.id}>
                {img && <CardFigure src={img.src} alt={img.alt} />}
                <CardHead
                  id={p.category}
                  name={p.name}
                  tag={p.tagline}
                  badge={<span className="badge">{p.badge.label}</span>}
                />
                <CardBody>
                  <p>{p.body}</p>
                  <div className="chips">
                    {p.skills.map(s => <span className="chip" key={s}>{s}</span>)}
                  </div>
                  {p.links.length > 0 && (
                    <div style={{ marginTop: 12, display: 'flex', gap: 8, flexWrap: 'wrap' }}>
                      {p.links.map(l => <a key={l.url} className="btn ghost" href={l.url}>{l.label} →</a>)}
                    </div>
                  )}
                </CardBody>
              </Card>
            );
          })}
        </div>
      </section>
    </div>
  );
}
