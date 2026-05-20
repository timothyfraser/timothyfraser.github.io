import SectionMast from '../components/SectionMast';
import Card, { CardBody, CardHead } from '../components/Card';
import { projects, site } from '../data/loaders';

export default function Projects() {
  const meng = projects.filter(p => p.kind === 'meng');
  const research = projects.filter(p => p.kind === 'research');

  return (
    <div className="wrap">
      <SectionMast
        kicker="Projects · MEng recruiting + research portfolio"
        title={<>Three projects on cities, air, &amp; <em>movement</em>.</>}
        lede="A connected line of work asking how data, emissions, and mobility shape urban life — from a single tolling cordon in Manhattan to bikeshare networks spanning the globe."
      />

      <section>
        <div className="kicker" style={{ marginBottom: 18 }}>MEng — recruiting students now</div>
        {meng.map(p => (
          <Card key={p.id} accent={p.accent}>
            <CardHead
              id={`Project ${p.n} / ${p.category}`}
              name={p.name}
              tag={p.tagline}
              badge={<span className={`badge ${p.badge.variant}`}>{p.badge.label}</span>}
            />
            <CardBody>
              {p.question && <p className="q">{p.question}</p>}
              <p>{p.body}</p>

              {p.phase && (
                <>
                  <span className="label">{p.phase.label}</span>
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

              <span className="label">Core work</span>
              <div className="chips">
                {p.skills.map(s => <span className="chip" key={s}><b>{s.split(' ')[0]}</b> {s.split(' ').slice(1).join(' ')}</span>)}
              </div>

              {(p.links.length > 0 || p.recruiting) && (
                <div style={{ marginTop: 18, display: 'flex', gap: 8, flexWrap: 'wrap' }}>
                  {p.recruiting && (
                    <a className="btn cornell" href={`mailto:${site.email}?subject=MEng%20interest%20—%20${p.name}`}>
                      Recruiting — email Tim →
                    </a>
                  )}
                  {p.links.map(l => <a key={l.url} className="btn ghost" href={l.url}>{l.label} →</a>)}
                </div>
              )}
            </CardBody>
          </Card>
        ))}
      </section>

      <section style={{ marginTop: 60 }}>
        <div className="kicker" style={{ marginBottom: 18 }}>Active research projects</div>
        <div className="grid-2">
          {research.map(p => (
            <Card key={p.id} accent={p.accent}>
              <CardHead
                id={p.category}
                name={p.name}
                tag={p.tagline}
                badge={<span className={`badge ${p.badge.variant}`}>{p.badge.label}</span>}
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
          ))}
        </div>
      </section>
    </div>
  );
}
