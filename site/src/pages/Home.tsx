import { Link } from 'react-router-dom';
import SectionMast from '../components/SectionMast';
import StatStrip from '../components/StatStrip';
import Card, { CardBody, CardHead } from '../components/Card';
import Markdown from '../components/Markdown';
import CoauthorNetwork from '../viz/CoauthorNetwork';
import '../viz/viz.css';
import { metrics, projects, press, site, markdownPages, software } from '../data/loaders';

export default function Home() {
  const featuredProjects = projects.filter(p => p.kind === 'meng');
  const featuredSoftware = software.filter(s => s.featured).slice(0, 4);
  const topPress = press.items.slice(0, 4);

  return (
    <div className="wrap">
      <SectionMast
        kicker={`${site.role} · ${site.institution}`}
        title={<>Building software<br/>for cities, air, &amp; <em>climate action</em>.</>}
        subline={
          <>
            <span><b>{site.name}</b> — {site.affiliation}</span>
            <span className="mono">ITHACA, NY</span>
            <span className="mono">{site.email}</span>
          </>
        }
        lede={<><span>Data, methods, and dashboards</span> that help communities combat and adapt to climate change — from a single tolling cordon in Manhattan to bikeshare networks spanning the globe.</>}
      />

      <div className="reveal d2" style={{ marginBottom: 24 }}>
        <Markdown>{markdownPages.home}</Markdown>
      </div>

      <StatStrip
        stats={[
          { label: 'Citations', value: metrics.citations ?? '—', accent: true },
          { label: 'h-index', value: metrics.h_index ?? '—' },
          { label: 'i10-index', value: metrics.i10_index ?? '—' },
          { label: 'Peer-reviewed', value: metrics.peer_reviewed },
          { label: 'Software', value: metrics.software },
          { label: 'Coauthors', value: metrics.coauthors },
          { label: 'Press mentions', value: metrics.press_total },
        ]}
      />

      <section className="reveal d4" style={{ marginTop: 36 }}>
        <div className="kicker" style={{ marginBottom: 16 }}>Coauthor citation network · live</div>
        <p style={{ maxWidth: '60ch', color: 'var(--muted)', marginBottom: 18 }}>
          Every node is a coauthor; edges are shared papers. Tim is at the center. Color encodes
          dominant research topic. Hover for details.
        </p>
        <CoauthorNetwork height={520} />
      </section>

      <section className="reveal d5" style={{ marginTop: 50 }}>
        <div className="kicker" style={{ marginBottom: 18 }}>MEng projects · recruiting</div>
        <div className="grid-2">
          {featuredProjects.map(p => (
            <Card key={p.id} accent={p.accent}>
              <CardHead
                id={`Project ${p.n} / ${p.category}`}
                name={p.name}
                tag={p.tagline}
                badge={<span className={`badge ${p.badge.variant}`}>{p.badge.label}</span>}
              />
              <CardBody>
                <p>{p.body}</p>
                <Link to="/projects" className="btn ghost" style={{ marginTop: 4 }}>See all projects →</Link>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>

      <section style={{ marginTop: 50 }}>
        <div className="kicker" style={{ marginBottom: 18 }}>Software & teaching</div>
        <div className="grid-2">
          {featuredSoftware.map(s => (
            <Card key={s.id} accent="signal">
              <CardHead
                id={s.tags.slice(0, 3).join(' · ').toUpperCase()}
                name={s.name}
                tag={s.blurb}
              />
              <CardBody>
                <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap' }}>
                  {s.live_url && <a className="btn" href={s.live_url}>Live →</a>}
                  {s.repo_url && <a className="btn ghost" href={s.repo_url}>GitHub →</a>}
                </div>
              </CardBody>
            </Card>
          ))}
        </div>
        <p style={{ marginTop: 16 }}>
          <Link to="/software" className="btn ghost">All software →</Link>
          <a className="btn ghost" href={site.links.sigma} style={{ marginLeft: 8 }}>Sigma textbook →</a>
          <a className="btn ghost" href={site.links.netsci} style={{ marginLeft: 8 }}>NetSci course →</a>
        </p>
      </section>

      <section style={{ marginTop: 50 }}>
        <div className="kicker" style={{ marginBottom: 18 }}>Latest press</div>
        {topPress.map((p, i) => (
          <div className="press-row" key={i}>
            <div className="date">{p.date_raw || p.year}</div>
            <div>
              <span className={`type-pill ${p.type}`}>{p.type.replace('_', ' ')}</span>
              <span className="title">
                {p.link ? <a href={p.link}>{p.title}</a> : p.title}
              </span>
            </div>
            <div className="outlet">{p.outlet}</div>
          </div>
        ))}
        <p style={{ marginTop: 14 }}>
          <Link to="/press" className="btn ghost">All press →</Link>
        </p>
      </section>
    </div>
  );
}
