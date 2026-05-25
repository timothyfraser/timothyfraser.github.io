import { Link } from 'react-router-dom';
import StatBox from '../components/StatBox';
import WNYCEmbed from '../components/WNYCEmbed';
import TopicBars from '../viz/TopicBars';
import '../viz/viz.css';
import { metrics, projects, press, site } from '../data/loaders';

interface WorkTile { img: string; alt: string; title: string; desc: string; href: string; }

export default function Home() {
  const mengProjects = projects.filter(p => p.kind === 'meng');
  const topPress = press.items.slice(0, 4);

  const workTiles: WorkTile[] = [
    {
      img: '/images/dashboard_cat.PNG',
      alt: 'Climate Action in Transportation dashboard',
      title: 'CAT — Climate Action in Transportation',
      desc: 'Open dashboard system that lets cities estimate transportation emissions online — built on EPA MOVES.',
      href: site.links.cat,
    },
    {
      img: '/images/image_nyc_congestion.png',
      alt: 'NYC congestion pricing study',
      title: 'NYC congestion pricing & PM2.5',
      desc: 'A first look at PM2.5 impacts after six months of NYC\'s cordon pricing — npj Clean Air, 2025.',
      href: 'https://doi.org/10.1038/s44407-025-00037-2',
    },
    {
      img: '/images/epic-failure-function.png',
      alt: 'The Epic Failure Function — cumulative probability of failure over time',
      title: 'Sigma — open textbook',
      desc: 'System Reliability & Six Sigma in R and Python. ~28 chapters, dual-language, with learning checks.',
      href: site.links.sigma,
    },
  ];

  return (
    <div className="wrap">
      {/* HERO BAND — red-tinted */}
      <section className="hero-band reveal d1">
        <div className="hero">
          <div>
            <div className="eyebrow">Cornell University · Systems Engineering · CTECH</div>
            <h1>Timothy Fraser, PhD</h1>
            <p className="hero-role">
              <b>Assistant Teaching Professor</b> of Systems Engineering at Cornell<br />
              Coordinator of the <b>Center for Transportation, Environment, and Community Health (CTECH)</b>.<br />
              <b>Cornell Atkinson Center for Sustainability</b> Faculty Fellow<br />
              I build methods, systems, and software to help communities combat environmental crises.
            </p>

            <div className="action-menu">
              <Link to="/research" className="btn ghost">Research</Link>
              <Link to="/teaching" className="btn ghost">Teaching</Link>
              <Link to="/projects" className="btn accent">MEng projects — recruiting →</Link>
              <Link to="/software" className="btn ghost">Software</Link>
              <Link to="/publications" className="btn ghost">Publications</Link>
              <Link to="/press" className="btn ghost">Press</Link>
              <a href={site.links.cv} className="btn ghost">CV ↗</a>
            </div>

            <div className="profile-links" style={{ marginTop: 14 }}>
              <a href={site.links.scholar}>Google Scholar</a><span className="sep">·</span>
              <a href={site.links.linkedin}>LinkedIn</a><span className="sep">·</span>
              <a href={site.links.orcid}>ORCID</a><span className="sep">·</span>
              <a href={site.links.researchgate}>ResearchGate</a><span className="sep">·</span>
              <a href={site.links.github}>GitHub</a><span className="sep">·</span>
              <a href={`mailto:${site.email}`}>{site.email}</a>
            </div>
          </div>
          <div className="hero-portrait">
            <img src="/images/headshot.jpg" alt="Portrait of Timothy Fraser" />
          </div>
        </div>

        {/* STAT BOXES — 4 across, red top borders */}
        <div className="grid-4" style={{ marginTop: 4 }}>
          <StatBox
            value={<><em>50+</em></>}
            label="Peer-reviewed studies"
            source={{ label: 'Publications', href: '/publications' }}
          />
          <StatBox
            value={metrics.citations?.toLocaleString() ?? '—'}
            label={`Citations · ${metrics.as_of}`}
            source={{ label: 'Google Scholar', href: site.links.scholar, external: true }}
          />
          <StatBox
            value={`h ${metrics.h_index} · i10 ${metrics.i10_index}`}
            label="Indices"
            source={{ label: 'Google Scholar', href: site.links.scholar, external: true }}
          />
          <StatBox
            value={<>{metrics.press_total} <span style={{ fontSize: '0.65em', color: 'var(--muted)' }}>· {metrics.press_last_12mo} in last 12 mo</span></>}
            label="Press mentions"
            source={{ label: 'See press', href: '/press' }}
          />
        </div>
      </section>

      {/* WNYC FEATURED INTERVIEW */}
      <section className="reveal d2">
        <WNYCEmbed />
      </section>

      {/* PUBLICATIONS BY TOPIC */}
      <section className="section reveal d3">
        <div className="section-head">
          <h2>What I work on</h2>
          <p className="subhead">Across emissions, resilience, polarization, and methods — counts from the publication record.</p>
        </div>
        <TopicBars />
      </section>

      {/* SELECTED WORK — image tiles */}
      <section className="section reveal d3">
        <div className="section-head">
          <h2>Selected figures</h2>
          <p className="subhead">Open dashboards, a recent paper on NYC congestion pricing, and an open textbook for Six Sigma in R and Python.</p>
        </div>
        <div className="grid-3">
          {workTiles.map(t => (
            <a key={t.title} href={t.href} className="work-tile" target="_blank" rel="noopener noreferrer">
              <img className="work-tile-img" src={t.img} alt={t.alt} loading="lazy" />
              <div className="work-tile-body">
                <h3>{t.title} →</h3>
                <p>{t.desc}</p>
              </div>
            </a>
          ))}
        </div>
      </section>

      {/* MENG RECRUITING */}
      <section className="section reveal d4">
        <div className="section-head">
          <h2>MEng projects — recruiting</h2>
          <p className="subhead">
            Cornell MEng students welcome — {' '}
            <a href={`mailto:${site.email}?subject=MEng%20interest`}>email me</a>{' '}or see all <Link to="/projects">project details</Link>.
          </p>
        </div>
        <div className="recruit-list">
          {mengProjects.map(p => (
            <div className="recruit-row" key={p.id}>
              <div>
                <div className="meng-tag">Project {p.n} · {p.category}</div>
                <h3>{p.name}</h3>
                <p>{p.tagline}</p>
              </div>
              <Link to="/projects" className="btn ghost">Details →</Link>
            </div>
          ))}
        </div>
      </section>

      {/* PRESS */}
      <section className="section reveal d5">
        <div className="section-head">
          <h2>Recent press</h2>
          <p className="subhead">A late-2025 cluster on NYC congestion pricing — Bloomberg, NYT, Vox, Gothamist, Newsweek, and others.</p>
        </div>
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
        <p style={{ marginTop: 18 }}>
          <Link to="/press" className="btn ghost">All press →</Link>
        </p>
      </section>
    </div>
  );
}
