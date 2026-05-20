import { Link } from 'react-router-dom';
import Markdown from '../components/Markdown';
import { metrics, projects, press, site, markdownPages } from '../data/loaders';

interface WorkTile { img: string; alt: string; title: string; desc: string; href: string; }

export default function Home() {
  const mengProjects = projects.filter(p => p.kind === 'meng');
  const topPress = press.items.slice(0, 4);

  const workTiles: WorkTile[] = [
    {
      img: '/images/dashboard_cat.PNG',
      alt: 'Climate Action in Transportation dashboard',
      title: 'Climate Action in Transportation',
      desc: 'Dashboard, R packages, and cloud jobs that let local decision-makers estimate transportation emissions online — built on EPA MOVES.',
      href: site.links.cat,
    },
    {
      img: '/images/image_nyc_congestion.png',
      alt: 'NYC congestion pricing study',
      title: 'NYC congestion pricing & PM2.5',
      desc: 'A first look at PM2.5 impacts after six months of New York City\'s cordon pricing — published in npj Clean Air, 2025.',
      href: 'https://doi.org/10.1038/s44407-025-00037-2',
    },
    {
      img: '/images/feature_dashstat.png',
      alt: 'Sigma textbook for Six Sigma in R and Python',
      title: 'Sigma — open textbook',
      desc: 'System Reliability & Six Sigma in R and Python. ~28 chapters, dual-language, with built-in learning checks. Cornell SYSEN 5300.',
      href: site.links.sigma,
    },
  ];

  return (
    <div className="wrap">
      {/* HERO */}
      <section className="hero reveal d1">
        <div>
          <h1>Timothy Fraser, PhD</h1>
          <p className="hero-role">
            <b>Assistant Teaching Professor</b>, Systems Engineering · Cornell University<br />
            Coordinator, CTECH · Ithaca, NY
          </p>
          <div className="hero-bio">
            <Markdown>{markdownPages.home}</Markdown>
          </div>
          <div className="profile-links">
            <a href={site.links.cv}>CV</a><span className="sep">·</span>
            <a href={site.links.scholar}>Scholar</a><span className="sep">·</span>
            <a href={site.links.linkedin}>LinkedIn</a><span className="sep">·</span>
            <a href={site.links.orcid}>ORCID</a><span className="sep">·</span>
            <a href={site.links.github}>GitHub</a><span className="sep">·</span>
            <a href={`mailto:${site.email}`}>{site.email}</a>
          </div>
          <p style={{ marginTop: 14, fontSize: '0.85rem', color: 'var(--muted)' }}>
            {metrics.peer_reviewed}+ peer-reviewed papers · h-index {metrics.h_index} · {metrics.citations?.toLocaleString()} citations ({metrics.as_of})
          </p>
        </div>
        <div className="hero-portrait">
          <img src="/images/headshot.jpg" alt="Portrait of Timothy Fraser" />
        </div>
      </section>

      {/* WORK TILES */}
      <section className="section reveal d2">
        <div className="section-head">
          <h2>Selected work</h2>
          <p className="subhead">Open dashboards, an open textbook, and a recent paper on the air-quality impacts of NYC's congestion-pricing cordon.</p>
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

      {/* RECRUITING — no card chrome */}
      <section className="section reveal d3">
        <div className="section-head">
          <h2>MEng projects — recruiting</h2>
          <p className="subhead">
            Three projects on cities, air, and movement. Cornell MEng students welcome —
            <a href={`mailto:${site.email}?subject=MEng%20interest`} style={{ marginLeft: 4 }}>email me</a>.
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
      <section className="section reveal d4">
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
