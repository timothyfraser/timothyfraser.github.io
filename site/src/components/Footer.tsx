import { site, metrics } from '../data/loaders';

export default function Footer() {
  const builtDate = metrics?.as_of ?? '';
  return (
    <footer className="site-footer">
      <div className="site-footer-inner">
        <div className="col">
          <h4>Timothy Fraser, PhD</h4>
          <p className="tagline">Data, air, and movement.</p>
          <p className="mono" style={{ marginTop: 8 }}>
            {site.affiliation}<br />
            {site.institution} · {site.location}
          </p>
          <p className="meta">Updated {builtDate || '—'}</p>
        </div>
        <div className="col">
          <h4>Sites</h4>
          <ul>
            <li><a href={site.links.cv}>CV</a></li>
            <li><a href={site.links.sigma}>Sigma (textbook)</a></li>
            <li><a href={site.links.netsci}>NetSci (course)</a></li>
            <li><a href={site.links.cat}>CAT dashboard</a></li>
            <li><a href={site.links.rpubs}>RPubs tutorials</a></li>
          </ul>
        </div>
        <div className="col">
          <h4>Elsewhere</h4>
          <ul>
            <li><a href={site.links.github}>GitHub</a></li>
            <li><a href={site.links.scholar}>Google Scholar</a></li>
            <li><a href={site.links.linkedin}>LinkedIn</a></li>
            <li><a href={site.links.dataverse}>Harvard Dataverse</a></li>
            <li><a href={site.links.researchgate}>ResearchGate</a></li>
            <li><a href={`mailto:${site.email}`}>{site.email}</a></li>
          </ul>
        </div>
      </div>
    </footer>
  );
}
