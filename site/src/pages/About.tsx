import Markdown from '../components/Markdown';
import { markdownPages, site } from '../data/loaders';

export default function About() {
  return (
    <div className="wrap">
      <section className="hero reveal d1">
        <div>
          <div className="eyebrow">About</div>
          <h1>Timothy Fraser, PhD</h1>
          <p className="hero-role">
            <b>Assistant Teaching Professor</b>, Systems Engineering · Cornell University<br />
            Coordinator, Center for Transportation, Environment, and Community Health (CTECH)
          </p>
          <div className="profile-links">
            <a href={site.links.cv}>CV</a><span className="sep">·</span>
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
      </section>

      <section className="section reveal d2">
        <Markdown>{markdownPages.about}</Markdown>
      </section>
    </div>
  );
}
