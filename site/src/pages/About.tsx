import Markdown from '../components/Markdown';
import FeaturePhoto from '../components/FeaturePhoto';
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
        <FeaturePhoto
          src="/images/keynote_cornell_speaking.jpg"
          alt="Timothy Fraser speaking on a Cornell Engineering panel about New York City's congestion pricing program"
          eyebrow="Public engagement"
          title="Taking congestion pricing to a public audience"
          focus="40% 30%"
          links={[{ label: 'Read the npj Clean Air study', url: 'https://doi.org/10.1038/s44407-025-00037-2' }]}
        >
          <p>
            A model, a dashboard, or a paper only counts for something once people outside
            the field can act on it. On a Cornell Engineering panel about New York City's
            congestion pricing program, I walked through what the first months of the cordon
            did to traffic and to air quality, and where the evidence is still thin.
          </p>
        </FeaturePhoto>
      </section>

      <section className="section reveal d3">
        <Markdown>{markdownPages.about}</Markdown>
      </section>
    </div>
  );
}
