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
        <div className="section-head">
          <h2>Closing Keynote — NSPE Professional Engineers Conference 2026</h2>
          <p className="subhead">
            National Society of Professional Engineers · New York City · August 7, 2026
          </p>
        </div>
        <FeaturePhoto
          src="/images/nspe_keynote_stage.jpg"
          alt="Timothy Fraser delivering the closing keynote at the National Society of Professional Engineers Professional Engineers Conference 2026 in New York City"
          layout="banner"
          eyebrow="Closing Keynote · NSPE Professional Engineers Conference 2026"
          title={<>&ldquo;Urban congestion pricing: impacts on air quality and public health in NYC&rdquo;</>}
          focus="50% 35%"
          links={[
            { label: 'Read the npj Clean Air study', url: 'https://doi.org/10.1038/s44407-025-00037-2' },
          ]}
        >
          <p>
            On August 7, 2026, I closed the National Society of Professional Engineers'
            Professional Engineers Conference in New York City, speaking to the country's
            licensed engineering community about what the first year of Manhattan's cordon
            pricing program actually did — to traffic volumes, to PM<sub>2.5</sub>, and to the
            people who breathe the air downwind of it.
          </p>
          <p>
            The talk walked through the evidence we have, the evidence we do not, and what it
            takes to design a pricing program whose air quality and public health benefits can
            be measured rather than assumed.
          </p>
          <p style={{ fontSize: '0.78rem', letterSpacing: '0.04em', color: 'var(--muted)', marginTop: 16 }}>
            Photo by Corpora Studios
          </p>
        </FeaturePhoto>

        <figure
          className="card"
          style={{
            display: 'flex',
            gap: 20,
            alignItems: 'center',
            margin: '18px 0 0',
            padding: 18,
            flexWrap: 'wrap',
          }}
        >
          <img
            src="/images/nspe_keynote_closeup.jpg"
            alt="Timothy Fraser holding a microphone while delivering the NSPE closing keynote"
            loading="lazy"
            style={{
              width: 220,
              maxWidth: '100%',
              aspectRatio: '3 / 2',
              objectFit: 'cover',
              objectPosition: '50% 30%',
              borderRadius: 'var(--radius)',
              border: '1px solid var(--line)',
              display: 'block',
            }}
          />
          <figcaption style={{ flex: '1 1 260px', minWidth: 0 }}>
            <div className="eyebrow">New York City · August 2026</div>
            <p style={{ fontSize: '0.95rem', lineHeight: 1.6, color: 'var(--ink-2)', margin: '0 0 10px', maxWidth: '52ch' }}>
              Closing out the NSPE Professional Engineers Conference — a national audience of
              licensed engineers, on how congestion pricing shows up in air quality data.
            </p>
            <div style={{ fontSize: '0.78rem', letterSpacing: '0.04em', color: 'var(--muted)' }}>
              Photo by Corpora Studios
            </div>
          </figcaption>
        </figure>
      </section>

      <section className="section reveal d3">
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

      <section className="section reveal d4">
        <Markdown>{markdownPages.about}</Markdown>
      </section>
    </div>
  );
}
