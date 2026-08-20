import SectionMast from '../components/SectionMast';
import Markdown from '../components/Markdown';
import Card, { CardBody, CardHead, CardFigure } from '../components/Card';
import { courses, markdownPages, teaching } from '../data/loaders';

const LEVEL_LABELS: Record<string, string> = {
  grad: 'Graduate',
  undergrad: 'Undergraduate',
};

export default function Teaching() {
  const featured = teaching.resources.filter(r => r.featured);
  const other = teaching.resources.filter(r => !r.featured);

  // The old "Courses taught" table is folded into the course cards: years ride
  // along in each card's meta line, and anything without a card is listed under
  // "Also taught" so no course is dropped.
  const cardCodes = new Set(courses.map(c => c.code));
  const yearsByCode = new Map<string, number[]>(
    teaching.courses_taught.map(c => [c.code, c.years] as [string, number[]]),
  );
  const alsoTaught = teaching.courses_taught.filter(c => !cardCodes.has(c.code));

  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Teaching"
        title="Open courses, open textbooks"
        subhead="Two-thirds of class time on workshops, labs, and team projects — plus a growing library of open textbooks, course sites, and R/Python tutorials."
      />

      <section className="section">
        <div className="section-head">
          <h2>Courses I teach</h2>
          <p className="subhead">
            Graduate courses in the Cornell Systems Engineering program, plus a winter statistics
            boot camp. Every course is backed by an open repository of workshops, data, and code.
          </p>
        </div>
        <div className="grid-2">
          {courses.map(c => {
            const years = yearsByCode.get(c.code);
            return (
              <Card key={c.id} featured>
                <CardFigure src={c.image} alt={`${c.code} — ${c.title}`} />
                <CardHead
                  id={c.code}
                  name={c.title}
                  tag={c.tagline}
                  badge={<span className="badge accent">{c.term}</span>}
                />
                <CardBody>
                  <details className="course-card-desc">
                    <summary>
                      <span className="course-card-desc-open">Read full course description</span>
                      <span className="course-card-desc-close">Hide full course description</span>
                    </summary>
                    <p className="course-card-desc-body">{c.description}</p>
                  </details>
                  <div className="chips">
                    {c.tags.map(t => <span className="chip" key={t}>{t}</span>)}
                  </div>
                  <p style={{ marginTop: 16, display: 'flex', flexWrap: 'wrap', gap: 8 }}>
                    {c.links.map(l => (
                      <a
                        key={l.url}
                        className={`btn${l.primary ? '' : ' ghost'}`}
                        href={l.url}
                      >
                        {l.label} →
                      </a>
                    ))}
                  </p>
                  <div className="card-meta">
                    {c.level} · {c.inst}
                    {years && years.length > 0 && <> · Taught {years.join(', ')}</>}
                  </div>
                </CardBody>
              </Card>
            );
          })}
        </div>

        {alsoTaught.length > 0 && (
          <>
            <h3 className="course-also-head">Also taught</h3>
            <ul className="course-also-list">
              {alsoTaught.map(c => (
                <li className="course-also-row" key={c.code}>
                  <div>
                    <div className="course-also-code">{c.code}</div>
                    <div className="course-also-title">{c.title}</div>
                  </div>
                  <div className="course-also-meta">
                    {LEVEL_LABELS[c.level] ?? c.level} · {c.inst} · {c.years.join(', ')}
                  </div>
                </li>
              ))}
            </ul>
          </>
        )}
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Featured open resources</h2>
          <p className="subhead">
            Not courses — the open textbooks, course sites, and browser-based tools behind them.
            Free to read, run, fork, and teach from.
          </p>
        </div>
        <div className="grid-2">
          {featured.map(r => (
            <a className="resource-card is-featured" key={r.id} href={r.url}>
              <span className="resource-card-kind">{r.kind}</span>
              <span className="resource-card-name">{r.name}</span>
              <span className="resource-card-blurb">{r.blurb}</span>
              <span className="chips">
                {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
              </span>
              <span className="resource-card-cta">Open →</span>
            </a>
          ))}
        </div>
      </section>

      <section className="section">
        <div className="section-head">
          <h2>More tutorials &amp; workshops</h2>
        </div>
        <div className="grid-2">
          {other.map(r => (
            <a className="resource-card" key={r.id} href={r.url}>
              <span className="resource-card-kind">{r.kind}</span>
              <span className="resource-card-name">{r.name}</span>
              <span className="resource-card-blurb">{r.blurb}</span>
              <span className="chips">
                {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
              </span>
              <span className="resource-card-cta">Open →</span>
            </a>
          ))}
        </div>
      </section>

      <section className="section">
        <div className="prose">
          <Markdown>{markdownPages.teaching}</Markdown>
        </div>
      </section>
    </div>
  );
}
