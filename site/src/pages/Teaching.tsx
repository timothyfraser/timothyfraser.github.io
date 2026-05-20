import SectionMast from '../components/SectionMast';
import Markdown from '../components/Markdown';
import Card, { CardBody, CardHead } from '../components/Card';
import { markdownPages, teaching } from '../data/loaders';

export default function Teaching() {
  const featured = teaching.resources.filter(r => r.featured);
  const other = teaching.resources.filter(r => !r.featured);

  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Teaching"
        title="Open courses, open textbooks"
        subhead="Two-thirds of class time on workshops, labs, and team projects — plus a growing library of open textbooks, course sites, and R/Python tutorials."
      />

      <section className="section">
        <div className="section-head">
          <h2>Featured open resources</h2>
        </div>
        <div className="grid-2">
          {featured.map(r => (
            <Card key={r.id} featured>
              <CardHead id={r.kind} name={r.name} tag={r.blurb} />
              <CardBody>
                <div className="chips">
                  {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
                </div>
                <p style={{ marginTop: 14 }}>
                  <a className="btn" href={r.url}>Open →</a>
                </p>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>

      <section className="section">
        <div className="section-head">
          <h2>More tutorials & workshops</h2>
        </div>
        <div className="grid-2">
          {other.map(r => (
            <Card key={r.id}>
              <CardHead id={r.kind} name={r.name} tag={r.blurb} />
              <CardBody>
                <div className="chips">
                  {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
                </div>
                <p style={{ marginTop: 14 }}>
                  <a className="btn ghost" href={r.url}>Open →</a>
                </p>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Courses taught</h2>
        </div>
        <table style={{ width: '100%', borderCollapse: 'collapse', marginTop: 8 }}>
          <thead>
            <tr>
              <th style={th}>Code</th>
              <th style={th}>Title</th>
              <th style={th}>Level</th>
              <th style={th}>Institution</th>
              <th style={th}>Years</th>
            </tr>
          </thead>
          <tbody>
            {teaching.courses_taught.map(c => (
              <tr key={c.code} style={{ borderBottom: '1px solid var(--line)' }}>
                <td style={td}>{c.code}</td>
                <td style={{ ...td, fontFamily: 'var(--font-display)', fontWeight: 600 }}>{c.title}</td>
                <td style={td}>{c.level}</td>
                <td style={td}>{c.inst}</td>
                <td style={td}>{c.years.join(', ')}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>

      <section className="section">
        <div className="prose">
          <Markdown>{markdownPages.teaching}</Markdown>
        </div>
      </section>
    </div>
  );
}

const th = {
  textAlign: 'left' as const,
  padding: '10px 6px',
  borderBottom: '2px solid var(--ink)',
  fontSize: '0.78rem',
  fontWeight: 600,
  color: 'var(--muted)',
};
const td = {
  padding: '10px 6px',
  fontSize: '0.94rem',
  verticalAlign: 'top' as const,
};
