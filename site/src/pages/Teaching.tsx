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
        kicker="Teaching · active learning, public textbooks"
        title={<>Teaching as <em>open infrastructure</em>.</>}
        lede="Two-thirds of class time spent on workshops, labs, and team projects — plus a growing library of open textbooks, course sites, and R/Python tutorials anyone can use."
      />

      <section className="cluster reveal d2">
        <div className="kicker" style={{ marginBottom: 16 }}>Featured open resources</div>
        <div className="grid-2">
          {featured.map(r => (
            <Card key={r.id} accent={(r.accent as any) || 'signal'}>
              <CardHead
                id={r.kind}
                name={r.name}
                tag={r.blurb}
              />
              <CardBody>
                <div className="chips">
                  {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
                </div>
                <p style={{ marginTop: 12 }}>
                  <a className="btn" href={r.url}>Open →</a>
                </p>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 16 }}>More tutorials & workshops</div>
        <div className="grid-2">
          {other.map(r => (
            <Card key={r.id} accent="gold">
              <CardHead id={r.kind} name={r.name} tag={r.blurb} />
              <CardBody>
                <div className="chips">
                  {r.tags.map(t => <span className="chip" key={t}>{t}</span>)}
                </div>
                <p style={{ marginTop: 12 }}>
                  <a className="btn ghost" href={r.url}>Open →</a>
                </p>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>

      <section className="cluster">
        <div className="kicker" style={{ marginBottom: 14 }}>Courses taught</div>
        <table style={{ width: '100%', borderCollapse: 'collapse', marginTop: 8 }}>
          <thead>
            <tr style={{ textAlign: 'left' }}>
              <th style={th}>Code</th>
              <th style={th}>Title</th>
              <th style={th}>Level</th>
              <th style={th}>Institution</th>
              <th style={th}>Years</th>
            </tr>
          </thead>
          <tbody>
            {teaching.courses_taught.map(c => (
              <tr key={c.code} style={{ borderBottom: '1px dashed var(--line)' }}>
                <td style={td}><span className="mono">{c.code}</span></td>
                <td style={{ ...td, fontFamily: 'var(--font-display)', fontWeight: 600 }}>{c.title}</td>
                <td style={td}>{c.level}</td>
                <td style={td}>{c.inst}</td>
                <td style={td}>{c.years.join(', ')}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>

      <section className="cluster reveal d3">
        <Markdown>{markdownPages.teaching}</Markdown>
      </section>
    </div>
  );
}

const th = {
  textAlign: 'left' as const,
  padding: '10px 6px',
  borderBottom: '2px solid var(--ink)',
  fontFamily: 'var(--font-mono)',
  fontSize: '0.72rem',
  letterSpacing: '0.14em',
  textTransform: 'uppercase' as const,
  color: 'var(--muted)',
};
const td = {
  padding: '10px 6px',
  fontSize: '0.92rem',
  verticalAlign: 'top' as const,
};
