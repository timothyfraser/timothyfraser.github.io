import SectionMast from '../components/SectionMast';
import Card, { CardBody, CardHead, CardFigure } from '../components/Card';
import { courses } from '../data/loaders';

export default function Courses() {
  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Courses"
        title="Courses I teach"
        subhead="Graduate courses in the Cornell Systems Engineering program, plus a winter statistics boot camp. Every course is backed by an open repository of workshops, data, and code."
      />

      <section className="section">
        <div className="grid-2">
          {courses.map(c => (
            <Card key={c.id} featured>
              <CardFigure src={c.image} alt={`${c.code} — ${c.title}`} />
              <CardHead
                id={c.code}
                name={c.title}
                tag={c.tagline}
                badge={<span className="badge accent">{c.term}</span>}
              />
              <CardBody>
                <p>{c.description}</p>
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
                </div>
              </CardBody>
            </Card>
          ))}
        </div>
      </section>
    </div>
  );
}
