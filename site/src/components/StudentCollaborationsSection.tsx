import { students } from '../data/loaders';

export default function StudentCollaborationsSection() {
  const current = students.filter(s => s.current);
  const past = students.filter(s => !s.current);

  return (
    <section className="section" id="students">
      <div className="section-head">
        <h2>Research with students</h2>
        <p className="subhead">
          34+ students, 13+ peer-reviewed coauthored papers. Cornell MEng teams are recruiting now.
        </p>
      </div>

      <div className="section-head" style={{ marginTop: 8 }}>
        <h3 style={{ fontFamily: 'var(--font-display)', fontSize: '1.15rem', fontWeight: 600, margin: 0 }}>
          Current Cornell teams
        </h3>
      </div>
      {current.map((s, i) => (
        <div className="row" key={i}>
          <div className="row-meta">{s.level} · {s.institution}</div>
          <div className="row-title">{s.team}</div>
          <div className="row-sub">{s.name === s.team ? '—' : s.name} · {s.outputs}</div>
        </div>
      ))}

      <div className="section-head" style={{ marginTop: 28 }}>
        <h3 style={{ fontFamily: 'var(--font-display)', fontSize: '1.15rem', fontWeight: 600, margin: 0 }}>
          Past student collaborations
        </h3>
      </div>
      {past.map((s, i) => (
        <div className="row" key={i}>
          <div className="row-meta">{s.level} · {s.institution}</div>
          <div className="row-title">{s.team}</div>
          <div className="row-sub">{s.name === s.team ? '—' : s.name} · {s.outputs}</div>
        </div>
      ))}
    </section>
  );
}
