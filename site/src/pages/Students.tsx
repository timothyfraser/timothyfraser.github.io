import SectionMast from '../components/SectionMast';
import { students } from '../data/loaders';

export default function Students() {
  const current = students.filter(s => s.current);
  const past = students.filter(s => !s.current);

  return (
    <div className="wrap">
      <SectionMast
        eyebrow="Students"
        title="Research with students"
        subhead="34+ students, 13+ peer-reviewed coauthored papers. Cornell MEng teams are recruiting now."
      />

      <section className="section">
        <div className="section-head">
          <h2>Current Cornell teams</h2>
        </div>
        {current.map((s, i) => (
          <div className="row" key={i}>
            <div className="row-meta">{s.level} · {s.institution}</div>
            <div className="row-title">{s.team}</div>
            <div className="row-sub">{s.name === s.team ? '—' : s.name} · {s.outputs}</div>
          </div>
        ))}
      </section>

      <section className="section">
        <div className="section-head">
          <h2>Past student collaborations</h2>
        </div>
        {past.map((s, i) => (
          <div className="row" key={i}>
            <div className="row-meta">{s.level} · {s.institution}</div>
            <div className="row-title">{s.team}</div>
            <div className="row-sub">{s.name === s.team ? '—' : s.name} · {s.outputs}</div>
          </div>
        ))}
      </section>
    </div>
  );
}
