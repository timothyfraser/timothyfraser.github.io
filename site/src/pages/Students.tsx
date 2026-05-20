import SectionMast from '../components/SectionMast';
import { students } from '../data/loaders';

export default function Students() {
  const current = students.filter(s => s.current);
  const past = students.filter(s => !s.current);

  return (
    <div className="wrap">
      <SectionMast
        kicker="Students · current teams + past collaborations"
        title={<>Research with <em>students</em>.</>}
        lede="34+ students, 13+ peer-reviewed coauthored papers. Cornell MEng teams are recruiting now."
      />

      <section>
        <div className="kicker" style={{ marginBottom: 14 }}>Current Cornell teams</div>
        {current.map((s, i) => (
          <div className="row" key={i}>
            <div className="row-meta">{s.level} · {s.institution}</div>
            <div className="row-title">{s.team}</div>
            <div className="row-sub">{s.name === s.team ? '—' : s.name} · {s.outputs}</div>
          </div>
        ))}
      </section>

      <section style={{ marginTop: 50 }}>
        <div className="kicker" style={{ marginBottom: 14 }}>Past student collaborations</div>
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
