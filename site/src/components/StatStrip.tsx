interface Stat {
  label: string;
  value: string | number;
  accent?: boolean;
}
export default function StatStrip({ stats }: { stats: Stat[] }) {
  return (
    <div className="stat-strip reveal d3">
      {stats.map((s, i) => (
        <div className="stat" key={i}>
          <div className="stat-val">{s.accent ? <em>{s.value}</em> : s.value}</div>
          <div className="stat-label">{s.label}</div>
        </div>
      ))}
    </div>
  );
}
