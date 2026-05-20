import type { ReactNode } from 'react';

interface Props {
  value: ReactNode;
  label: string;
  source?: { label: string; href: string; external?: boolean };
}

export default function StatBox({ value, label, source }: Props) {
  return (
    <div className="statbox">
      <div className="v">{value}</div>
      <div className="l">{label}</div>
      {source && (
        source.external
          ? <a className="src" href={source.href} target="_blank" rel="noopener noreferrer">{source.label} →</a>
          : <a className="src" href={source.href}>{source.label} →</a>
      )}
    </div>
  );
}
