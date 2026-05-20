import type { ReactNode } from 'react';

interface Props {
  kicker: string;
  title: ReactNode;
  lede?: ReactNode;
  subline?: ReactNode;
}

export default function SectionMast({ kicker, title, lede, subline }: Props) {
  return (
    <header className="section-mast reveal d1">
      <div className="kicker">{kicker}</div>
      <h1>{title}</h1>
      {subline && <div className="sub">{subline}</div>}
      {lede && <p className="lede">{lede}</p>}
    </header>
  );
}
