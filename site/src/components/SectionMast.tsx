import type { ReactNode } from 'react';

interface Props {
  eyebrow?: string;
  title: ReactNode;
  subhead?: ReactNode;
}

export default function SectionMast({ eyebrow, title, subhead }: Props) {
  return (
    <header className="reveal d1" style={{ padding: '44px 0 24px' }}>
      {eyebrow && <div className="eyebrow">{eyebrow}</div>}
      <h1>{title}</h1>
      {subhead && <p className="subhead">{subhead}</p>}
    </header>
  );
}
