import type { ReactNode } from 'react';

interface CardProps {
  accent?: 'cool' | 'hot' | 'signal' | 'gold' | 'cornell';
  className?: string;
  children: ReactNode;
}
export default function Card({ accent, className = '', children }: CardProps) {
  return (
    <article className={`card ${accent ? `accent-${accent}` : ''} ${className}`.trim()}>
      {children}
    </article>
  );
}

export function CardHead({ id, name, tag, badge }: {
  id?: string;
  name: ReactNode;
  tag?: ReactNode;
  badge?: ReactNode;
}) {
  return (
    <div className="card-head">
      <div>
        {id && <div className="card-id">{id}</div>}
        <h2 className="card-name">{name}</h2>
        {tag && <div className="card-tag">{tag}</div>}
      </div>
      {badge}
    </div>
  );
}

export function CardBody({ children }: { children: ReactNode }) {
  return <div className="card-body">{children}</div>;
}
