import type { ReactNode } from 'react';

interface CardProps {
  featured?: boolean;
  className?: string;
  children: ReactNode;
}
export default function Card({ featured, className = '', children }: CardProps) {
  return (
    <article className={`card${featured ? ' featured' : ''} ${className}`.trim()}>
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
      <div style={{ flex: 1, minWidth: 0 }}>
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

export function CardFigure({ src, alt }: { src: string; alt: string }) {
  return (
    <figure className="card-figure">
      <img src={src} alt={alt} loading="lazy" />
    </figure>
  );
}
