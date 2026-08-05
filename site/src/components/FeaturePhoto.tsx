import type { ReactNode } from 'react';

interface Props {
  src: string;
  alt: string;
  eyebrow?: string;
  title: ReactNode;
  children?: ReactNode;
  /** `split` puts the photo beside the copy; `banner` runs it full width above the copy. */
  layout?: 'split' | 'banner';
  /** Where to bias the crop when the photo is taller than its box. */
  focus?: string;
  links?: { label: string; url: string }[];
}

export default function FeaturePhoto({
  src, alt, eyebrow, title, children, layout = 'split', focus = 'center', links = [],
}: Props) {
  return (
    <figure className={`feature-photo ${layout}`}>
      <div className="feature-photo-img">
        <img src={src} alt={alt} style={{ objectPosition: focus }} loading="lazy" />
      </div>
      <figcaption className="feature-photo-body">
        {eyebrow && <div className="eyebrow">{eyebrow}</div>}
        <h3>{title}</h3>
        {children}
        {links.length > 0 && (
          <div className="feature-photo-links">
            {links.map(l => (
              <a key={l.url} className="btn ghost" href={l.url} target="_blank" rel="noopener noreferrer">
                {l.label} ↗
              </a>
            ))}
          </div>
        )}
      </figcaption>
    </figure>
  );
}
