import type { CSSProperties } from 'react';
import { VizEmbed } from './viz/VizEmbed';

export default function PublicationsShelfSection({
  className = '',
  style,
}: {
  className?: string;
  style?: CSSProperties;
}) {
  return (
    <section className={`section ${className}`.trim()} style={style}>
      <div className="section-head">
        <h2>Browse the shelf</h2>
      </div>
      <VizEmbed
        widget="publicationsShelf"
        title="Publications shelf"
        height={880}
        autoHeight
      />
    </section>
  );
}
