import { useEffect, useMemo, useState } from 'react';

const WIDGETS = {  publicationsShelf: 'publications-shelf.html',
  softwareConveyor: 'software-conveyor.html',
  scholarPulse: 'scholar-pulse.html',
  pressRibbon: 'press-ribbon.html',
};

const VIZ_BASE = import.meta.env.VITE_VIZ_BASE ?? '/viz-widgets';

export function VizEmbed({
  widget,
  title,
  height = 520,
  autoHeight = false,
  className = '',
}) {
  const [frameHeight, setFrameHeight] = useState(height);

  const src = useMemo(() => {
    const file = WIDGETS[widget];
    if (!file) throw new Error(`Unknown widget: ${widget}`);
    const base = VIZ_BASE.replace(/\/$/, '');
    return `${base}/widgets/${file}?embed=1`;
  }, [widget]);

  useEffect(() => {
    setFrameHeight(height);
  }, [height, src]);

  useEffect(() => {
    if (!autoHeight) return;
    function onMessage(event) {
      if (event.data?.type !== 'od-viz-resize') return;
      if (typeof event.data.height !== 'number') return;
      setFrameHeight(Math.max(event.data.height, height));
    }
    window.addEventListener('message', onMessage);
    return () => window.removeEventListener('message', onMessage);
  }, [autoHeight, height]);

  return (
    <section
      className={`viz-embed section ${className}`.trim()}
      aria-label={title}
      data-od-viz={widget}
    >
      <iframe
        src={src}
        title={title}
        width="100%"
        height={frameHeight}
        style={{ border: 0, display: 'block', maxWidth: '100%', overflow: 'hidden' }}
        loading="lazy"
        referrerPolicy="no-referrer-when-downgrade"
        scrolling="no"
      />
    </section>
  );
}

export { WIDGETS, VIZ_BASE };
