import { useEffect, useRef } from 'react';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { researchSites } from '../data/loaders';

/**
 * Research footprint on a real slippy map. Base tiles are CARTO Positron
 * (light, label-rich) served from the CARTO CDN; points are vector
 * CircleMarkers colored by research topic, so there are no marker-icon
 * asset dependencies to break under a hashed build.
 */

const TOPIC_COLOR: Record<string, string> = {
  environment: 'oklch(50% 0.12 160)',
  transportation: 'oklch(60% 0.16 40)',
  disaster: 'oklch(50% 0.12 240)',
  resilience: 'oklch(50% 0.12 300)',
  social_infrastructure: 'oklch(50% 0.12 300)',
  social_capital: 'oklch(50% 0.12 300)',
  polarization: 'oklch(60% 0.13 80)',
  energy: 'oklch(55% 0.14 60)',
  health: 'oklch(50% 0.13 10)',
  networks: 'oklch(52% 0.10 220)',
  home: 'oklch(48% 0.18 28)',
};

export default function ResearchMap({ height = 360 }: { height?: number }) {
  const elRef = useRef<HTMLDivElement | null>(null);
  const mapRef = useRef<L.Map | null>(null);

  useEffect(() => {
    if (!elRef.current || mapRef.current) return;

    const map = L.map(elRef.current, {
      worldCopyJump: true,
      scrollWheelZoom: false,
      minZoom: 1,
    });
    mapRef.current = map;

    L.tileLayer(
      'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',
      {
        subdomains: 'abcd',
        maxZoom: 19,
        attribution:
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
      },
    ).addTo(map);

    const latlngs: L.LatLngExpression[] = [];
    for (const s of researchSites) {
      const color = TOPIC_COLOR[s.topic] || 'oklch(22% 0.02 260)';
      const isHome = s.topic === 'home';
      latlngs.push([s.lat, s.lng]);
      L.circleMarker([s.lat, s.lng], {
        radius: isHome ? 9 : 6,
        color: '#ffffff',
        weight: 1.5,
        fillColor: color,
        fillOpacity: 0.9,
      })
        .addTo(map)
        .bindTooltip(
          `<strong>${s.label}</strong>${s.year ? ` · ${s.year}` : ''}<br>${s.blurb}`,
          { direction: 'top', offset: [0, -4] },
        );
    }

    if (latlngs.length) {
      map.fitBounds(L.latLngBounds(latlngs), { padding: [30, 30], maxZoom: 5 });
    } else {
      map.setView([20, 0], 2);
    }

    // Container is sized after mount; make sure Leaflet measures correctly.
    setTimeout(() => map.invalidateSize(), 0);

    return () => {
      map.remove();
      mapRef.current = null;
    };
  }, []);

  return (
    <div className="research-map">
      <div className="eyebrow" style={{ marginBottom: 12 }}>
        Research sites · {researchSites.length} locations
      </div>
      <div
        ref={elRef}
        style={{
          height,
          width: '100%',
          borderRadius: 3,
          border: '1px solid var(--line)',
          overflow: 'hidden',
          isolation: 'isolate',
        }}
        role="img"
        aria-label={`Map of ${researchSites.length} research sites across the Americas, East Asia, and beyond`}
      />

      <details
        className="viz-fallback"
        style={{ marginTop: 14, border: '1px dashed var(--line)', padding: '12px 14px', borderRadius: 3 }}
      >
        <summary>Research sites (text)</summary>
        <ul style={{ margin: '10px 0 4px 18px' }}>
          {researchSites.map((s, i) => (
            <li key={i}>
              <strong>{s.label}</strong> — {s.blurb}
              {s.year ? ` (${s.year})` : ''}
            </li>
          ))}
        </ul>
      </details>
    </div>
  );
}
