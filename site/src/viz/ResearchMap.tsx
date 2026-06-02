import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { researchSites } from '../data/loaders';
import type { ResearchSite } from '../data/types';

/**
 * Research footprint map with a guided tour: Next steps through each site,
 * zooming to continent-level bounds and highlighting the active marker.
 */

const TOPIC_COLOR: Record<string, string> = {
  environment: '#2d6a4f',
  transportation: '#1d4e89',
  disaster: '#c45c26',
  resilience: '#6b4c9a',
  social_infrastructure: '#7b5ea7',
  social_capital: '#6b4c9a',
  polarization: '#9b2335',
  energy: '#b8860b',
  health: '#c23b4a',
  networks: '#5c4d7d',
  home: '#b31b1b',
};

const ACCENT = '#b31b1b';

function isValidCoord(lat: number, lng: number) {
  return Number.isFinite(lat) && Number.isFinite(lng) && Math.abs(lat) <= 90 && Math.abs(lng) <= 180;
}

/** Continent-ish bounds for flyTo when touring a site. */
function continentBounds(lat: number, lng: number): L.LatLngBounds {
  // East Asia / Japan
  if (lng >= 120 && lat >= 25) {
    return L.latLngBounds([28, 127], [46, 146]);
  }
  // South America
  if (lat < 15 && lng >= -82 && lng <= -35) {
    return L.latLngBounds([-36, -78], [-18, -48]);
  }
  // Sub-Saharan Africa (incl. South Africa case study)
  if (lat < 10 && lng >= 8 && lng <= 42) {
    return L.latLngBounds([-35, 12], [-18, 36]);
  }
  // Mexico & Central America
  if (lat >= 10 && lat < 24 && lng >= -118 && lng <= -86) {
    return L.latLngBounds([14, -110], [24, -86]);
  }
  // North America (US / Canada)
  if (lng >= -130 && lng <= -60 && lat >= 20) {
    return L.latLngBounds([24, -125], [52, -66]);
  }
  // Fallback: padded box around the point
  return L.latLngBounds([lat - 7, lng - 10], [lat + 7, lng + 10]);
}

function allSitesBounds(sites: ResearchSite[]): L.LatLngBounds | null {
  const pts = sites.filter(s => isValidCoord(s.lat, s.lng));
  if (!pts.length) return null;
  return L.latLngBounds(pts.map(s => [s.lat, s.lng] as L.LatLngTuple));
}

interface SiteMarker {
  site: ResearchSite;
  marker: L.CircleMarker;
}

function markerStyle(site: ResearchSite, active: boolean) {
  const isHome = site.topic === 'home';
  return {
    radius: active ? (isHome ? 14 : 11) : (isHome ? 10 : 7),
    color: active ? ACCENT : '#ffffff',
    weight: active ? 3 : 2,
    fillColor: TOPIC_COLOR[site.topic] || '#4a5568',
    fillOpacity: active ? 1 : 0.38,
  };
}

export default function ResearchMap({ height = 360 }: { height?: number }) {
  const elRef = useRef<HTMLDivElement | null>(null);
  const mapRef = useRef<L.Map | null>(null);
  const markersRef = useRef<SiteMarker[]>([]);
  const reducedMotionRef = useRef(false);

  const sites = useMemo(
    () => researchSites.filter(s => isValidCoord(s.lat, s.lng)),
    [],
  );

  const [activeIndex, setActiveIndex] = useState(0);
  const [mapReady, setMapReady] = useState(false);
  const active = sites[activeIndex] ?? sites[0];

  const focusSite = useCallback((index: number, map: L.Map, markers: SiteMarker[]) => {
    const site = sites[index];
    if (!site) return;

    markers.forEach((m, i) => {
      m.marker.setStyle(markerStyle(m.site, i === index));
      if (i === index) m.marker.openTooltip();
      else m.marker.closeTooltip();
    });

    const bounds = continentBounds(site.lat, site.lng);
    const opts: L.FitBoundsOptions = { padding: [44, 44], maxZoom: 6, animate: !reducedMotionRef.current };
    if (reducedMotionRef.current) {
      map.fitBounds(bounds, opts);
    } else {
      map.flyToBounds(bounds, { ...opts, duration: 1.1 });
    }
  }, [sites]);

  const goNext = useCallback(() => {
    setActiveIndex(i => (i + 1) % sites.length);
  }, [sites.length]);

  const showOverview = useCallback(() => {
    const map = mapRef.current;
    const bounds = allSitesBounds(sites);
    if (!map || !bounds) return;
    markersRef.current.forEach(m => {
      m.marker.setStyle(markerStyle(m.site, false));
      m.marker.closeTooltip();
    });
    const opts: L.FitBoundsOptions = { padding: [36, 36], maxZoom: 4, animate: !reducedMotionRef.current };
    if (reducedMotionRef.current) map.fitBounds(bounds, opts);
    else map.flyToBounds(bounds, { ...opts, duration: 1.1 });
  }, [sites]);

  // Init map once
  useEffect(() => {
    const container = elRef.current;
    if (!container) return;

    setMapReady(false);
    reducedMotionRef.current = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    const map = L.map(container, {
      worldCopyJump: true,
      scrollWheelZoom: false,
      minZoom: 1,
      maxZoom: 10,
    });
    mapRef.current = map;

    L.tileLayer('https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png', {
      subdomains: 'abcd',
      maxZoom: 19,
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
    }).addTo(map);

    const markers: SiteMarker[] = [];
    for (const site of sites) {
      const marker = L.circleMarker([site.lat, site.lng], markerStyle(site, false))
        .addTo(map)
        .bindTooltip(
          `<strong>${site.label}</strong>${site.year ? ` · ${site.year}` : ''}<br>${site.blurb}`,
          { direction: 'top', offset: [0, -6], sticky: true },
        );
      markers.push({ site, marker });
    }
    markersRef.current = markers;

    const bounds = allSitesBounds(sites);
    if (bounds) map.fitBounds(bounds, { padding: [36, 36], maxZoom: 4 });
    else map.setView([20, 0], 2);

    const syncSize = () => map.invalidateSize();
    requestAnimationFrame(syncSize);
    const t = window.setTimeout(syncSize, 120);

    const ro = new ResizeObserver(syncSize);
    ro.observe(container);

    setMapReady(true);

    return () => {
      window.clearTimeout(t);
      ro.disconnect();
      map.remove();
      mapRef.current = null;
      markersRef.current = [];
      setMapReady(false);
    };
  }, [sites]);

  // Tour step + initial focus when map is ready
  useEffect(() => {
    if (!mapReady) return;
    const map = mapRef.current;
    const markers = markersRef.current;
    if (!map || !markers.length || !sites.length) return;
    focusSite(activeIndex, map, markers);
  }, [activeIndex, mapReady, focusSite, sites.length]);

  return (
    <div className="research-map">
      <div className="research-map-head">
        <div className="eyebrow">Research sites · {sites.length} locations</div>
        <div className="research-map-tour" aria-live="polite">
          <button type="button" className="btn ghost research-map-overview" onClick={showOverview}>
            Overview
          </button>
          <button type="button" className="btn accent research-map-next" onClick={goNext}>
            Next site →
          </button>
        </div>
      </div>

      {active && (
        <div className="research-map-spotlight">
          <strong>{active.label}</strong>
          {active.year ? ` · ${active.year}` : ''}
          <span className="research-map-spotlight-meta"> — {active.blurb}</span>
          <span className="research-map-counter">
            {activeIndex + 1} / {sites.length}
          </span>
        </div>
      )}

      <div
        ref={elRef}
        className="research-map-canvas"
        style={{ height }}
        role="img"
        aria-label={`Map tour of ${sites.length} research sites. Currently showing ${active?.label ?? 'overview'}.`}
      />

      <details
        className="viz-fallback"
        style={{ marginTop: 14, border: '1px dashed var(--line)', padding: '12px 14px', borderRadius: 3 }}
      >
        <summary>Research sites (text)</summary>
        <ul style={{ margin: '10px 0 4px 18px' }}>
          {sites.map((s, i) => (
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
