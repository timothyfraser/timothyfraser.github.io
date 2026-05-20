import { useEffect, useRef, useState } from 'react';
import * as d3 from 'd3';
import { coauthorGraph } from '../data/loaders';
import type { CoauthorNode, CoauthorEdge } from '../data/types';

const TOPIC_COLOR: Record<string, string> = {
  environment: 'var(--topic-environment)',
  transportation: 'var(--topic-transportation)',
  disaster: 'var(--topic-disaster)',
  resilience: 'var(--topic-resilience)',
  social_infrastructure: 'var(--topic-resilience)',
  polarization: 'var(--topic-polarization)',
  energy: 'var(--topic-energy)',
  health: 'var(--topic-health)',
  networks: 'var(--topic-networks)',
  gis: 'var(--topic-networks)',
  other: 'var(--muted)',
};

interface Props {
  height?: number;
  showLegend?: boolean;
  showTable?: boolean;
}

interface SimNode extends CoauthorNode {
  x?: number;
  y?: number;
  vx?: number;
  vy?: number;
  fx?: number | null;
  fy?: number | null;
}

export default function CoauthorNetwork({ height = 540, showLegend = true, showTable = true }: Props) {
  const ref = useRef<SVGSVGElement | null>(null);
  const wrapRef = useRef<HTMLDivElement | null>(null);
  const [hovered, setHovered] = useState<string | null>(null);
  const [reduced, setReduced] = useState(false);

  useEffect(() => {
    const mq = window.matchMedia('(prefers-reduced-motion: reduce)');
    setReduced(mq.matches);
    const onChange = () => setReduced(mq.matches);
    mq.addEventListener?.('change', onChange);
    return () => mq.removeEventListener?.('change', onChange);
  }, []);

  useEffect(() => {
    if (!ref.current || !wrapRef.current) return;
    const svg = d3.select(ref.current);
    svg.selectAll('*').remove();

    const width = wrapRef.current.clientWidth;
    const cx = width / 2;
    const cy = height / 2;

    svg.attr('viewBox', `0 0 ${width} ${height}`).attr('width', width).attr('height', height);

    // Defensive copies — d3-force mutates.
    const nodes: SimNode[] = coauthorGraph.nodes.map(n => ({ ...n }));
    const links = coauthorGraph.edges.map((e: CoauthorEdge) => ({ ...e }));

    // If reduced motion: skip simulation, use precomputed radial layout.
    if (reduced) {
      const layout = coauthorGraph.layout;
      nodes.forEach(n => {
        const p = layout[n.id];
        n.x = (p?.x ?? 0) + cx;
        n.y = (p?.y ?? 0) + cy;
      });
      drawStatic(svg, nodes, links, width, height, setHovered);
      return;
    }

    // Center Tim
    const center = nodes.find(n => n.isCenter);
    if (center) { center.fx = cx; center.fy = cy; }

    const sizeFor = (n: CoauthorNode) =>
      n.isCenter ? 14 : Math.max(3, Math.min(10, 3 + Math.sqrt(n.weight) * 2.2));

    const sim = d3.forceSimulation<SimNode>(nodes)
      .force('charge', d3.forceManyBody().strength(d => (d as SimNode).isCenter ? -160 : -80))
      .force('link', d3.forceLink(links).id((d: any) => d.id).distance(60).strength(0.4))
      .force('center', d3.forceCenter(cx, cy).strength(0.05))
      .force('collide', d3.forceCollide<SimNode>().radius(d => sizeFor(d) + 4))
      .alpha(0.9)
      .alphaDecay(0.04);

    const linkSel = svg.append('g')
      .attr('class', 'links')
      .attr('stroke-opacity', 0.45)
      .selectAll('line')
      .data(links)
      .join('line')
      .attr('stroke', (d: any) => TOPIC_COLOR[d.topic] || 'var(--muted)')
      .attr('stroke-width', (d: any) => Math.min(2.2, 0.6 + d.weight * 0.4));

    const nodeSel = svg.append('g')
      .attr('class', 'nodes')
      .selectAll('g')
      .data(nodes)
      .join('g')
      .style('cursor', 'pointer')
      .on('mouseenter', (_e, d) => setHovered(d.id))
      .on('mouseleave', () => setHovered(null));

    nodeSel.append('circle')
      .attr('r', d => sizeFor(d))
      .attr('fill', d => d.isCenter ? 'var(--ink)' : TOPIC_COLOR[d.topic] || 'var(--muted)')
      .attr('stroke', d => d.isCenter ? 'var(--accent)' : 'var(--bg)')
      .attr('stroke-width', d => d.isCenter ? 3 : 1.5);

    // Label only Tim + top-weight nodes
    const labelThreshold = d3.quantile(nodes.map(n => n.weight).sort((a, b) => b - a), 0.18) || 3;
    nodeSel.filter(d => d.isCenter || d.weight >= labelThreshold)
      .append('text')
      .text(d => d.isCenter ? 'Timothy Fraser' : d.label)
      .attr('x', d => sizeFor(d) + 4)
      .attr('y', 4)
      .attr('font-family', 'var(--font-body)')
      .attr('font-size', d => d.isCenter ? 12 : 10)
      .attr('font-weight', d => d.isCenter ? 700 : 500)
      .attr('fill', 'var(--ink)')
      .attr('pointer-events', 'none')
      .attr('paint-order', 'stroke')
      .attr('stroke', 'var(--bg)')
      .attr('stroke-width', 3)
      .attr('stroke-linejoin', 'round');

    sim.on('tick', () => {
      linkSel
        .attr('x1', (d: any) => d.source.x)
        .attr('y1', (d: any) => d.source.y)
        .attr('x2', (d: any) => d.target.x)
        .attr('y2', (d: any) => d.target.y);
      nodeSel.attr('transform', d => `translate(${d.x ?? 0},${d.y ?? 0})`);
    });

    // Stop after a bounded time — d3-force can run forever otherwise.
    const stopAt = Date.now() + 5000;
    const stopper = setInterval(() => {
      if (Date.now() > stopAt) { sim.stop(); clearInterval(stopper); }
    }, 500);

    return () => { sim.stop(); clearInterval(stopper); };
  }, [reduced, height]);

  // Top coauthor table (always present; serves as accessible fallback)
  const topCoauthors = [...coauthorGraph.nodes]
    .filter(n => !n.isCenter)
    .sort((a, b) => b.weight - a.weight)
    .slice(0, 12);

  const hoveredNode = hovered ? coauthorGraph.nodes.find(n => n.id === hovered) : null;

  return (
    <div ref={wrapRef} className="coauthor-viz">
      <svg ref={ref} role="img" aria-label="Coauthorship network of Timothy Fraser. Center node is Timothy Fraser; surrounding nodes are collaborators sized by number of joint publications and colored by dominant research topic." />
      {hoveredNode && (
        <div className="viz-tooltip" aria-live="polite">
          <strong>{hoveredNode.label}</strong>
          <span> · {hoveredNode.weight} paper{hoveredNode.weight === 1 ? '' : 's'} together</span>
          <span> · {hoveredNode.topic.replace(/_/g, ' ')}</span>
        </div>
      )}
      {showLegend && (
        <div className="viz-legend">
          {(['environment','transportation','disaster','resilience','energy','health','polarization','networks'] as const).map(t => (
            <span key={t}>
              <span className="dot" style={{ background: TOPIC_COLOR[t] }} /> {t.replace(/_/g, ' ')}
            </span>
          ))}
        </div>
      )}
      {showTable && (
        <details className="viz-fallback">
          <summary>Top collaborators (text)</summary>
          <ol>
            {topCoauthors.map(n => (
              <li key={n.id}>{n.label} — {n.weight} paper{n.weight === 1 ? '' : 's'}</li>
            ))}
          </ol>
        </details>
      )}
    </div>
  );
}

function drawStatic(
  svg: d3.Selection<SVGSVGElement, unknown, null, undefined>,
  nodes: SimNode[],
  links: any[],
  width: number,
  height: number,
  setHovered: (id: string | null) => void
) {
  const nodeById = new Map(nodes.map(n => [n.id, n]));
  svg.attr('viewBox', `0 0 ${width} ${height}`);

  svg.append('g')
    .selectAll('line')
    .data(links)
    .join('line')
    .attr('stroke', (d: any) => TOPIC_COLOR[d.topic] || 'var(--muted)')
    .attr('stroke-opacity', 0.35)
    .attr('stroke-width', 0.8)
    .attr('x1', (d: any) => nodeById.get(d.source)?.x ?? 0)
    .attr('y1', (d: any) => nodeById.get(d.source)?.y ?? 0)
    .attr('x2', (d: any) => nodeById.get(d.target)?.x ?? 0)
    .attr('y2', (d: any) => nodeById.get(d.target)?.y ?? 0);

  const g = svg.append('g').selectAll('g').data(nodes).join('g')
    .attr('transform', d => `translate(${d.x},${d.y})`)
    .on('mouseenter', (_e, d) => setHovered(d.id))
    .on('mouseleave', () => setHovered(null));

  g.append('circle')
    .attr('r', d => d.isCenter ? 14 : Math.max(3, Math.min(10, 3 + Math.sqrt(d.weight) * 2.2)))
    .attr('fill', d => d.isCenter ? 'var(--ink)' : TOPIC_COLOR[d.topic] || 'var(--muted)')
    .attr('stroke', d => d.isCenter ? 'var(--accent)' : 'var(--bg)')
    .attr('stroke-width', d => d.isCenter ? 3 : 1.5);
}
