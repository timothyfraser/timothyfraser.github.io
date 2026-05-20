import { useEffect, useRef } from 'react';

/**
 * Animated network background — a sparse force-directed graph rendered
 * to canvas behind all content. Cornell-red edges, ink nodes, both low
 * alpha. Respects prefers-reduced-motion (renders a single static snapshot).
 */
interface Node {
  x: number; y: number;
  vx: number; vy: number;
  r: number;
}

const NODE_COUNT = 42;
const EDGES_PER_NODE = 2;
const TARGET_FPS = 30;

function rand(min: number, max: number) { return min + Math.random() * (max - min); }

function readCssVar(name: string): string {
  if (typeof window === 'undefined') return '#10201c';
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim() || '#10201c';
}

export default function NetworkBackground() {
  const ref = useRef<HTMLCanvasElement | null>(null);
  const rafRef = useRef<number | null>(null);

  useEffect(() => {
    const canvas = ref.current;
    if (!canvas) return;
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const reduced = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    const offSwitch = document.documentElement.dataset.bg === 'off';
    if (offSwitch) return;

    const ctx = canvas.getContext('2d', { alpha: true });
    if (!ctx) return;

    let w = window.innerWidth;
    let h = window.innerHeight;
    const setSize = () => {
      w = window.innerWidth;
      h = window.innerHeight;
      canvas.width = w * dpr;
      canvas.height = h * dpr;
      canvas.style.width = w + 'px';
      canvas.style.height = h + 'px';
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    };
    setSize();

    // Resolve accent + ink at runtime — variables are oklch() so we read
    // computed style to get a renderable color.
    const accent = readCssVar('--accent') || '#B31B1B';
    const ink = readCssVar('--ink') || '#10201c';

    // Spawn nodes
    const nodes: Node[] = Array.from({ length: NODE_COUNT }, () => ({
      x: rand(0, w),
      y: rand(0, h),
      vx: rand(-0.15, 0.15),
      vy: rand(-0.12, 0.12),
      r: rand(1.5, 3.2),
    }));

    // Static edges: each node connects to its k nearest neighbors at spawn time.
    const edges: [number, number][] = [];
    {
      const distSq = (a: Node, b: Node) => {
        const dx = a.x - b.x, dy = a.y - b.y;
        return dx * dx + dy * dy;
      };
      for (let i = 0; i < nodes.length; i++) {
        const order = nodes
          .map((n, j) => ({ j, d: j === i ? Infinity : distSq(nodes[i], n) }))
          .sort((a, b) => a.d - b.d)
          .slice(0, EDGES_PER_NODE);
        for (const o of order) {
          // Dedupe: smaller-index-first
          const a = Math.min(i, o.j), b = Math.max(i, o.j);
          if (!edges.find(([x, y]) => x === a && y === b)) edges.push([a, b]);
        }
      }
    }

    const step = () => {
      ctx.clearRect(0, 0, w, h);

      // Move nodes (only if not reduced motion)
      if (!reduced) {
        for (const n of nodes) {
          n.x += n.vx;
          n.y += n.vy;
          // Soft bounce
          if (n.x < 0 || n.x > w) { n.vx *= -1; n.x = Math.max(0, Math.min(w, n.x)); }
          if (n.y < 0 || n.y > h) { n.vy *= -1; n.y = Math.max(0, Math.min(h, n.y)); }
          // Tiny drift jitter
          n.vx += rand(-0.003, 0.003);
          n.vy += rand(-0.003, 0.003);
          // Cap speed
          const sp = Math.hypot(n.vx, n.vy);
          if (sp > 0.4) { n.vx *= 0.4 / sp; n.vy *= 0.4 / sp; }
        }
      }

      // Draw edges
      ctx.strokeStyle = accent;
      ctx.lineWidth = 1;
      ctx.globalAlpha = 0.12;
      ctx.beginPath();
      for (const [a, b] of edges) {
        ctx.moveTo(nodes[a].x, nodes[a].y);
        ctx.lineTo(nodes[b].x, nodes[b].y);
      }
      ctx.stroke();

      // Draw nodes
      ctx.fillStyle = ink;
      ctx.globalAlpha = 0.20;
      for (const n of nodes) {
        ctx.beginPath();
        ctx.arc(n.x, n.y, n.r, 0, Math.PI * 2);
        ctx.fill();
      }
      ctx.globalAlpha = 1;
    };

    if (reduced) {
      step();
      return;
    }

    const interval = 1000 / TARGET_FPS;
    let last = performance.now();
    const loop = (now: number) => {
      if (document.hidden) {
        rafRef.current = requestAnimationFrame(loop);
        return;
      }
      const dt = now - last;
      if (dt >= interval) {
        last = now - (dt % interval);
        step();
      }
      rafRef.current = requestAnimationFrame(loop);
    };
    rafRef.current = requestAnimationFrame(loop);

    const onResize = () => setSize();
    window.addEventListener('resize', onResize);
    return () => {
      window.removeEventListener('resize', onResize);
      if (rafRef.current !== null) cancelAnimationFrame(rafRef.current);
    };
  }, []);

  return <canvas id="bg-network" ref={ref} aria-hidden="true" />;
}
