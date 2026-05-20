export interface Publication {
  id: string;
  title: string;
  year: number | null;
  journal: string | null;
  authors: string[];
  doi: string | null;
  link: string | null;
  description: string | null;
  type: string;
  topics: string[];
  dominant: string;
  volume: string | null;
  issue: string | null;
}

export interface PressItem {
  type: string;
  press_author: string | null;
  title: string;
  outlet: string | null;
  media_type: string | null;
  link: string | null;
  date_raw: string | null;
  date: string | null;
  year: number | null;
}

export interface CoauthorNode {
  id: string;
  label: string;
  weight: number;
  topic: string;
  isCenter: boolean;
}
export interface CoauthorEdge {
  source: string;
  target: string;
  weight: number;
  topic: string;
}
export interface CoauthorGraph {
  nodes: CoauthorNode[];
  edges: CoauthorEdge[];
  layout: Record<string, { x: number; y: number }>;
}

export interface MetricsComputed {
  citations: number | null;
  h_index: number | null;
  i10_index: number | null;
  as_of: string | null;
  peer_reviewed: number;
  chapters: number;
  software: number;
  coauthors: number;
  press_total: number;
  press_last_12mo: number;
  press_by_year: Record<string, number>;
  press_by_type: Record<string, number>;
  by_type: Record<string, number>;
  topics: Record<string, number>;
}

export interface ProjectItem {
  id: string;
  n: string;
  kind: 'meng' | 'research';
  category: string;
  name: string;
  tagline: string;
  badge: { label: string; variant: string };
  accent: 'cool' | 'hot' | 'signal' | 'gold' | 'cornell';
  body: string;
  question: string | null;
  phase: { label: string; steps: string[]; now: string } | null;
  skills: string[];
  recruiting: boolean;
  links: { label: string; url: string }[];
}

export interface TeachingResource {
  id: string;
  name: string;
  kind: string;
  blurb: string;
  url: string;
  featured?: boolean;
  accent?: string;
  tags: string[];
}
export interface TeachingData {
  courses_taught: { code: string; title: string; level: string; years: number[]; inst: string }[];
  resources: TeachingResource[];
}

export interface SoftwareItem {
  id: string;
  name: string;
  blurb: string;
  repo_url: string | null;
  live_url: string | null;
  tags: string[];
  year: number | null;
  status: string;
  featured: boolean;
}

export interface Student {
  name: string;
  level: string;
  team: string;
  institution: string;
  outputs: string;
  current: boolean;
}

export interface SiteCfg {
  name: string;
  short_name: string;
  role: string;
  affiliation: string;
  institution: string;
  location: string;
  mission: string;
  email: string;
  nav: { label: string; path: string }[];
  links: Record<string, string>;
}

export interface ResearchSite {
  label: string;
  lat: number;
  lng: number;
  topic: string;
  blurb: string;
  year: number | null;
}
