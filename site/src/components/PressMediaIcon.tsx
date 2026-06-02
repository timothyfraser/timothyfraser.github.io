type PressIconKey =
  | 'newspaper_broadsheet'
  | 'newspaper_tabloid'
  | 'online_globe'
  | 'online_rss'
  | 'journal_academic'
  | 'government_capitol'
  | 'government_seal'
  | 'radio_mic'
  | 'tv_broadcast'
  | 'magazine'
  | 'university'
  | 'default_info';

const ICON_LABELS: Record<PressIconKey, string> = {
  newspaper_broadsheet: 'Broadsheet',
  newspaper_tabloid: 'Tabloid / local',
  online_globe: 'Online / web',
  online_rss: 'Digital news',
  journal_academic: 'Journal / peer review',
  government_capitol: 'Government / capitol',
  government_seal: 'Official release',
  radio_mic: 'Radio / podcast',
  tv_broadcast: 'TV broadcast',
  magazine: 'Magazine / trade',
  university: 'University press',
  default_info: 'Generic press',
};

const MEDIA_MAP: Record<string, PressIconKey> = {
  newspaper: 'newspaper_broadsheet',
  online_news: 'online_globe',
  journal: 'journal_academic',
  government: 'government_capitol',
  radio: 'radio_mic',
  tv: 'tv_broadcast',
};

const OUTLET_MAP: Record<string, PressIconKey> = {
  'The New York Times': 'newspaper_broadsheet',
  Newsday: 'newspaper_tabloid',
  'Hell Gate': 'newspaper_tabloid',
  'Highlands Current': 'newspaper_tabloid',
  "Crain's New York": 'newspaper_broadsheet',
  'El Mercurio': 'newspaper_broadsheet',
  'The Academic Times': 'online_rss',
  SupplyChain247: 'magazine',
  'CiTTi Magazine': 'magazine',
  'Cornell Chronicle': 'university',
  "New York Governor's Office": 'government_seal',
  Bloomberg: 'online_rss',
  Vox: 'online_globe',
  'WNYC (NYC Public Radio)': 'radio_mic',
  'News12 Long Island': 'tv_broadcast',
};

export function resolvePressIcon(
  outlet: string | null | undefined,
  mediaType: string | null | undefined,
): PressIconKey {
  const outletKey = (outlet || '').trim();
  if (outletKey && OUTLET_MAP[outletKey]) return OUTLET_MAP[outletKey];
  const media = (mediaType || '').toLowerCase().replace(/\s+/g, '_');
  if (media && MEDIA_MAP[media]) return MEDIA_MAP[media];
  return 'default_info';
}

function IconSvg({ name }: { name: PressIconKey }) {
  const props = { viewBox: '0 0 48 48', width: 36, height: 36, 'aria-hidden': true as const };

  switch (name) {
    case 'newspaper_broadsheet':
      return (
        <svg {...props}>
          <rect x="10" y="8" width="28" height="32" rx="1" fill="none" stroke="currentColor" strokeWidth="2" />
          <line x1="14" y1="16" x2="34" y2="16" stroke="currentColor" strokeWidth="2.5" />
          <line x1="14" y1="22" x2="34" y2="22" stroke="currentColor" strokeWidth="1.5" />
          <line x1="14" y1="28" x2="26" y2="28" stroke="currentColor" strokeWidth="1.5" />
          <line x1="14" y1="34" x2="30" y2="34" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'newspaper_tabloid':
      return (
        <svg {...props}>
          <rect x="12" y="10" width="24" height="28" rx="2" fill="none" stroke="currentColor" strokeWidth="2" />
          <rect x="15" y="14" width="18" height="10" fill="currentColor" opacity="0.25" />
          <line x1="15" y1="28" x2="33" y2="28" stroke="currentColor" strokeWidth="1.5" />
          <line x1="15" y1="32" x2="28" y2="32" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'online_globe':
      return (
        <svg {...props}>
          <circle cx="24" cy="24" r="14" fill="none" stroke="currentColor" strokeWidth="2" />
          <ellipse cx="24" cy="24" rx="6" ry="14" fill="none" stroke="currentColor" strokeWidth="1.5" />
          <line x1="10" y1="24" x2="38" y2="24" stroke="currentColor" strokeWidth="1.5" />
          <path d="M24 10c4 4 6 9 6 14s-2 10-6 14" fill="none" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'online_rss':
      return (
        <svg {...props}>
          <path d="M10 32a14 14 0 0 1 14-14" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" />
          <path d="M10 24a22 22 0 0 1 22-22" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" />
          <circle cx="12" cy="36" r="3" fill="currentColor" />
        </svg>
      );
    case 'journal_academic':
      return (
        <svg {...props}>
          <path d="M14 8h18l8 8v26a2 2 0 0 1-2 2H14a2 2 0 0 1-2-2V10a2 2 0 0 1 2-2z" fill="none" stroke="currentColor" strokeWidth="2" />
          <path d="M26 8v10h10" fill="none" stroke="currentColor" strokeWidth="2" />
          <line x1="18" y1="24" x2="32" y2="24" stroke="currentColor" strokeWidth="1.5" />
          <line x1="18" y1="30" x2="28" y2="30" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'government_capitol':
      return (
        <svg {...props}>
          <path d="M8 22h32v16H8z" fill="none" stroke="currentColor" strokeWidth="2" />
          <path d="M14 22V14l10-6 10 6v8" fill="none" stroke="currentColor" strokeWidth="2" strokeLinejoin="round" />
          <line x1="24" y1="8" x2="24" y2="12" stroke="currentColor" strokeWidth="2" />
          <rect x="20" y="28" width="8" height="10" fill="none" stroke="currentColor" strokeWidth="2" />
        </svg>
      );
    case 'government_seal':
      return (
        <svg {...props}>
          <circle cx="24" cy="24" r="15" fill="none" stroke="currentColor" strokeWidth="2" />
          <circle cx="24" cy="24" r="9" fill="none" stroke="currentColor" strokeWidth="1.5" />
          <path d="M24 12v4M24 32v4M12 24h4M32 24h4" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'radio_mic':
      return (
        <svg {...props}>
          <rect x="18" y="8" width="12" height="20" rx="6" fill="none" stroke="currentColor" strokeWidth="2" />
          <path d="M14 20a10 10 0 0 0 20 0" fill="none" stroke="currentColor" strokeWidth="2" />
          <line x1="24" y1="30" x2="24" y2="38" stroke="currentColor" strokeWidth="2" />
          <line x1="16" y1="38" x2="32" y2="38" stroke="currentColor" strokeWidth="2" />
        </svg>
      );
    case 'tv_broadcast':
      return (
        <svg {...props}>
          <rect x="8" y="14" width="32" height="22" rx="3" fill="none" stroke="currentColor" strokeWidth="2" />
          <path d="M18 10l6-5 6 5" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" />
          <circle cx="24" cy="25" r="5" fill="none" stroke="currentColor" strokeWidth="1.5" />
          <path d="M16 40h16" stroke="currentColor" strokeWidth="2" strokeLinecap="round" />
        </svg>
      );
    case 'magazine':
      return (
        <svg {...props}>
          <rect x="12" y="6" width="24" height="36" rx="2" fill="none" stroke="currentColor" strokeWidth="2" />
          <rect x="16" y="10" width="16" height="12" fill="currentColor" opacity="0.2" />
          <line x1="16" y1="26" x2="32" y2="26" stroke="currentColor" strokeWidth="1.5" />
          <line x1="16" y1="32" x2="28" y2="32" stroke="currentColor" strokeWidth="1.5" />
        </svg>
      );
    case 'university':
      return (
        <svg {...props}>
          <path d="M24 8L6 18l18 10 18-10L24 8z" fill="none" stroke="currentColor" strokeWidth="2" strokeLinejoin="round" />
          <path d="M12 22v12l12 6 12-6V22" fill="none" stroke="currentColor" strokeWidth="2" strokeLinejoin="round" />
        </svg>
      );
    default:
      return (
        <svg {...props}>
          <circle cx="24" cy="24" r="14" fill="none" stroke="currentColor" strokeWidth="2" />
          <line x1="24" y1="18" x2="24" y2="26" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" />
          <circle cx="24" cy="31" r="1.8" fill="currentColor" />
        </svg>
      );
  }
}

export default function PressMediaIcon({
  outlet,
  mediaType,
}: {
  outlet: string | null | undefined;
  mediaType: string | null | undefined;
}) {
  const key = resolvePressIcon(outlet, mediaType);
  return (
    <span className="press-media-icon" title={ICON_LABELS[key]}>
      <IconSvg name={key} />
    </span>
  );
}
