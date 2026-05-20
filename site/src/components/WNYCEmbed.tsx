/**
 * WNYC featured-interview card. Graceful link out to the story page.
 * To embed the in-page audio widget, swap the <a> for an <iframe> using
 * WNYC's `widgets/ondemand_player/` URL once Tim provides the exact one.
 */
interface Props {
  storyUrl?: string;
  title?: string;
  outlet?: string;
  date?: string;
}

export default function WNYCEmbed({
  storyUrl = 'https://www.wnyc.org/story/congestion-pricing-has-nyc-breathing-easier/',
  title = 'Congestion pricing has NYC breathing easier',
  outlet = 'WNYC Public Radio',
  date = 'December 2025',
}: Props) {
  return (
    <a className="wnyc-card" href={storyUrl} target="_blank" rel="noopener noreferrer">
      <div className="wnyc-icon" aria-hidden="true">
        {/* Simple play-triangle */}
        <svg width="22" height="22" viewBox="0 0 22 22" fill="currentColor"><path d="M6 4 L18 11 L6 18 Z" /></svg>
      </div>
      <div>
        <div className="wnyc-meta">Featured interview · {outlet}</div>
        <div className="wnyc-title">{title}</div>
        <div className="wnyc-sub">{date} · Tim discusses the npj Clean Air congestion-pricing study.</div>
      </div>
      <div className="wnyc-cta">Listen on WNYC →</div>
    </a>
  );
}
