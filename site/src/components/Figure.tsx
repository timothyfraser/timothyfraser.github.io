interface Props {
  src: string;
  alt: string;
  caption?: string;
  align?: 'left' | 'right' | 'full';
}
export default function Figure({ src, alt, caption, align = 'full' }: Props) {
  return (
    <figure className={`figure${align === 'right' ? ' right' : align === 'left' ? ' left' : ''}`}>
      <img src={src} alt={alt} loading="lazy" />
      {caption && <figcaption>{caption}</figcaption>}
    </figure>
  );
}
