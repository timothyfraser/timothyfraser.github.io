import { Link } from 'react-router-dom';
import SectionMast from '../components/SectionMast';

export default function NotFound() {
  return (
    <div className="wrap">
      <SectionMast
        eyebrow="404"
        title="Not the page you were looking for"
        subhead="If you were trying to reach the CV, the Sigma textbook, or another sub-site, those live at their own URLs."
      />
      <p>
        <Link to="/" className="btn">Back to home →</Link>
      </p>
    </div>
  );
}
