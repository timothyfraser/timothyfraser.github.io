import { Link } from 'react-router-dom';
import SectionMast from '../components/SectionMast';

export default function NotFound() {
  return (
    <div className="wrap">
      <SectionMast
        kicker="404 · page not found"
        title={<><em>Not</em> the page you were looking for.</>}
        lede="If you were trying to reach the CV, the Sigma textbook, or another sub-site, those live at their own URLs."
      />
      <p>
        <Link to="/" className="btn">Back to home →</Link>
      </p>
    </div>
  );
}
