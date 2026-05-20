import { Routes, Route, NavLink } from 'react-router-dom';
import Home from './pages/Home';
import About from './pages/About';
import Research from './pages/Research';
import Teaching from './pages/Teaching';
import Projects from './pages/Projects';
import Software from './pages/Software';
import Students from './pages/Students';
import Press from './pages/Press';
import Publications from './pages/Publications';
import NotFound from './pages/NotFound';
import Footer from './components/Footer';
import { site } from './data/loaders';

export default function App() {
  return (
    <>
      <a className="skip-link" href="#main">Skip to content</a>
      <nav className="site-nav" aria-label="Primary">
        <div className="site-nav-inner">
          <NavLink to="/" className="site-nav-brand">
            Timothy Fraser
          </NavLink>
          <ul>
            {site.nav.map(item => (
              <li key={item.path}>
                <NavLink
                  to={item.path}
                  end={item.path === '/'}
                  className={({ isActive }) => `nav-link${isActive ? ' active' : ''}`}
                >
                  {item.label}
                </NavLink>
              </li>
            ))}
            <li>
              <a className="nav-link cv" href={site.links.cv}>CV</a>
            </li>
          </ul>
        </div>
      </nav>
      <main id="main" className="page">
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/about" element={<About />} />
          <Route path="/research" element={<Research />} />
          <Route path="/teaching" element={<Teaching />} />
          <Route path="/projects" element={<Projects />} />
          <Route path="/software" element={<Software />} />
          <Route path="/students" element={<Students />} />
          <Route path="/press" element={<Press />} />
          <Route path="/publications" element={<Publications />} />
          <Route path="*" element={<NotFound />} />
        </Routes>
      </main>
      <Footer />
    </>
  );
}
