import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import NetworkBackground from './components/NetworkBackground';
import './design/base.css';
import './design/components.css';
import './design/layout.css';

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <NetworkBackground />
    <div className="brand-stripe" aria-hidden="true" />
    <BrowserRouter>
      <App />
    </BrowserRouter>
  </StrictMode>
);
