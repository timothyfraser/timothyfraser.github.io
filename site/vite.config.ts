import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'node:path'
import fs from 'node:fs'

// Copy index.html → 404.html for GitHub Pages SPA deep-link support.
function spa404() {
  return {
    name: 'spa-404',
    closeBundle() {
      const out = path.resolve('dist')
      const src = path.join(out, 'index.html')
      const dst = path.join(out, '404.html')
      if (fs.existsSync(src)) fs.copyFileSync(src, dst)
    },
  }
}

export default defineConfig({
  base: '/',
  plugins: [react(), spa404()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, 'src'),
      '@content': path.resolve(__dirname, 'content'),
    },
  },
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    target: 'es2020',
    rollupOptions: {
      output: {
        manualChunks: {
          d3: ['d3'],
          react: ['react', 'react-dom', 'react-router-dom'],
        },
      },
    },
  },
})
