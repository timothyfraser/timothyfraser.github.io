/// <reference types="vite/client" />

declare module '*.md?raw' {
  const src: string;
  export default src;
}
declare module '*.csv?raw' {
  const src: string;
  export default src;
}
