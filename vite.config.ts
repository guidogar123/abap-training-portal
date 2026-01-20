import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  base: './',
  optimizeDeps: {
    exclude: ['@abaplint/transpiler', '@abaplint/runtime']
  },
  build: {
    commonjsOptions: {
      transformMixedEsModules: true,
    },
    rollupOptions: {
      output: {
        manualChunks: {
          abaplint: ['@abaplint/transpiler', '@abaplint/runtime']
        }
      }
    }
  }
})
