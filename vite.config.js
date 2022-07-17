import {defineConfig} from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  publicDir: 'assets',
  server: {port: 1999},
  plugins: [elmPlugin({debug: false})]
})
