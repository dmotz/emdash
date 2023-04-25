import {defineConfig} from 'vite'
import elmPlugin from 'vite-plugin-elm'
import {ViteMinifyPlugin} from 'vite-plugin-minify'

export default defineConfig({
  publicDir: 'assets',
  server: {port: 1999},
  plugins: [elmPlugin({debug: false}), ViteMinifyPlugin({})],
  esbuild: {legalComments: 'none'}
})
