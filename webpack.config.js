const path = require('path')
const webpack = require('webpack')
const {merge} = require('webpack-merge')
const elmMinify = require('elm-minify')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const HTMLWebpackPlugin = require('html-webpack-plugin')
const {CleanWebpackPlugin} = require('clean-webpack-plugin')
const {GenerateSW} = require('workbox-webpack-plugin')

const dev = 'development'
const prod = 'production'
const mode = process.env.npm_lifecycle_event === 'build' ? prod : dev
const filename = mode === prod ? '[name]-[contenthash].js' : 'index.js'
const assetsDir = 'assets'

const common = {
  mode,
  entry: './src/index.js',
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/',
    filename
  },
  plugins: [
    new HTMLWebpackPlugin({
      template: 'src/index.html',
      inject: 'body'
    })
  ],
  resolve: {
    modules: [path.join(__dirname, 'src'), 'node_modules'],
    extensions: ['.js', '.elm', '.sass', '.png']
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [['@babel/env', {useBuiltIns: 'usage', corejs: 3}]]
          }
        }
      },
      {
        test: /\.sass$/,
        use: ['style-loader', 'css-loader', 'sass-loader']
      }
    ]
  }
}

if (mode === dev) {
  module.exports = merge(common, {
    optimization: {
      moduleIds: 'named'
    },
    plugins: [new webpack.NoEmitOnErrorsPlugin()],
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {loader: 'elm-hot-webpack-loader'},
            {loader: 'elm-webpack-loader', options: {debug: false}}
          ]
        },
        {
          test: /\.js$/,
          enforce: 'pre',
          use: ['source-map-loader']
        }
      ]
    },
    devServer: {
      inline: true,
      stats: {
        children: true
      },
      contentBase: path.join(__dirname, assetsDir),
      historyApiFallback: true
    }
  })
}

if (mode === prod) {
  module.exports = merge(common, {
    output: {
      ...common.output,
      publicPath: './'
    },
    stats: {
      children: true
    },
    plugins: [
      new elmMinify.WebpackPlugin(),
      new CleanWebpackPlugin({
        root: __dirname,
        exclude: [],
        verbose: true,
        dry: false
      }),
      new CopyWebpackPlugin({patterns: [{from: assetsDir}]}),
      new GenerateSW({
        swDest: 'sw.js',
        clientsClaim: true,
        skipWaiting: true,
        runtimeCaching: [
          {
            urlPattern: /^https:\/\/fonts\.googleapis\.com/,
            handler: 'StaleWhileRevalidate'
          },
          {
            urlPattern: /^https:\/\/fonts\.gstatic\.com/,
            handler: 'CacheFirst'
          }
        ]
      })
    ],
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: 'elm-webpack-loader',
            options: {
              optimize: true
            }
          }
        }
      ]
    }
  })
}
