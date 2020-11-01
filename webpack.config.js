const path = require('path')
const webpack = require('webpack')
const merge = require('webpack-merge')
const elmMinify = require('elm-minify')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const HTMLWebpackPlugin = require('html-webpack-plugin')
const {CleanWebpackPlugin} = require('clean-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const WorkboxPlugin = require('workbox-webpack-plugin')

const dev = 'development'
const prod = 'production'
const MODE = process.env.npm_lifecycle_event === 'build' ? prod : dev
const filename = MODE === prod ? '[name]-[hash].js' : 'index.js'
const assetsDir = 'assets'

const common = {
  mode: MODE,
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
        exclude: [/elm-stuff/, /node_modules/],
        loaders: ['style-loader', 'css-loader?url=false', 'sass-loader']
      }
    ]
  }
}

if (MODE === dev) {
  console.log('Building for dev...')
  module.exports = merge(common, {
    plugins: [
      new webpack.NamedModulesPlugin(),
      new webpack.NoEmitOnErrorsPlugin()
    ],
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {loader: 'elm-hot-webpack-loader'},
            {
              loader: 'elm-webpack-loader',
              options: {
                debug: false,
                forceWatch: true
              }
            }
          ]
        }
      ]
    },
    devServer: {
      inline: true,
      stats: 'errors-only',
      contentBase: path.join(__dirname, assetsDir),
      historyApiFallback: true
    }
  })
}

if (MODE === prod) {
  console.log('Building for production...')
  module.exports = merge(common, {
    output: {
      ...common.output,
      publicPath: './'
    },
    plugins: [
      new elmMinify.WebpackPlugin(),
      new CleanWebpackPlugin({
        root: __dirname,
        exclude: [],
        verbose: true,
        dry: false
      }),
      new CopyWebpackPlugin([{from: assetsDir, ignore: ['.DS_Store']}]),
      new MiniCssExtractPlugin({filename: '[name]-[hash].css'}),
      new WorkboxPlugin.GenerateSW({
        swDest: 'sw.js',
        clientsClaim: true,
        skipWaiting: true,
        globDirectory: assetsDir,
        globPatterns: ['**/*.{js,css,html,svg,png}'],
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
        },
        {
          test: /\.css$/,
          exclude: [/elm-stuff/, /node_modules/],
          loaders: [MiniCssExtractPlugin.loader, 'css-loader?url=false']
        },
        {
          test: /\.sass$/,
          exclude: [/elm-stuff/, /node_modules/],
          loaders: [
            MiniCssExtractPlugin.loader,
            'css-loader?url=false',
            'sass-loader'
          ]
        }
      ]
    }
  })
}
