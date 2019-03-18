const path = require('path');
const glob = require('glob');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

var config = {
  optimization: {
    minimizer: [
      new UglifyJsPlugin({ cache: true, parallel: true, sourceMap: false }),
      new OptimizeCSSAssetsPlugin({})
    ]
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      },
      {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader']
      }
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({ filename: '../css/app.css' }),
    new CopyWebpackPlugin([{ from: 'static/', to: '../' }])
  ]
};

var navConfig = Object.assign({}, config, {
  entry: ['./js/nav.js'],
  output: {
    filename: 'nav.js',
    path: path.resolve(__dirname, '../priv/static/js')
  }
})

var signupConfig = Object.assign({}, config, {
  entry: ['./js/signup.js'],
  output: {
    filename: 'signup.js',
    path: path.resolve(__dirname, '../priv/static/js')
  }
})

var adminConfig = Object.assign({}, config, {
  entry: ['./js/admin/admin.js'],
  output: {
    filename: 'admin.js',
    path: path.resolve(__dirname, '../priv/static/js')
  }
})

var gameConfig = Object.assign({}, config, {
  entry: ['./js/game.js', './js/map.js'],
  output: {
    filename: 'game.js',
    path: path.resolve(__dirname, '../priv/static/js')
  }
})
module.exports = [navConfig, gameConfig, signupConfig, adminConfig]

