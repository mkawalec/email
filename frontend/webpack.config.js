const webpack = require('webpack');
const PurescriptWebpackPlugin = require('purescript-webpack-plugin');
const LiveReloadPlugin = require('webpack-livereload-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const purescriptPlugin = new PurescriptWebpackPlugin({
  src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
  ffi: ['bower_components/purescript-*/src/**/*.js'],
  bundle: false,
  psc: 'psa',
  pscArgs: {
    sourceMaps: true,
    noOpts: true
  }
});

const htmlPluginOpts = {
  title: 'My email client',
  hash: true,
}

module.exports = {
  entry:  __dirname + '/src/index.js',
  devtool: 'source-map',
  devServer: {
    contentBase: 'dist/',
    port: 4009,
    stats: 'errors-only'
  },
  module: {
    loaders: [
      { test: /\.purs$/, loader: 'purs-loader?output=output&src[]=bower_components/purescript-*/src/**/*.purs&src=[]=src/**/*.purs&ffi[]=bower_components/purescript-*/src/**/*.js'},
      { test: /\.js$/, loader: 'source-map-loader', exclude: /node_modules|bower_components/},
      { test: /\.png$/, loader: 'url-loader' }
    ]
  },
  plugins: [purescriptPlugin, new LiveReloadPlugin(), new HtmlWebpackPlugin(htmlPluginOpts)],
  output: {
    path: __dirname + '/dist',
    pathinfo: true,
    filename: 'bundle.js'
  },
  resolve: {
    extensions: ['', '.purs', '.js', '.css', '.styl'],
    modulesDirectories: ['bower_components', 'node_modules'],
    alias: {
    },
  },
};
