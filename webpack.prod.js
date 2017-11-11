const webpack = require("webpack");
const webpackConfig = require("./webpack.config.js");
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

webpackConfig.plugins = [
    new webpack.DefinePlugin({
        'process.env': {
            'NODE_ENV': JSON.stringify('production')
        }
    }),
    new webpack.optimize.ModuleConcatenationPlugin()
]
webpackConfig.output.filename = 'app.[hash].js'

module.exports = webpackConfig;