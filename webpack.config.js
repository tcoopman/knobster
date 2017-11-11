var webpack = require("webpack");
var path = require("path");

module.exports = {
    entry: {
        main: [
            './src/app.js' // entry point for your application code
        ],
    },
    devtool: "source-map",
    output: {
        path: path.join(__dirname, './release/'),
        filename: 'app.js',
    },
    resolve: {
        extensions: ['.js', '.ml', '.re'],
    },
    module: {
        rules: [{
                test: /\.(jpe?g|png|gif|svg|woff2|woff|eot|ttf)$/i,
                use: [{
                    loader: "file-loader",
                    options: {
                        hash: "sha512",
                        digest: "hex",
                        name: "[hash].[ext]"
                    }
                }]
            },
            {
                test: /\.(js)$/,
                use: [{loader: 'babel-loader'}]
            },
            {
                test: /\.css$/,
                use: ["style-loader", "css-loader"]
            }
        ]
    },
};