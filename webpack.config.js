var path = require('path');
var webpack = require('webpack');

// https://www.npmjs.com/package/webpack

// TODO - Maybe this instead of webpack dev server?
//https://babeljs.io/docs/setup/#webstorm

var isDevelopment = (process.env.NODE_ENV !== 'production');
//console.log("IS DEVELOPMENT? " + isDevelopment);
//var entry = isDevelopment ? [
//    'webpack-dev-server/client?http://localhost:8080/',
//    'webpack/hot/dev-server', // Which do we need?
//    'webpack/hot/only-dev-server', // or this?
//    './src/entry'
//] : ['./src/entry'];

//var GSC_ITEMS_SERVICE_HOST = isDevelopment ?  'http://catalogs.gospotcheck.com' :
//    'https://someprodurl'; // will need review and such

//var ITEMS_SERVICE_URL = isDevelopment? 'http://items.gospotcheck.com:4000' :
//    process.env.ITEMS_SERVICE_BASE_URL;

var pluggins = isDevelopment ? [
    new webpack.HotModuleReplacementPlugin()
    //new webpack.NoErrorsPlugin()
    // new webpack.DefinePlugin({
    //     ITEMS_SERVICE_URL: JSON.stringify('http://items.gospotcheck.com:4000')
    // })
] : [
    new webpack.DefinePlugin({
        ITEMS_SERVICE_URL: JSON.stringify(process.env.ITEMS_SERVICE_BASE_URL)
    })
];

var publicPath = isDevelopment ? 'http://localhost:8080/' : "/";

//console.log("********** CONFIGURE IN webpack.config.js *********");
//console.log("publicPath = " + publicPath);
// https://github.com/gaearon/react-hot-loader/issues/92
//watch: true
//module.exports = {
//    entry: {
//        index: ['./src/js/index.jsx']
//    },
//    output: {
//        path: path.resolve(__dirname, 'build/js'),
//        filename: '[name].js',
//        publicPath: 'test/for/'
//    },

module.exports = {
    // CLI only :(
    //devServer: {
    //    hot: true, inline: true
    //},
    //devtool: 'eval',
    entry: {
        src: ['./static/src/entry.js']
    },
    output: {
        path: path.resolve(__dirname, "static"),
        publicPath: publicPath,
        filename: 'bundle.js'
    },
    plugins: pluggins,
    resolve: {
        root: path.resolve('./static/src'),
        extensions: ['', '.js', '.jsx', '.js.jsx']
    },
    module: {
        loaders: [
            {
                test: /\.jsx?$/, // /\.jsx?/,
                include: [
                    path.join(__dirname, 'static/src'),
                    path.join(__dirname, 'static/src/components')
                ],
                loader: 'babel-loader',
                // loader: 'react-hot',
                // loaders: ['react-hot', 'babel'],
                query: {
                    presets: ['react', 'es2015']
                }
            },
            {
                test: /\.css$/,
                loader: 'style!css-loader?modules&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]'
            },
            {
                test: /\.scss$/,
                loader: "style!css-loader?modules&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]!sass"
            }
            //{ test: /\.scss?$/,
            //    loader: 'style!css!sass',
            //    include: path.join(__dirname, 'css') },
            //{ test: /\.css$/,
            //    loader: 'style!css' }
        ]
    }
};
