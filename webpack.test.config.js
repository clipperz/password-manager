'use strict'

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

module.exports = {
	mode: "development",
	watchOptions: {
		aggregateTimeout: 200,
		poll: 500,
		ignored: /node_modules/
	},
	entry: {
    	test:   '/src/test/purescript/_testMain.js',
		debug:  '/src/test/purescript/_debugApp_main.js',
	},
	output: {
		filename: '[name]-bundle.[fullhash].js',
		path: path.resolve(__dirname, 'target', "output.webpack.test"),
		clean: true,
	},
	plugins: [
		new webpack.ProvidePlugin({
			process: 'process/browser',
		}),
		// test html package configuration
		new HtmlWebpackPlugin({
			template: '/src/test/html/test_index.html',
			filename: 'test_index.html',
			scriptLoading: 'module',
			chunks: ["test"],
			minify: true,
		}),
		// debug app package configuration
		new HtmlWebpackPlugin({
			template: '/src/test/html/debug_index.html',
			filename: 'debug_index.html',
			scriptLoading: 'module',
			chunks: ["debug"],
			minify: true,
		}),
	],
	module: {
		rules: [{
			test: /\.m?js/,
			resolve: {
				fullySpecified: false,
			},
		},
		{
			test: /\.s[ac]ss$/i,
			use: [
			  // Creates `style` nodes from JS strings
			  "style-loader",
			  // Translates CSS into CommonJS
			  "css-loader",
			  // Compiles Sass to CSS
			  "sass-loader",
			],
		},
		]
	},
	devServer: {
		https: true,
		static: [
			{
				directory: path.join(__dirname, 'target', 'output.webpack.test'),
				publicPath: "/static"
			},
		],
		allowedHosts: 'all',
		compress: true,
		port: 9000,
	},
};