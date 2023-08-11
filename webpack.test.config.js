'use strict'

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');

module.exports = {
	mode: "development",
	entry: {
    	test:   '/src/test/purescript/_testMain.js',
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
			template: '/src/test/html/index.html',
			filename: 'index.html',
			scriptLoading: 'module',
			chunks: ["test"],
			minify: true,
		})
	],
	module: {
		rules: [{
			test: /\.m?js/,
			resolve: {
				fullySpecified: false,
			},
		}
		]
	},
	devServer: {
		https: true,
		static: {
			directory: path.join(__dirname, 'src', 'test', 'html'),
		},
		allowedHosts: 'all',
		compress: true,
		port: 9000,
	},
};