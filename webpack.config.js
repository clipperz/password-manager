'use strict'

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const HtmlInlineScriptPlugin = require('html-inline-script-webpack-plugin');
const webpack = require('webpack');

 module.exports = {
	mode: 'development',
	// mode: 'production',
	watchOptions: {
		aggregateTimeout: 200,
		poll: 1000,
		ignored: /node_modules/
	},
	entry: {
    	index: '/src/main/purescript/_main.js',
	},
	plugins: [
		new webpack.ProvidePlugin({
			process: 'process/browser',
		}),
		new HtmlWebpackPlugin({
			template: '/src/main/html/index.html',
			scriptLoading: 'module',
		}),
		new HtmlInlineScriptPlugin({
			htmlMatchPattern: [/index.html$/],
			scriptMatchPattern: [/bundle.[a-zA-Z0-9]+.js/],
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
	optimization: {
		minimize: true,
	},
	output: {
		filename: 'bundle.[fullhash].js',
		path: path.resolve(__dirname, 'target', "output.webpack"),
		clean: true,
	},
 };