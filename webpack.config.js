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
		poll: 500,
		ignored: /node_modules/
	},
	entry: {
    	app:   '/src/main/purescript/_app_main.js',
    	pg:    '/src/main/purescript/_pg_main.js',
		share: '/src/main/purescript/_share_main.js',
	},
	output: {
		filename: '[name]-bundle.[fullhash].js',
		path: path.resolve(__dirname, 'target', "output.webpack"),
		clean: true,
	},
	optimization: {
		minimize: true,
	},
	plugins: [
		new webpack.ProvidePlugin({
			process: 'process/browser',
		}),
		// app html package configuration
		new HtmlWebpackPlugin({
			template: '/src/main/html/index.html',
			filename: 'index.html',
			scriptLoading: 'module',
			chunks: ["app"],
			minify: true,
		}),
		new HtmlInlineScriptPlugin({
			htmlMatchPattern: [/index.html$/],
			scriptMatchPattern: [/app-bundle.[a-zA-Z0-9]+.js/],
		}),
		// password generator html package configuration
		new HtmlWebpackPlugin({
			template: '/src/main/html/pg_index.html',
			filename: 'pg_index.html',
			scriptLoading: 'module',
			chunks: ["pg"],
			minify: true,
		}),
		new HtmlInlineScriptPlugin({
			htmlMatchPattern: [/pg_index.html$/],
			scriptMatchPattern: [/pg-bundle.[a-zA-Z0-9]+.js/],
		}),
		// share html package configuration
		new HtmlWebpackPlugin({
			template: '/src/main/html/share_index.html',
			filename: 'share_index.html',
			scriptLoading: 'module',
			chunks: ["share"],
			minify: true,
		}),
		new HtmlInlineScriptPlugin({
			htmlMatchPattern: [/share_index.html$/],
			scriptMatchPattern: [/share-bundle.[a-zA-Z0-9]+.js/],
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
	}
 };