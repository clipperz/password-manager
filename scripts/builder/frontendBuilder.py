#!/usr/bin/python
# -*- coding: UTF-8 -*-

import sys, os, re
import cssmin
import jsmin
import codecs
import shutil
import StringIO
import urllib

#from mercurial import ui, hg
#from mercurial.node import hex
from dulwich.repo import Repo

import main



class FrontendBuilder:

	def __init__ (self, frontend, settings):
		if '.' in frontend:
			moduleComponents = frontend.split('.')
			self.module = moduleComponents[0]
			self.submodule = moduleComponents[1]
		else:
			self.module = frontend
			self.submodule = frontend

		self.settings = settings
		self.projectDir = main.projectBaseDir()
		self.processedFiles = {}
		

	def mercurialRepositoryVersion (self):
		repo = hg.repository(ui.ui(), self.projectDir)
		context = repo['tip']
		result = str(context)
		
		return result
	

	def gitRepositoryVersion (self):
		repo = Repo(self.projectDir)
		#if repo.is_dirty():
		#	print "WARNING: build run with dirty repository"
		result = repo.refs['HEAD']
		
		return result
	

		
	def repositoryVersion (self): 
		cacheKey = 'repositoryVersion'
		if not self.processedFiles.has_key(cacheKey):
			#result = self.mercurialRepositoryVersion()
			result = self.gitRepositoryVersion()
			self.processedFiles[cacheKey] = result
		else:
			result = self.processedFiles[cacheKey]
		
		return result
	

	#def relativePath (self):
	#	return self.module
	#

	def log (self, message):
		print "frontend [" + self.module + "]: " + message
	

	def absolutePathForSourceFile (self, folder, basePath, file):
		return folder + '/frontend/' + self.module + '/' + basePath + '/' + file
	

	def absolutePathForTargetFile (self, folder, basePath, file):
		return folder + '/' + self.module + '/' + basePath + '/' + file

	def filterFiles (self, files):
		result = []
		
		for file in files:
			if file.startswith('--'):
				pass
			else:
				result.append(file)
			
		return result
	

	def copyResources (self, sourceFolder, destinationFolder, fileType):
		for file in self.filterFiles(self.settings[fileType]):
			src = self.absolutePathForSourceFile(sourceFolder,      fileType, file)
			dst = self.absolutePathForTargetFile(destinationFolder, fileType, file)
			main.createFolder(os.path.dirname(dst))
			shutil.copy2(src, dst)
		

	def copyResourcesToTargetFolder (self, targetFolder):
		self.copyResources(self.projectDir, targetFolder, 'css')
		self.copyResources(self.projectDir, targetFolder, 'js')
	

	def loadFilesContent (self, basePath, files):
		result = ""
		
		for file in self.filterFiles(files):
			try:
				fileHandler = codecs.open(self.absolutePathForSourceFile(self.projectDir, basePath, file), 'r', 'utf-8')
			except:
				print "FILE: " + file

			result += fileHandler.read() + '\n'
			fileHandler.close()
			
		return result
	

	def template (self):
		processedFile = 'html_template'
		if not self.processedFiles.has_key(processedFile):
			self.processedFiles[processedFile] = self.loadFilesContent('html', ['index_template.html'])
			
		return self.processedFiles[processedFile]
	

	def cssminCompressor (self, css):
		# package found here:
		# - http://stackoverflow.com/questions/222581/python-script-for-minifying-css/2396777#2396777
		# actual downloaded version: http://pypi.python.org/pypi/cssmin/0.1.4
		return cssmin.cssmin(css)
	

	def regexCssCompressor (self, css):
		# http://stackoverflow.com/questions/222581/python-script-for-minifying-css/223689#223689
		
		# remove comments - this will break a lot of hacks :-P
		css = re.sub( r'\s*/\*\s*\*/', "$$HACK1$$", css ) # preserve IE<6 comment hack
		css = re.sub( r'/\*[\s\S]*?\*/', "", css )
		css = css.replace( "$$HACK1$$", '/**/' ) # preserve IE<6 comment hack

		# url() doesn't need quotes
		css = re.sub( r'url\((["\'])([^)]*)\1\)', r'url(\2)', css )

		# spaces may be safely collapsed as generated content will collapse them anyway
		css = re.sub( r'\s+', ' ', css )

		# shorten collapsable colors: #aabbcc to #abc
		css = re.sub( r'#([0-9a-f])\1([0-9a-f])\2([0-9a-f])\3(\s|;)', r'#\1\2\3\4', css )

		# fragment values can loose zeros
		css = re.sub( r':\s*0(\.\d+([cm]m|e[mx]|in|p[ctx]))\s*;', r':\1;', css )

		for rule in re.findall( r'([^{]+){([^}]*)}', css ):

		    # we don't need spaces around operators
		    selectors = [re.sub( r'(?<=[\[\(>+=])\s+|\s+(?=[=~^$*|>+\]\)])', r'', selector.strip() ) for selector in rule[0].split( ',' )]

		    # order is important, but we still want to discard repetitions
		    properties = {}
		    porder = []
		    for prop in re.findall( '(.*?):(.*?)(;|$)', rule[1] ):
		        key = prop[0].strip().lower()
		        if key not in porder: porder.append( key )
		        properties[ key ] = prop[1].strip()
		
		    # output rule if it contains any declarations
		    if properties:
		        print "%s{%s}" % ( ','.join( selectors ), ''.join(['%s:%s;' % (key, properties[key]) for key in porder])[:-1] )
		
		return css
	

	def compressCSS (self, css):
		self.log("compressing CSS")
		#return self.regexCssCompressor(css)
		return self.cssminCompressor(css)
	

	#==========================================================================

	def compressJS_jsmin (self, js):
		self.log("compressing JS code")
		original = StringIO.StringIO(js)
		output = StringIO.StringIO()
		
		jsMinifier = jsmin.JavascriptMinify()
		jsMinifier.minify(original, output)
		
		result = output.getvalue()
		
		original.close()
		output.close()
		
		return result

	def compressJS_closureCompiler (self, js):
		#	Googles Closure compiler
		#	java -jar compiler.jar --js=in1.js --js=in2.js ... --js_output_file=out.js
		
		result = js
		
		return result
	

	def compressJS (self, js):
		return self.compressJS_jsmin(js)
		#return self.compressJS_closureCompiler(js)
	

	#==========================================================================

	def packBookmarklet (self, bookmakeletCode):
		replacers = [
			('isLoginForm',				'ilf'),
			('findLoginForm',			'flf'),
			('findLoginForm',			'flf'),
			('formParameters',			'fp' ),
			('pageParameters',			'pp' ),
			('serializeJSON',			'sj' ),
			('reprString',				'rs' ),
			('logFormParameters',		'lfp'),
			('loadClipperzBookmarklet',	'lcb'),
			('loginForm',				'lf' ),
			('parameters',				'p'  ),
			('inputElementValues',		'iev'),
		]
		result = self.compressJS(bookmakeletCode)
		
		result = re.sub('\n', ' ', result)	#	Fit all in a single line
		# result = re.sub('\s+', ' ', result)	#	Collapse "redundant" spaces. WARNING: this could have some evil side effects on constant strings used inside to code!!
		# result = re.sub('\s?([,\+=\(\)\{\};])\s?', '\\1', result)
		
		for replacer in replacers:
			result = re.sub(replacer[0], replacer[1], result)
		
#		<!--	escaping required to handle the bookmarklet code within the javascript code		-->
		result = re.sub('\://',		'%3a%2f%2f',	result)
		result = re.sub('/',		'%2f',			result)
#		result = re.sub('"',		'%22',			result)
		result = re.sub('"',		'\\"',			result)
		result = re.sub('\"',		'%22',			result)
		result = re.sub('\'',		'%22',			result)
		result = re.sub('\\\\',		'%5c',			result)
		result = result.strip()
		result = 'javascript:' + result

#		replacers = [
#			('aForm',				'_1' ),
#			('inputFields',			'_2' ),
#			('passwordFieldsFound',	'_3' ),
#			('aDocument',			'_6' ),
#			('aLevel',				'_7' ),
#		#	('result',				'_8' ),
#			('documentForms',		'_9' ),
#			('iFrames',				'_c' ),
#			('anInputElement',		'_d' ),
#			('options',				'_f' ),
#			('option',				'_12'),
#			('aLoginForm',			'_13'),
#		#	('action',				'_17'),
#			('radioValues',			'_18'),
#			('radioValueName',		'_19'),
#			('inputElement',		'_1a'),
#			('elementValues',		'_1b'),
#			('radioValue',			'_1c'),
#			('values',				'_1d'),
#			('objtype',				'_21'),
#			('useKey',				'_27'),
#			('bookmarkletDiv',		'_28'),
#			('someParameters',		'_29'),
#			('anException',			'_2a'),
#			('newDiv',				'_2b'),
#			('base_url',			'_2c'),
#			('help_url',			'_2d'),
#			('logo_image_url',		'_2e'),
#			('background_image_url','_2f'),
#			('close_image_url',		'_30'),
#		#	('bookmarklet_textarea','_31'),
#			('innerHTML',			'_32'),
#		]
#		for replacer in replacers:
#			result = re.sub('([^\.])' + replacer[0], '\\1' + replacer[1], result)

#		replacers = [
#			('headNode',			'_1' ),
#			('clipperzScriptNode',	'_2' ),
#		]
#		for replacer in replacers:
#			result = re.sub('([^\.])' + replacer[0], '\\1' + replacer[1], result)

#		result = re.sub(';', ';\n', result)
		
		return result
		
	

	def bookmarklet (self):
		cacheKey = 'bookmarklet'
		if not self.processedFiles.has_key(cacheKey):
			result = 'bookmarklet="' + self.packBookmarklet(self.loadFilesContent('js', ['Bookmarklet.js'])) + '";bookmarklet_ie="' + self.packBookmarklet(self.loadFilesContent('js', ['Bookmarklet_IE.js'])) + '";'
			self.processedFiles[cacheKey] = result
		else:
			result = self.processedFiles[cacheKey]
		
		return result
	

	def replaceTemplatePlaceholders (self, assemblyMode, pageTitle, copyright, css, code, version, versionType):
		result = self.template()
		
		result = result.replace('@page.title@',					pageTitle,		1)
		result = result.replace('@copyright@',					copyright,		1)
		result = result.replace('@css@',						css,			1)
		#result = result.replace('@bookmarklet@',				bookmarklet,	1)
		result = result.replace('@application.version@',		version,		1)
		result = result.replace('@application.version.type@',	versionType,	1)
		result = result.replace('@js_' + assemblyMode + '@',	code,			1)

		result = re.sub('@js_[^@]+@', '', result)

		return result
	

	def assembleCopyrightHeader (self):
		processedFile = 'copyright'
		if not self.processedFiles.has_key(processedFile):
			#self.log("assembling copyright header")
			copyrightValues = self.settings['copyright.values']
			license = self.loadFilesContent('../../properties', ['license.txt'])
			result  = self.loadFilesContent('properties', ['creditsAndCopyrights.txt'])
			
			result = re.sub('@clipperz.license@', license, result)
			for key in copyrightValues:
				result = re.sub('@'+key+'@', copyrightValues[key], result)
			
			self.processedFiles[processedFile] = result
			
		return self.processedFiles[processedFile]
	

	def cssTagsForFiles (self, basePath, files):
		#<link rel="stylesheet" type="text/css" href="./css/reset-min.css" />
		return '\n'.join(map(lambda file: '<link rel="stylesheet" type="text/css" href="./' + basePath + '/' + file + '" />', files))
	

	def cssTagForContent (self, content):
		return '<style type="text/css">' + content + '</style>'
	

	def scriptTagsForFiles (self, basePath, files):
		#<script type='text/javascript' src='./js/src/bookmarklet.js'></script>
		return '\n'.join(map(lambda file: '<script type="text/javascript" src="./' + basePath + '/' + file + '"></script>', files))
	

	def scriptTagForContent (self, content):
		return '<script>' + content + '</script>'
	

	def assembleVersion (self, assemblyMode, pageTitle, copyright, css, js, version, versionType):
		cacheKey = version + "-" + versionType
		if not self.processedFiles.has_key(cacheKey):
			result = self.replaceTemplatePlaceholders(assemblyMode, pageTitle, copyright, css, js, version, versionType)
			self.processedFiles[cacheKey] = result
		else:
			result = self.processedFiles[cacheKey]
		
		#self.log("# cacheKey:\n" + result)
		return result
	

	def assemble (self, assemblyMode='INSTALL', versionType='LIVE'):
		pageTitle = "Clipperz - " + self.module
		if versionType != 'LIVE':
			pageTitle += " [" + versionType + " - " + assemblyMode +"]"
		
		if assemblyMode == 'INSTALL':
			css	= self.cssTagForContent(self.compressCSS(self.loadFilesContent('css', self.settings['css'])))
			js	= self.scriptTagForContent(self.bookmarklet() + '\n' + self.compressJS(self.loadFilesContent('js', self.settings['js'])))
		else:
			css	= self.cssTagsForFiles('css', self.filterFiles(self.settings['css']))
			js	= self.scriptTagForContent(self.bookmarklet()) + '\n' + self.scriptTagsForFiles('js', self.filterFiles(self.settings['js']))
		
		return self.assembleVersion(
			assemblyMode	= assemblyMode,
			pageTitle		= pageTitle,
			copyright		= self.assembleCopyrightHeader(),
			css				= css,
			js				= js,
			version			= self.repositoryVersion(),
			versionType		= versionType
		)




