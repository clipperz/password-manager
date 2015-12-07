#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys, os, re
import cssmin
import jsmin
import codecs
import shutil
import StringIO
import urllib

import main

#===============================================================================

class FrontendBuilder(object):

	def __init__ (self, frontend, settings, repositoryVersion):
		if '.' in frontend:
			moduleComponents = frontend.split('.')
			self.module = moduleComponents[0]
			self.submodule = moduleComponents[1]
		else:
			self.module = frontend
			self.submodule = frontend

		self.settings = settings
		self.projectDir = main.projectBaseDir()
		# self.repository = repository.repositoryWithPath(self.projectDir)
		self.repositoryVersion = repositoryVersion
		self.processedFiles = {}

	#---------------------------------------------------------------------------

	def name (self):
		raise NotImplementedError()

	def projectResourceTypes (self):
		raise NotImplementedError()

#	def copyStaticResources (self, targetFolder):
#		raise NotImplementedError()

	def copyResourcesToFolder (self, targetFolder, backendSettings):
		raise NotImplementedError()

	def preprocessCSS (self, targetFile):
		pass

	#---------------------------------------------------------------------------

	def log (self, message):
		module = self.module
		if (self.module != self.submodule):
			module = module + "." + self.submodule
		print "frontend [" + module + "]: " + message
	

	def absolutePathForSources (self):
		return os.path.join(self.projectDir, 'frontend', self.module)

	
	def absolutePathForSourceFile (self, basePath, file):
		return os.path.join(self.absolutePathForSources(), basePath, file)
	

	def absolutePathForTargetFile (self, folder, basePath, file):
		return os.path.join(folder, self.module, basePath, file)


	def filterFiles (self, files):
		result = []
		
		for file in files:
			if file.startswith('--'):
				pass
			else:
				result.append(file)
			
		return result


	def copyResources (self, sourceFolder, destinationFolder, fileType):
		if fileType in self.settings:
			for file in self.filterFiles(self.settings[fileType]):
				src = self.absolutePathForSourceFile(fileType, file)
				dst = self.absolutePathForTargetFile(destinationFolder, fileType, file)
				main.createFolder(os.path.dirname(dst))
				shutil.copy2(src, dst)
		else:
			srcFolder = os.path.join(self.absolutePathForSources(), fileType)
			dstFolder = os.path.join(destinationFolder, self.module, fileType)
			if not(os.path.exists(dstFolder)):
				shutil.copytree(srcFolder, dstFolder)

#			try:
#				shutil.copytree(srcFolder, dstFolder)
#			except:
#				pass

		

#	def copyResourcesToFolder (self, targetFolder, backendSettings):
#		for resoureceType in self.projectResourceTypes():
#			self.copyResources(self.projectDir, targetFolder, resoureceType)
#		self.copyStaticResources(targetFolder)
	
	def copyDebugResourcesToFolder (self, targetFolder):
		for resoureceType in self.projectResourceTypes():
			self.copyResources(self.projectDir, targetFolder, resoureceType)


	def loadIndividualFilesContent (self, basePath, files):
		result = {}
		
		for file in self.filterFiles(files):
			try:
				fileHandler = codecs.open(self.absolutePathForSourceFile(basePath, file), 'r', 'utf-8')
			except:
				print "FILE: " + file

			result[file] = fileHandler.read()
			fileHandler.close()
		
		return result


	def loadFilesContent (self, basePath, files):
		result = ""

		fileContent = self.loadIndividualFilesContent(basePath, files)
		for file in self.filterFiles(files):
			result += fileContent[file] + '\n'
		
		return result


#	def packFilesContent (self, filesContent):
#		result = ""
#		
#		for name, content in filesContent:
#			result += content + '\n'
#			
#		return result


	def template (self):
		processedFile = 'html_template'
		if not self.processedFiles.has_key(processedFile):
		#	self.processedFiles[processedFile] = self.loadFilesContent('html', ['index_template.html'])
			self.processedFiles[processedFile] = self.loadFilesContent('html', [self.settings['html.template']])
			
		return self.processedFiles[processedFile]
	

	#==========================================================================

	def cssminCompressor (self, css):
		# package found here:
		# - http://stackoverflow.com/questions/222581/python-script-for-minifying-css/2396777#2396777
		# actual downloaded version: http://pypi.python.org/pypi/cssmin/0.1.4 -> 0.2.0
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


	def mockCssCompressor (self, css):
		return css;


	def compressCSS (self, css):
		self.log("compressing CSS")
		#return self.regexCssCompressor(css)
		#return self.cssminCompressor(css)
		return self.mockCssCompressor(css)

	#--------------------------------------------------------------------------

	#==========================================================================

	def compressJS_jsmin (self, js, description):
		self.log("compressing " + description + " code")
		original = StringIO.StringIO(js)
		output = StringIO.StringIO()
		
		jsMinifier = jsmin.JavascriptMinify()
		jsMinifier.minify(original, output)
		
		result = output.getvalue()
		
		original.close()
		output.close()
		
		return result

	def compressJS_closureCompiler (self, js, description):
		#	Googles Closure compiler
		#	java -jar compiler.jar --js=in1.js --js=in2.js ... --js_output_file=out.js
		
		result = js
		
		return result
	

	def compressJS (self, js, description):
		return self.compressJS_jsmin(js, description)
		#return self.compressJS_closureCompiler(js, description)
	

	#==========================================================================

	def packBookmarklet (self, bookmakeletCode, version):
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
		result = self.compressJS(bookmakeletCode, version + " bookmarklet")
		
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
		
		return result
	

	def bookmarklet (self):
		cacheKey = 'bookmarklet'
		if not self.processedFiles.has_key(cacheKey):
			result = 'bookmarklet="' + self.packBookmarklet(self.loadFilesContent('js', ['Bookmarklet.js']), "regular") + '";bookmarklet_ie="' + self.packBookmarklet(self.loadFilesContent('js', ['Bookmarklet_IE.js']), "IE") + '";'
			self.processedFiles[cacheKey] = result
		else:
			result = self.processedFiles[cacheKey]
		
		return result
	

	#==========================================================================

	def replaceTemplatePlaceholders (self, pageTitle, copyright, css, code, jsLoadMode, version, versionType):
		result = self.template()
		
		result = result.replace('@page.title@',					pageTitle)
		result = result.replace('@copyright@',					copyright)
		result = result.replace('@css@',						css)
		#result = result.replace('@bookmarklet@',				bookmarklet)
		result = result.replace('@application.version@',		version)
		result = result.replace('@application.version.type@',	versionType)
		result = result.replace('@js_' + jsLoadMode + '@',		code)

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
		return '\n'.join(map(lambda file: '<link rel="stylesheet" type="text/css" href="' + basePath + '/' + file + '" />', files))
	

	def cssTagForContent (self, content):
		return '<style type="text/css">' + content + '</style>'
	

	def scriptTagsForFiles (self, basePath, files):
		#<script type='text/javascript' src='./js/src/bookmarklet.js'></script>
		return '\n'.join(map(lambda file: '<script type="text/javascript" src="' + basePath + '/' + file + '" charset="utf-8"></script>', files))
	

	def scriptTagForContent (self, content):
		return '<script>' + content + '</script>'
	

	def assembleVersion (self, pageTitle, copyright, css, js, jsLoadMode, version, versionType):
		cacheKey = version + "-" + versionType
		if not self.processedFiles.has_key(cacheKey):
			result = self.replaceTemplatePlaceholders(pageTitle, copyright, css, js, jsLoadMode, version, versionType)
			self.processedFiles[cacheKey] = result
		else:
			result = self.processedFiles[cacheKey]
		
		#self.log("# cacheKey:\n" + result)
		return result
	

	def assemble (self, assemblyMode='INSTALL', versionType='LIVE'):

		if versionType == 'LIVE':
			pageTitle = "Clipperz - " + self.module
		else:
			pageTitle = "Clipperz - " + self.module + " [" + versionType + " - " + assemblyMode +"]"

		for cssFile in self.settings['css']:
#			self.preprocessCSS(self.absolutePathForSourceFile('css', cssFile))
			pass

		if assemblyMode == 'INSTALL':
			copyright = self.assembleCopyrightHeader()
			css	=	self.cssTagForContent(self.compressCSS(self.loadFilesContent('css', self.settings['css'])))
			js	=	self.scriptTagForContent(
						self.bookmarklet() +
						'\n' +
						self.compressJS(self.loadFilesContent('js', self.settings['js']), "application")
					)
			jsLoadMode = 'EMBEDDED'

		elif assemblyMode == 'DEBUG':
			copyright = self.assembleCopyrightHeader()
			css	=	self.cssTagsForFiles('./css', self.filterFiles(self.settings['css']))
			js	=	self.scriptTagForContent(
						self.bookmarklet()) + \
				 		'\n' + \
						self.scriptTagsForFiles('./js', self.filterFiles(self.settings['js'])
					)
			jsLoadMode = 'LINKED'

		elif assemblyMode == 'DEVELOPMENT':
			copyright = ""
			css	=	self.cssTagsForFiles('file://' + str(os.path.join(self.absolutePathForSources(), 'css')), self.filterFiles(self.settings['css']))
			js	=	self.scriptTagForContent(
						self.bookmarklet()) + \
					 	'\n' + \
						self.scriptTagsForFiles('file://' + str(os.path.join(self.absolutePathForSources(), 'js')), self.filterFiles(self.settings['js'])
					)
			jsLoadMode = 'LINKED'
			versionType = 'development'

		else:
			raise NotImplementedError()

		return self.assembleVersion(
			pageTitle		= pageTitle,
			copyright		= copyright,
			css				= css,
			js				= js,
			jsLoadMode		= jsLoadMode,
			version			= self.repositoryVersion,
			versionType		= versionType
		)




