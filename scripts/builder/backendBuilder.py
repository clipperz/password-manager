#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys, os, json
import shutil
import hashlib

from functools import reduce
from operator import add
from itertools import izip

import main

#===================================================================

class BackendBuilder(object):
	
	def __init__ (self, projectTargetDir, frontends, versions, settings):
		self.projectTargetDir = projectTargetDir
		self.frontends = frontends
		self.versions = versions
		self.settings = settings

	# --------------------------------------------------------------------------
	
	def name (self):
		raise NotImplementedError()

	
	def relativePath (self):
		raise NotImplementedError()

	
	def compileCode (self):
		raise NotImplementedError()

	
	def createPackage (self):
		raise NotImplementedError()

	# --------------------------------------------------------------------------

	def sourceFolder (self):
		return os.path.join(main.projectBaseDir() , 'backend', self.relativePath(), 'src')


	def tempFolder (self):
		return os.path.join(self.projectTargetDir, '.tmp', self.relativePath())
		

	def frontEndTempFolder (self):
		return self.tempFolder()


	def developmentTargetFolder (self):
		return os.path.join(self.projectTargetDir, 'development')


	def targetFolder (self):
		return os.path.join(self.projectTargetDir, self.relativePath())

		
	# --------------------------------------------------------------------------
	
	def writeToFolder (self, folder, filename, content):
		file = open(os.path.join(folder, filename), 'w')
		file.write(content.encode('utf-8'))
		file.close()
		

#	def configureIndexContent (self, indexContent, requestPathPrefix = ".."):
	def configureIndexContent (self, indexContent):
		result = indexContent
#		result = result.replace( '@request.path@',    requestPathPrefix + '/' + self.settings['request.path']    )
		result = result.replace( '@request.path@',    self.settings['request.path']    )
		result = result.replace( '@dump.path@',       self.settings['dump.path']       )
		result = result.replace( '@should.pay.toll@', self.settings['should.pay.toll'] )

		return result
	

	def formatMAC (self, value):
		x = iter(value)
		return ' '.join([reduce(add, tup) for tup in izip(x, x, x, x)])


	def logChecksums (self, content, message):
		md5Digest		= self.formatMAC(hashlib.md5(content.encode('utf-8')).hexdigest())
		shaDigest		= self.formatMAC(hashlib.sha1(content.encode('utf-8')).hexdigest())
		sha256Digest	= self.formatMAC(hashlib.sha256(content.encode('utf-8')).hexdigest())
		print "-----"
		print message + ": " + md5Digest + " (md5)"
		print message + ": " + shaDigest + " (sha1)"
		print message + ": " + sha256Digest + " (sha256)"
		print "file size: " + "{:,}".format(len(content))
		print "====="
		
	
	def shouldCompileCode (self):
		return ('debug' in self.versions) or ('install' in self.versions)


	def run (self):
		print self.name() + " - RUN"

		if self.shouldCompileCode():
			self.compileCode()
		
			for frontend in self.frontends:
				if (frontend.module == frontend.submodule):
					submoduleExtension = ''
				else:
					submoduleExtension = '.' + frontend.submodule

				main.createFolder(os.path.join(self.frontEndTempFolder(), frontend.module))
				frontend.copyResourcesToFolder(self.frontEndTempFolder(), self.settings)

				if 'debug' in self.versions:
					frontend.copyDebugResourcesToFolder(self.frontEndTempFolder())
					index = self.configureIndexContent(frontend.assemble(assemblyMode='DEBUG', versionType='DEBUG'))
					self.writeToFolder(self.frontEndTempFolder(), os.path.join(frontend.module, 'index_debug' + submoduleExtension + '.html'), index)

				if 'install' in self.versions:
#					frontend.copyResourcesToFolder(self.frontEndTempFolder(), self.settings)
					index = self.configureIndexContent(frontend.assemble())
					self.writeToFolder(self.frontEndTempFolder(), os.path.join(frontend.module, 'index' + submoduleExtension + '.html'), index)

					self.logChecksums(index, "[" + self.name() + " - " + frontend.module + "] index" + submoduleExtension + ".html checksum")
			
			self.createPackage()

#		if 'development' in self.versions:
#			for frontend in self.frontends:
#				if (frontend.module == frontend.submodule):
#					submoduleExtension = ''
#				else:
#					submoduleExtension = '.' + frontend.submodule
#
#				print "FRONTEND module: " + frontend.module
#				print "FRONTEND submodule: " + frontend.submodule
#
##				main.createFolder(os.path.join(self.developmentTargetFolder(), frontend.module))
#				main.createFolder(self.developmentTargetFolder())
#
#				index = self.configureIndexContent(frontend.assemble(assemblyMode='DEVELOPMENT', versionType='DEBUG'), self.settings['development.settings']['url'])
##				self.writeToFolder(self.developmentTargetFolder(), os.path.join(frontend.module, 'index_development' + submoduleExtension + '.html'), index)
#				self.writeToFolder(self.developmentTargetFolder(), os.path.join(frontend.module + submoduleExtension + '.html'), index)

	
#===================================================================
