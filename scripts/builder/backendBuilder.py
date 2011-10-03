#!/usr/bin/python
# -*- coding: UTF-8 -*-

import sys, os, json
import shutil
import main
import hashlib

class BackendBuilder:
	
	def __init__ (self, projectTargetDir, frontends, versions, settings):
		self.projectTargetDir = projectTargetDir
		self.frontends = frontends
		self.versions = versions
		self.settings = settings
	
	def name (self):
		raise NotImplementedError()
	
	def relativePath (self):
		raise NotImplementedError()
	
	def compileCode (self):
		pass
	
	def copyCompiledCodeToTargetDir (self):
		src = self.sourceFolder()
		dst = self.targetFolder()
		main.createFolder(os.path.dirname(dst))
		shutil.copytree(src, dst)

	def sourceFolder (self):
		return main.projectBaseDir() + '/backend/' + self.relativePath() + '/src'
	

	def targetFolder (self):
		return self.projectTargetDir + self.relativePath()
	
	def createTargetFolder (self):
		main.createFolder(self.targetFolder())
	

#	def copyFrontendResources (self, frontend):
#		print "copying resources for frontend: " + frontend
#		print "SETTINGS: " + str(self.settings)
	

	def writeToTargetFolder (self, filename, content):
		file = open(self.targetFolder() + '/' + filename, 'w')
		file.write(content.encode('utf-8'))
		file.close()
		

	def configureIndexContent (self, indexContent):
		result = indexContent
		result = result.replace( '@request.path@',    self.settings['request.path']    )
		result = result.replace( '@should.pay.toll@', self.settings['should.pay.toll'] )

		return result
	

	def logChecksums (self, content, message):
		md5Digest		= hashlib.md5(content.encode('utf-8')).hexdigest()
		shaDigest		= hashlib.sha1(content.encode('utf-8')).hexdigest()
		sha256Digest	= hashlib.sha256(content.encode('utf-8')).hexdigest()
		print message + ": " + md5Digest + " (md5)"
		print message + ": " + shaDigest + " (sha1)"
		print message + ": " + sha256Digest + " (sha256)"
		
	

	def run (self):
		print self.name() + " - RUN"

		self.compileCode()
		self.copyCompiledCodeToTargetDir()
		
		for frontend in self.frontends:
			frontendPath = frontend.module + '/'
			if 'debug' in self.versions:
				frontend.copyResourcesToTargetFolder(self.targetFolder())
				#self.writeToTargetFolder(frontendPath + 'index_debug.html', self.configureIndexContent(frontend.assembleDebugVersion()))
				self.writeToTargetFolder(frontendPath + 'index_debug.html', self.configureIndexContent(frontend.assemble(assemblyMode='DEBUG', versionType='DEBUG')))
			
			if 'install' in self.versions:
				index = self.configureIndexContent(frontend.assemble())
				self.writeToTargetFolder(frontendPath + 'index.html', index)
				self.logChecksums(index, "[" + self.name() + " - " + frontend.module + "] index.html checksum")
	
