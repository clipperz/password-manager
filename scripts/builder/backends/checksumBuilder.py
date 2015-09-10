#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import shutil
import subprocess
import main

from backendBuilder import BackendBuilder

class ChecksumBuilder(BackendBuilder):
	
	def name(self):
		return "Checksum builder"

	
	def relativePath(self):
		return 'checksum'


	def compileCode (self):
		pass


	def createPackage (self):
		src = self.tempFolder()
		dst = self.targetFolder()

		shutil.copytree(src, dst)


	def run (self):
		print self.name() + " - RUN (checksum)"

		for frontend in self.frontends:
			if (frontend.module == frontend.submodule):
				submoduleExtension = ''
			else:
				submoduleExtension = '.' + frontend.submodule

			main.createFolder(os.path.join(self.frontEndTempFolder(), frontend.module))

			index = self.configureIndexContent(frontend.assemble())
			self.writeToFolder(self.frontEndTempFolder(), os.path.join(frontend.module, 'index' + submoduleExtension + '.html'), index)
			frontend.copyResourcesToFolder(self.frontEndTempFolder(), self.settings)
			self.logChecksums(index, "[" + self.name() + " - " + frontend.module + "] index" + submoduleExtension + ".html checksum")
			print ""

		self.createPackage()


