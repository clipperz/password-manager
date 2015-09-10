#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import shutil
import subprocess
import main

from backendBuilder import BackendBuilder

class DevBuilder(BackendBuilder):
	
	def name(self):
		return "Dev builder"

	
	def relativePath(self):
		return 'dev'


	def compileCode (self):
		pass


	def createPackage (self):
		src = self.tempFolder()
		dst = self.targetFolder()

		shutil.copytree(src, dst)


	def run (self):
		print self.name() + " - RUN (dev)"

		for frontend in self.frontends:
			if (frontend.module == frontend.submodule):
				submoduleExtension = ''
			else:
				submoduleExtension = '.' + frontend.submodule

			main.createFolder(os.path.join(self.frontEndTempFolder(), frontend.module))
			frontend.copyResourcesToFolder(self.frontEndTempFolder(), self.settings)

			index = self.configureIndexContent(frontend.assemble(assemblyMode='DEBUG', versionType='DEBUG'))
			self.writeToFolder(self.frontEndTempFolder(), os.path.join(frontend.module, 'index' + submoduleExtension + '.html'), index)
		
		self.createPackage()


