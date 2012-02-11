#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from scriptLanguageBuilder import ScriptLanguageBuilder

class PythonBuilder(ScriptLanguageBuilder):
	
	def name(self):
		return "Python builder"

	
	def relativePath(self):
		return 'python'


	def compileCode (self):
		src = self.sourceFolder()
		dst = self.targetFolder()

		shutil.copytree(src, dst)


#	def copyCompiledCodeToTargetDir (self):
#		src = self.sourceFolder()
#		dst = self.targetFolder()
#
#		shutil.copytree(src, dst)
