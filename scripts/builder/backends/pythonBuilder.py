#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from scriptLanguageBuilder import ScriptLanguageBuilder

class PythonBuilder(ScriptLanguageBuilder):
	
	def name(self):
		return "Python builder"

	
	def relativePath(self):
		return 'python'
