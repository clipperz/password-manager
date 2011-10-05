#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from backendBuilder import BackendBuilder

class PythonBuilder(BackendBuilder):
	
	def name(self):
		return "Python builder"
	
	def relativePath(self):
		return 'python'
	

