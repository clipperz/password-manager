#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from backendBuilder import BackendBuilder

class PhpBuilder(BackendBuilder):
	
	def name(self):
		return "PHP builder"
	
	def relativePath(self):
		return 'php'
	

