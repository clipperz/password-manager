#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
import os
import json
import shutil
import pprint
import codecs
import itertools
from collections   import deque

#import frontendBuilder
import repository

pp = pprint.PrettyPrinter(indent=4, depth=4)

#--------------------------------------------------------------------

def scriptDir ():
	return  os.path.dirname(sys.argv[0])

def projectBaseDir ():
	return os.path.abspath(scriptDir() + '/../..')

def projectTargetDir(): 
	return projectBaseDir() + '/target/'

#--------------------------------------------------------------------

def createFolder (path):
	if not os.path.exists(path):
		os.makedirs(path)

#--------------------------------------------------------------------

def loadSettings (component, module):
	# print "MODULE: " + module

	if '.' in module:
		moduleComponents = module.split('.')
		module = moduleComponents[0]
		submodule = moduleComponents[1]
	else:
		submodule = module

	#settings = codecs.open(projectBaseDir() + os.sep + component + os.sep + module + os.sep + 'properties' + os.sep + submodule + '.properties.json', 'r', 'utf-8')
	settings = codecs.open(os.path.join(projectBaseDir(), component, module, 'properties', submodule + '.properties.json'), 'r', 'utf-8')
	result = json.load(settings)
	settings.close

	return result

#====================================================================
# 
# def assembleFrontend (frontend, versions):
# 	result = {}
# 	settings = loadSettings('frontend', frontend)
# 	builder = frontendBuilder.FrontendBuilder(frontend, settings, projectBaseDir())
# 	
# 	for version in versions:
# 		if version == 'install':
# 			result[version] = builder.assembleInstallVersion()
# 		elif version == 'debug':
# 			result[version] = builder.assembleDebugVersion()
# 		else:
# 			raise Exception('unrecognized version: ' + version)
# 	
# 	return result
# 
#====================================================================

def assembleBackend (backend, frontends, versions):
	settings = loadSettings('backend', backend)
	
	builderModuleName = backend + 'Builder'
	builderClassName  = backend.capitalize() + 'Builder'
	#print ("BUILD BACKENDS - module: " + builderModuleName + " , class: " + builderClassName)
	builderModule  = __import__(builderModuleName)
	builderClass   = getattr(builderModule, builderClassName)
	
	backendBuilder = builderClass(projectTargetDir(), frontends, versions, settings)
	backendBuilder.run()	

#====================================================================

def build (settings, repository):
	frontends = []
	
	if repository.areTherePendingChanges():
		if 'install' in settings['versions']:
#			print "\nWARNING: repository has pending changes\n"
			raise Exception("repository has pending changes, can't 'install'")
		else:
			print "\nWARNING: repository has pending changes\n"

	for frontend in settings['frontends']:
		normalizedFrontendName = frontend.replace(".", "_")
		builderModuleName = normalizedFrontendName + 'Builder'
		builderClassName  = normalizedFrontendName.title() + 'Builder'

		#print ("BUILD FRONTEND - module: " + builderModuleName + " , class: " + builderClassName)
		builderModule  = __import__(builderModuleName)
		builderClass   = getattr(builderModule, builderClassName)
		builder = builderClass(frontend, loadSettings('frontend', frontend), repository.version())
		#builder = frontendBuilder.FrontendBuilder(frontend, loadSettings('frontend', frontend), repository.version())
		frontends.append(builder)

	for backend in settings['backends']:
		assembleBackend(backend, frontends, settings['versions'])

#--------------------------------------------------------------------

def clean ():
	# print "cleaning up …"
	if os.path.exists(projectTargetDir()):
		shutil.rmtree(projectTargetDir())

#--------------------------------------------------------------------

def usage (message):
	if message != None:
		print "ERROR: " + message
	
	print
	# print "build clean"
	# print "build clean install"
	print "build install --ALL"
	print "build install debug --ALL"
	print "build install debug development --ALL"
	# print "build clean install debug --ALL"
	print "build install debug --backends php python --frontends beta gamma"
	print "build install debug development --backends php python --frontends beta gamma gamma.mobile"
	exit(1)

#--------------------------------------------------------------------

def allFrontends ():
	return ['beta', 'gamma',  'delta']

def allBackends ():
	return ['php',  'python']

#--------------------------------------------------------------------

def main ():
	settings = {}
	parameters = list(itertools.islice(sys.argv, 1, None))

	sys.path.append(os.path.join(scriptDir(), 'backends'))
	sys.path.append(os.path.join(scriptDir(), 'frontends'))
	currentRepository = repository.repositoryWithPath(projectBaseDir())

	clean()
	versions = list(itertools.takewhile(lambda x: not x.startswith('--'), parameters))
	settings['versions']  = versions;		#['debug', 'install', 'development', 'checksum']
	parameters = deque(itertools.dropwhile(lambda x: not x.startswith('--'), parameters))
	
	if len(parameters) > 0:
		parameter = parameters.popleft()
		if parameter == "--ALL":
			settings['frontends'] = allFrontends()
			settings['backends']  = allBackends()
		else:
			while parameter != None:
				values = list(itertools.takewhile(lambda x: not x.startswith('--'), parameters))
			
				if parameter == "--backends":
					settings['backends'] = values
				elif parameter == "--frontends":
					settings['frontends'] = values
			
				parameters = deque(itertools.dropwhile(lambda x: not x.startswith('--'), parameters))
				if parameters:
					parameter = parameters.popleft()
				else:
					parameter = None
	
		if 'checksum' in settings['versions']:
			if not 'backends' in settings:
				settings['backends'] = []
			settings['backends'].append('checksum')
		
		if (not settings.has_key('versions')):
			usage("missing 'versions'")
		if (not settings.has_key('frontends')):
			usage("missing 'frontends'")
		if (not settings.has_key('backends')):
			usage("missing 'backends'")
		
		build(settings, currentRepository)
	else:
		usage("Suggestions on how to call the 'build' script:")


if __name__ == "__main__":
	main()
