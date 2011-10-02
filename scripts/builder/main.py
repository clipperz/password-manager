#!/usr/bin/python
# -*- coding: UTF-8 -*-

import sys, os, json
import shutil
import pprint
import frontendBuilder
import codecs
import itertools

from collections   import deque
from phpBuilder    import PhpBuilder
from pythonBuilder import PythonBuilder

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
	print "MODULE: " + module

	if '.' in module:
		moduleComponents = module.split('.')
		module = moduleComponents[0]
		submodule = moduleComponents[1]
	else:
		submodule = module

	settings = codecs.open(projectBaseDir() + '/' + component + '/' + module + '/properties/' + submodule + '.properties.json', 'r', 'utf-8')
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
	
	if backend == 'php':
		backendBuilder = PhpBuilder(projectTargetDir(), frontends, versions, settings)
	elif backend == 'python':
		backendBuilder = PythonBuilder(projectTargetDir(), frontends, versions, settings)
	#elif backend == 'java':
	#	buildJavaBackend (frontends, versions, settings)
	else:
		raise Exception('unrecognized backend: ' + backend)
		
	backendBuilder.run()	

#====================================================================

def build (settings):
	frontends = []
	
	for frontend in settings['frontends']:
		frontends.append(frontendBuilder.FrontendBuilder(frontend, loadSettings('frontend', frontend)))

	for backend in settings['backends']:
		assembleBackend(backend, frontends, settings['versions'])

#--------------------------------------------------------------------

def clean ():
	print "cleaning up …"
	if os.path.exists(projectTargetDir()):
		shutil.rmtree(projectTargetDir())

#--------------------------------------------------------------------

def usage (message):
	if message != None:
		print "ERROR: " + message
	
	print
	print "build.py clean"
	print "build.py clean install"
	print "build.py install --ALL"
	print "build.py install debug --ALL"
	print "build.py clean install debug --ALL"
	print "build.ph install, debug --backends php java --frontends beta gamma"
	print "build.ph install, debug --backends php java --frontends beta gamma gamma.mobile"
	exit(1)

#--------------------------------------------------------------------

def main ():
	settings = {}
	parameters = list(itertools.islice(sys.argv, 1, None))
	
	shouldClean = len(filter(lambda x: x == 'clean', parameters)) > 0
	if (shouldClean):
		clean ()
	
	parameters = filter(lambda x: x != 'clean', parameters)
	versions = list(itertools.takewhile(lambda x: not x.startswith('--'), parameters))
	settings['versions']  = versions;		#['debug',  'install']
	parameters = deque(itertools.dropwhile(lambda x: not x.startswith('--'), parameters))
	
	if len(parameters) > 0:
		parameter = parameters.popleft()
		if parameter == "--ALL":
			settings['frontends'] = ['beta', 'gamma',  'mobile']
			settings['backends']  = ['php',  'python', 'java']
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
	
		if (not settings.has_key('versions')):
			usage("missing 'versions'")
		if (not settings.has_key('frontends')):
			usage("missing 'frontends'")
		if (not settings.has_key('backends')):
			usage("missing 'backends'")
		
		build (settings)
	


if __name__ == "__main__":
    main()