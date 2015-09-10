from frontendBuilder import FrontendBuilder
#from scss import Scss

import os
import shutil
import main
import logging


class DeltaBuilder(FrontendBuilder):

	def name(self):
		return "/delta builder"

	def projectResourceTypes (self):
		return ['js', 'css']


#	def copyStaticResources (self, targetFolder):
	def copyResourcesToFolder (self, targetFolder, backendSettings):
		#print "DELTA - copyResourcesToFolder"
		resourcesToCopy = [
			{'folder': 'properties',	'source': 'manifest.appcache',	'target': 'manifest.appcache'}
		]

		for resource in resourcesToCopy:
			#print "copying resource: " + str(resource['source'])
			content = self.loadFilesContent(resource['folder'], [resource['source']])
			content = content.replace('@application.version@',	self.repositoryVersion)
			content = content.replace('@request.path@',			backendSettings['request.path'])

			dst = self.absolutePathForTargetFile(targetFolder, '', resource['target'])
			file = open(dst, 'w')
			file.write(content.encode('utf-8'))
			file.close()


	def bookmarklet (self):
		return ""

	def preprocessCSS (self, targetFile):
		from scss import Scss
		logging.basicConfig()
		scssVariables = {}
		scssCompiler = Scss(
			scss_vars = None,
			scss_opts = {
				'compress': True,
#				'debug_info': True,
			},
			scss_files = self.loadIndividualFilesContent('scss', self.settings['scss']),
#			super_selector = None,
#			live_errors = False,
#			library = ALL_BUILTINS_LIBRARY,
			search_paths = [os.path.join(self.absolutePathForSources(), 'scss')]
		)

		cssFileContent = scssCompiler.compile()

		dst = targetFile
		dst = os.path.join(os.path.dirname(dst), "_" + os.path.basename(dst))
		main.createFolder(os.path.dirname(dst))
		file = open(dst, 'w')
		file.write(cssFileContent.encode('utf-8'))
		file.close()
