from frontendBuilder import FrontendBuilder
import shutil

class DeltaBuilder(FrontendBuilder):

	def name(self):
		return "/delta builder"

	def projectResourceTypes (self):
		return ['js', 'css']

	def copyStaticResources (self, targetFolder):
		resourcesToCopy = [
			{'folder': 'properties',	'source': 'manifest.appcache',	'target': 'manifest.appcache'}
		]

		for resource in resourcesToCopy:
			src = self.absolutePathForSourceFile(resource['folder'], resource['source'])
			dst = self.absolutePathForTargetFile(targetFolder, '', resource['target'])
			shutil.copy2(src, dst)

	def bookmarklet (self):
		return ""