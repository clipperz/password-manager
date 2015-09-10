from frontendBuilder import FrontendBuilder
import shutil

class GammaBuilder(FrontendBuilder):

	def name(self):
		return "/gamma builder"

	def projectResourceTypes (self):
		return ['js', 'css', 'images']

#	def copyStaticResources (self, targetFolder):
	def copyResourcesToFolder (self, targetFolder, backendSettings):
		self.copyResources(self.projectDir, targetFolder, 'images')

		resourcesToCopy = [
			{'folder': 'html',	'source': 'exit_template.html',	'target': 'exit.html'},
#			{'folder': 'html',	'source': 'exit_template.html',	'target': 'logout.html'},
			{'folder': 'css',	'source': 'static.css',			'target': 'static.css'}
		]

		for resource in resourcesToCopy:
			src = self.absolutePathForSourceFile(resource['folder'], resource['source'])
			dst = self.absolutePathForTargetFile(targetFolder, '', resource['target'])
			shutil.copy2(src, dst)

		# src = self.absolutePathForSourceFile('html', 'exit_template.html')
		# dst = self.absolutePathForTargetFile(targetFolder, '', 'exit.html')
		# shutil.copy2(src, dst)

		# src = self.absolutePathForSourceFile('css', 'static.css')
		# dst = self.absolutePathForTargetFile(targetFolder, '', 'static.css')
		# shutil.copy2(src, dst)
