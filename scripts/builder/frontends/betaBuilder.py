from frontendBuilder import FrontendBuilder
import shutil

class BetaBuilder(FrontendBuilder):

	def name(self):
		return "/beta builder"

	def projectResourceTypes (self):
		return ['js', 'css', 'images']

#	def copyStaticResources (self, targetFolder):
	def copyResourcesToFolder (self, targetFolder, backendSettings):
		self.copyResources(self.projectDir, targetFolder, 'images')

		for file in self.filterFiles(self.settings['staticResources']):
			src = self.absolutePathForSourceFile('staticResources', file)
			dst = self.absolutePathForTargetFile(targetFolder, '', file)
			shutil.copy2(src, dst)

