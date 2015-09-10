from frontendBuilder import FrontendBuilder

class Gamma_MobileBuilder(FrontendBuilder):

	def name(self):
		return "/gamma.mobile builder"

	def projectResourceTypes (self):
		return ['js', 'css', 'images']

#	def copyStaticResources (self, targetFolder):
	def copyResourcesToFolder (self, targetFolder, backendSettings):
		pass
