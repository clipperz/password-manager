from frontendBuilder import FrontendBuilder
import shutil

class DeltaBuilder(FrontendBuilder):

	def name(self):
		return "/delta builder"

	def projectResourceTypes (self):
		return ['js', 'css']

	def copyStaticResources (self, targetFolder):
		pass

	def bookmarklet (self):
		return ""