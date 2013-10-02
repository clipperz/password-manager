from twisted.internet import reactor
from twisted.web import proxy, server, http, resource, static
from posixpath import basename, dirname

import copy
import sys
import os
import pprint

#--------------------------------------------------------------------

def scriptDir ():
	return  os.path.dirname(sys.argv[0])

def projectBaseDir ():
	return os.path.abspath(scriptDir() + '/../..')

def projectTargetDir(): 
	return projectBaseDir() + '/target/'

#--------------------------------------------------------------------

class ClipperzTestSite(server.Site):

	def __init__(self, resource, logPath=None, timeout=60 * 60 * 12):
		server.Site.__init__(self, resource, logPath, timeout)


	def getResourceFor(self, request):
		uri = request.uri
		uri = uri.split("?", 1)[0]
		uri = uri.split("#", 1)[0]
		if uri.startswith('/json') or uri.startswith('/dump'):
			request.site = self
			request.sitepath = copy.copy(request.prepath)
			result = resource.getChildForRequest(self.resource, request)

		else:
			pathParts = uri.split('/')
			version = pathParts[1]

			if pathParts[2].startswith('index.'):
				contentType = 'text/html'
				absoluteFilePath = os.path.join(projectTargetDir(), 'dev', version, pathParts[2])
				result = static.File(absoluteFilePath, contentType)
			elif pathParts[2].endswith('.webapp'):
				contentType = 'application/x-web-app-manifest+json'
#				absoluteFilePath = os.path.join(projectTargetDir(), 'dev', version, pathParts[2])
				absoluteFilePath = os.path.join(projectBaseDir(), 'frontend', version, 'properties', pathParts[2])
				result = static.File(absoluteFilePath, contentType)
			elif pathParts[2].endswith('.appcache'):
				contentType = 'text/cache-manifest'
				absoluteFilePath = os.path.join(projectBaseDir(), 'frontend', version, 'properties', pathParts[2])
				result = static.File(absoluteFilePath, contentType)
			else: 
#	http://homer.local:8888/beta/css/clipperz/images/loginInfoBackground.png
#	pathParts: ['', 'beta', 'css', 'clipperz', 'images', 'loginInfoBackground.png']
				try:
					imagePathIndex = pathParts.index('images')
					resourceType = 'images'
					for _ in range(2, imagePathIndex):
						del pathParts[2]
				except:
					resourceType = pathParts[2]

				basePath = projectBaseDir() + '/frontend'
				if resourceType == 'images':
					fileExtension = os.path.splitext(uri)[1]
					if fileExtension == '.png':
						contentType = 'image/png'
					elif fileExtension == '.jpg':
						contentType = 'image/jpeg'
					elif fileExtension == '.gif':
						contentType = 'image/gif'
					else:
						print "ERROR - unknown image extension: " + fileExtension

					absoluteFilePath = basePath + '/'.join(pathParts)
				else:
					resourceType = pathParts[2]

					if resourceType == 'css':
						contentType = 'text/css'
					elif resourceType == 'js':
						contentType = 'text/javascript'
					else:
						contentType = 'text/html'
					
					absoluteFilePath = basePath + uri

				result = static.File(absoluteFilePath, contentType)


		return result



def main ():
	site = ClipperzTestSite(proxy.ReverseProxyResource('localhost', 8080, '/java-backend'))
#	site = ClipperzTestSite(proxy.ReverseProxyResource('www.clipperz.com', 443, '/'))
	reactor.listenTCP(8888, site)
	reactor.run()


if __name__ == "__main__":
	main()

