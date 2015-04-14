from twisted.internet	import reactor
from twisted.web		import proxy, server, http, resource, static
from posixpath			import basename, dirname

import copy
import sys
import os
import uuid
import pprint
import codecs
import time
import subprocess

#! sys.path.append("/usr/local/lib/python2.7/site-packages")
# > export PYTHONPATH=/usr/local/lib/python2.7/site-packages

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

		if uri == '/':
			# This serves the message, but also throws an exception; can't understand why...
			result = static.Data('<html>In production you would now be on https://clipperz.is/</html>', 'text/html')
		elif uri.startswith('/json') or uri.startswith('/dump'):
			resource.prepath = ['app']
			result = resource.getChildForRequest(self.resource, request)
		elif uri.startswith('/payment'):
			resource.prepath = ['payment']
			result = resource.getChildForRequest(self.resource, request)
		elif uri == '/favicon.ico':
			return
		else:
			pathParts = uri.split('/')
			version = pathParts[1]

			if pathParts[2].startswith('index.'):
				print("-> index")
				contentType = 'text/html'
				absoluteFilePath = os.path.join(projectTargetDir(), 'dev', version, pathParts[2])
#				print("INDEX.HTML absolute path " + str(absoluteFilePath))
				result = static.File(absoluteFilePath, contentType)
			elif pathParts[2].endswith('.webapp'):
				print("-> webapp")
				contentType = 'application/x-web-app-manifest+json'
				absoluteFilePath = os.path.join(projectBaseDir(), 'frontend', version, 'properties', pathParts[2])
				result = static.File(absoluteFilePath, contentType)
#			elif pathParts[2].endswith('.appcache'):
			elif pathParts[2].endswith('.appcache_disabled'):
				print("-> appcache")
				contentType = 'text/cache-manifest'
				absoluteFilePath = os.path.join(projectBaseDir(), 'frontend', version, 'properties', pathParts[2])
				fileContent = codecs.open(absoluteFilePath, 'r', 'utf-8').read()
#				fileContent = fileContent.replace('@application.version@', str(uuid.uuid1()))
				fileContent = fileContent.replace('@application.version@', str(round(time.time())))
				result = static.Data(str(fileContent), contentType)
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
#						print("-> image - png")
						contentType = 'image/png'
					elif fileExtension == '.jpg':
#						print("-> image - jpg")
						contentType = 'image/jpeg'
					elif fileExtension == '.gif':
#						print("-> image - gif")
						contentType = 'image/gif'
					else:
						print "ERROR - unknown image extension: " + fileExtension

					absoluteFilePath = basePath + '/'.join(pathParts)
				else:
					resourceType = pathParts[2]

					if resourceType == 'css':
#						print("-> css")
						contentType = 'text/css'
					elif resourceType == 'js':
#						print("-> js")
						contentType = 'text/javascript'
					else:
#						print("-> text/html")
						contentType = 'text/html'
					
					absoluteFilePath = basePath + uri

				result = static.File(absoluteFilePath, contentType)

#		print("RESULT\n" + str(result))
		return result

def main ():
#	site = ClipperzTestSite(proxy.ReverseProxyResource('localhost', 8084, '/java-backend'))
	site = ClipperzTestSite(proxy.ReverseProxyResource('localhost', 8084, '/app'))
#	site = ClipperzTestSite(proxy.ReverseProxyResource('www.clipperz.com', 443, '/'))
	reactor.listenTCP(8888, site)

	reactor.run()


if __name__ == "__main__":
	main()
