#!/usr/bin/env python
# -*- coding: UTF-8 -*-


def repositoryWithPath (path):
	try:
		from dulwich.repo import Repo

		repo = Repo(path)
		result = GitRepository(repo, path)
	except ImportError:
		print "Failed to import dulwich, please install http://www.samba.org/~jelmer/dulwich/"
	except:
		from mercurial import ui, hg

		repo = hg.repository(ui.ui(), path)
		result = HgRepository(repo, path)

	return result


#===================================================================


class Repository(object):

	def __init__ (self, repository, path):
		self.repository = repository
		self.path = path


	def revision (self):
		raise NotImplementedError()
	

	def areTherePendingChanges (self):
		raise NotImplementedError()


	def version (self):
		result = self.revision()
		if self.areTherePendingChanges():
			result = '>>> ' + result + ' <<<'

		# print "VERSION: " + result
		return result


#===================================================================


class GitRepository(Repository):

	def revision (self):
		return self.repository.refs['HEAD']


	def areTherePendingChanges (self):
		return repository.is_dirty()


#===================================================================


class HgRepository(Repository):
	#	http://mercurial.selenic.com/wiki/MercurialApi

	def revision (self):
		return 'hg:' + str(self.repository['tip'])
	

	def areTherePendingChanges (self):
		# TODO: FIXME: repository.status() does not report 'unknown(?)' files. :(
		return not all(map(lambda fileList: len(fileList) == 0, self.repository.status()))


#===================================================================
