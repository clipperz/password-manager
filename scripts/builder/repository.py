#!/usr/bin/env python
# -*- coding: UTF-8 -*-

def repositoryWithPath (path):
	try:
		# pip install hgapi
		import hgapi

		repo = hgapi.Repo(path)
		result = HgRepository(repo, path)
		repo.hg_status()
	except:
		try:
			from git import Repo
			repo = Repo(path)
			result = GitRepository(repo, path)
		except ImportError, exception:
			print "Failed to import git, please install http://gitorious.org/git-python"
			print "Use sudo apt-get install python-git for Ubuntu/Debian"
			print "Use sudo yum install GitPython for Fedora/RHEL/CentOS"
			print "Or manually running the following command: easy_install gitpython"
		except:
			result = SnapshotRepository('', path)


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

		return result


#===================================================================


class GitRepository(Repository):
	#	http://gitorious.org/git-python

	def revision (self):
		try:
			return self.repository.head.commit.hexsha
		except:
			return self.repository.commits()[0].id


	def areTherePendingChanges (self):
		try:
			return self.repository.is_dirty()
		except TypeError, te:
			return self.repository.is_dirty



#===================================================================


class HgRepository(Repository):
	##	http://mercurial.selenic.com/wiki/MercurialApi
	#	https://bitbucket.org/haard/hgapi

	def revision (self):
		return 'hg: ' + str(self.repository['tip'].node)

	def areTherePendingChanges (self):
		return not all(map(lambda fileList: len(fileList) == 0, self.repository.hg_status()))


#===================================================================


class SnapshotRepository(Repository):

	def revision (self):
		return 'SNAPSHOT'

 
	def areTherePendingChanges (self):
		return False
