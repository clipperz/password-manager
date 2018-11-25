#
#	Copyright 2008-2018 Clipperz Srl
#	
#	This file is part of Clipperz, the online password manager.
#	For further information about its features and functionalities please
#	refer to http://www.clipperz.com.
#	
#	* Clipperz is free software: you can redistribute it and/or modify it 
#     under the terms of the GNU Affero General Public License as published
#     by the Free Software Foundation, either version 3 of the License, or 
#     (at your option) any later version.
#	
#	* Clipperz is distributed in the hope that it will be useful, but 
#     WITHOUT ANY WARRANTY; without even the implied warranty of 
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#	  See the GNU Affero General Public License for more details.
#	
#	* You should have received a copy of the GNU Affero General Public
#	  License along with Clipperz. If not, see http://www.gnu.org/licenses/.
#

import os
import cgi
import wsgiref.handlers

import datetime
import uuid
import random
import hashlib

import logging

from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext import db
from google.appengine.ext.webapp import template

from django.utils import simplejson

#==============================================================================

sessionTimeout = datetime.timedelta(minutes=-2)

def randomSeed():
	return hex(random.getrandbits(32*8))[2:-1]

def clipperzHash(aString):
	#logging.info(">>> string: " + aString)
	firstRound = hashlib.sha256()
	firstRound.update(aString)
	#logging.info("firstRound: " + firstRound.hexdigest() + " - " + firstRound.digest())
	result = hashlib.sha256()
	result.update(firstRound.digest())
	#logging.info("<<< finalResul: " + result.hexdigest())

	return result.hexdigest()
	
#==============================================================================

class User(db.Model):
	username	= db.StringProperty()
	srp_s		= db.StringProperty()
	srp_v		= db.StringProperty()
	header		= db.TextProperty()
	statistics	= db.TextProperty()
	auth_version= db.StringProperty()
	version		= db.StringProperty()
	lock		= db.StringProperty()
	
	def updateCredentials(self, someCredentials):
		self.username		= someCredentials['C']
		self.srp_s			= someCredentials['s']
		self.srp_v			= someCredentials['v']
		self.auth_version	= someCredentials['version']

	def update(self, someData):
		self.header		= someData['header']
		self.statistics	= someData['statistics']
		self.version	= someData['version']
		self.lock		= someData['lock']

#------------------------------------------------------------------------------

class Record(db.Model):
	user			= db.ReferenceProperty(User)
	reference		= db.StringProperty()
	data			= db.TextProperty()
	version			= db.StringProperty()
	creation_date	= db.DateTimeProperty(auto_now_add=True)
	update_date		= db.DateTimeProperty(auto_now_add=True)
	access_date		= db.DateTimeProperty(auto_now_add=True)

#------------------------------------------------------------------------------

class RecordVersion(db.Model):
	record				= db.ReferenceProperty(Record)
	reference			= db.StringProperty()
	header				= db.TextProperty()
	data				= db.TextProperty()
	version				= db.StringProperty()
	previousVersionKey	= db.StringProperty()
	previousVersion		= db.SelfReferenceProperty()
	creation_date		= db.DateTimeProperty(auto_now_add=True)
	update_date			= db.DateTimeProperty(auto_now_add=True)
	access_date			= db.DateTimeProperty(auto_now_add=True)

	def update(self, someData):
		recordData = someData['record'];
		self.parent().reference =	recordData['reference']
		self.parent().data =		recordData['data']
		self.parent().version =		recordData['version']
		self.parent().update_date =	datetime.datetime.now()

		recordVersionData = someData['currentRecordVersion'];
		self.reference =			recordVersionData ['reference']
		self.data =					recordVersionData ['data']
		self.version =				recordVersionData ['version']
		#self.previous_version	=	#recordVersionData ['previousVersion']
		self.previous_version_key =	recordVersionData ['previousVersionKey']
		self.update_date =			datetime.datetime.now()

#------------------------------------------------------------------------------

class OneTimePassword(db.Model):
	user			= db.ReferenceProperty(User)
	status			= db.StringProperty()
	reference		= db.StringProperty()
	keyValue		= db.StringProperty()
	keyChecksum		= db.StringProperty()
	data			= db.TextProperty()
	version			= db.StringProperty()
	creation_date	= db.DateTimeProperty(auto_now_add=True)
	request_date	= db.DateTimeProperty()
	usage_date		= db.DateTimeProperty()

	def update(self, someParameters, aStatus):
		self.reference =		someParameters['reference']
		self.keyValue =			someParameters['key']
		self.keyChecksum =		someParameters['keyChecksum']
		self.data =				someParameters['data']
		self.version =			someParameters['version']
		self.status =			aStatus

	def reset(self, aStatus):
		self.data =		""
		self.status =	aStatus

		return self

#------------------------------------------------------------------------------

class Session(db.Expando):
	sessionId	= db.StringProperty()
	access_date	= db.DateTimeProperty()

#==============================================================================

class MainPage(webapp.RequestHandler):
	def get(self):
		path = os.path.join(os.path.dirname(__file__), 'static%s' % self.request.path)
		self.response.out.write(template.render(path, {}))

#==============================================================================

class XHR(webapp.RequestHandler):

	#==========================================================================

	def get(self):
		logging.info("self.request.path: " + self.request.path)
		if self.request.path == "/dump":
			session = self.getSession()
			userData = {}
			offline_data_placeholder = ""
		
			user = db.Query(User).filter('username =', session.C).get()
		
			userData['users'] = {
				'catchAllUser': {
					'__masterkey_test_value__': 'masterkey',
					's': '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00',
					'v': '112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00'
				}
			}

			records = {}
			for currentRecord in db.Query(Record).ancestor(user):
				versions = {}
				for currentVersion in db.Query(RecordVersion).ancestor(currentRecord):
					versions[currentVersion.reference] ={
						'header':		currentVersion.header,
						'data':			currentVersion.data,
						'version':		currentVersion.version,
						'creationDate':	str(currentVersion.creation_date),
						'updateDate':	str(currentVersion.update_date),
						'accessDate':	str(currentVersion.access_date)
					}

				records[currentRecord.reference] = {
					'data':				currentRecord.data,
					'version':			currentRecord.version,
					'creationDate':		str(currentRecord.creation_date),
					'updateDate':		str(currentRecord.update_date),
					'accessDate':		str(currentRecord.access_date),
					'currentVersion':	currentVersion.reference,
					'versions':			versions
				}

			userData['users'][user.username] = {
				's':					user.srp_s,
				'v':					user.srp_v,
				'version':				user.auth_version,
				'maxNumberOfRecords':	'100',
				'userDetails':			user.header,
				'statistics':			user.statistics,
				'userDetailsVersion':	user.version,
				'records':				records
			}

			offline_data_placeholder = offline_data_placeholder + "_clipperz_dump_data_ = " + simplejson.dumps(userData, indent=4) + "\n"
			offline_data_placeholder = offline_data_placeholder + "Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.Offline();" + "\n"
			offline_data_placeholder = offline_data_placeholder + "Clipperz.Crypto.PRNG.defaultRandomGenerator().fastEntropyAccumulationForTestingPurpose();" + "\n"

			path = os.path.join(os.path.dirname(__file__), 'static/dump.html')

			self.response.headers.add_header('Content-Type', 'text/html')
			self.response.headers.add_header('Content-Disposition', 'attachment', filename='Clipperz.html')
			self.response.out.write(template.render(path, {'offline_data_placeholder': offline_data_placeholder}))

	#==========================================================================

	def post(self):
		method = self.request.get('method')
		parameters = simplejson.loads(self.request.get('parameters'))
		session = self.getSession()
		result = {};

		#----------------------------------------------------------------------

		if method == 'registration':
			message = parameters['message'];
			
			if message == 'completeRegistration':
				user = User()
			
				user.updateCredentials(parameters['credentials'])
				user.update(parameters['user'])
				user.put()

				result['lock'] = user.lock
				result['result'] = "done"

		#----------------------------------------------------------------------

		elif method == 'handshake':
			srp_g = 2L
			srp_n = long("0x%s" % "115b8b692e0e045692cf280b436735c77a5a9e8a9e7ed56c965f87db5b2a2ece3", 16)

			message = parameters['message'];

			#------------------------------------------------------------------

			if message == 'connect':
				session.C = parameters['parameters']['C']
				session.A = parameters['parameters']['A']

				user = db.Query(User).filter('username =', session.C).get()

				if user != None:
					try:
						optId = session.otpId

						oneTimePassword = db.Query(OneTimePassword).filter('keyValue =', optId).get()
						
						if oneTimePassword.parent().username != user.username:
							oneTimePassword.reset('DISABLED').put()
							raise Exception, "User missmatch between the current session and 'One Time Password' user"
						elif oneTimePassword.status != 'REQUESTED':
							oneTimePassword.reset('DISABLED').put()
							raise Exception, "Tring to use an 'One Time Password' in the wrong state"

						oneTimePassword.reset("USED").put()

						result['oneTimePassword'] = oneTimePassword.reference

					except Exception, detail:
						logging.error("connect.optId: " + str(detail))

					session.s = user.srp_s
					session.v = user.srp_v
				else:
					session.s = "112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00"
					session.v = "112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00"

				session.b = randomSeed()
				session.B = hex(long("0x%s" % session.v, 16) + pow(srp_g, long("0x%s" %session.b, 16), srp_n))[2:-1]

				result['s'] = session.s
				result['B'] = session.B

			#------------------------------------------------------------------

			elif message == 'credentialCheck':
				B = long("0x%s" % session.B, 16)
				b = long("0x%s" % session.b, 16)
				A = long("0x%s" % session.A, 16)
				v = long("0x%s" % session.v, 16)
				u = long("0x%s" % clipperzHash(str(B)), 16)
				n = srp_n

				S  = pow((A * pow(v, u, n)), b, n)
				K  = clipperzHash(str(S))
				M1 = clipperzHash(str(A) + str(B) + K)

				if M1 == parameters['parameters']['M1']:
					session.K = K
					M2 = clipperzHash(str(A) + M1 + K)

					result['M2'] = M2
					result["connectionId"] = ""
					result["loginInfo"] = {}
					result["loginInfo"]["latest"] = {}
					result["loginInfo"]["current"] = {}
					result["offlineCopyNeeded"] = "false";
					result["lock"] = "----";
				else:
					result['error'] = "?"

			#------------------------------------------------------------------

			elif message == 'oneTimePassword':
				oneTimePassword = db.Query(OneTimePassword).filter("keyValue =", parameters["parameters"]["oneTimePasswordKey"]).get()

				if oneTimePassword != None:
					if oneTimePassword.status == 'ACTIVE':
						if oneTimePassword.keyChecksum == parameters['parameters']['oneTimePasswordKeyChecksum']:
							#session.userId =	str(oneTimePassword.parent().username)
							session.otpId =		str(oneTimePassword.keyValue)
							
							result['data'] = oneTimePassword.data
							result['version'] = oneTimePassword.version

							oneTimePassword.reset('REQUESTED').put()

						else:
							oneTimePassword.reset('DISABLED').put()
							raise Exception, "The requested One Time Password has been disabled, due to a wrong keyChecksum"
					else:
						raise Exception, "The requested One Time Password was not active"
				else:
					raise Exception, "The requested One Time Password has not been found"

		#----------------------------------------------------------------------

		elif method == 'message':
			if parameters['srpSharedSecret'] == session.K:
				message = parameters['message']

				if message == 'getUserDetails':
					#	{"message":"getUserDetails", "srpSharedSecret":"f18e5cf7c3a83b67d4db9444af813ee48c13daf4f8f6635397d593e52ba89a08", "parameters":{}}
					user = db.Query(User).filter('username =', session.C).get()
					
					result['header'] =		user.header;
					result['statistics'] =	user.statistics;
					result['version'] =		user.version;

				elif 	message == "addNewRecords":
					user = db.Query(User).filter('username =', session.C).get()
					result = db.run_in_transaction(self.addNewRecords, session, user, parameters)

					"""
					user = db.Query(User).filter('username =', session.C).get()
					user.update(parameters['parameters']['user'])
					
					for recordParameter in parameters['parameters']['records']:
						record = Record(parent=user)
						record.put()
						recordVersion = RecordVersion(parent=record)
						recordVersion.put()
						
						recordVersion.update(recordParameter)

						record.put()
						recordVersion.put()

					user.put();

					result['lock'] = user.lock
					result['result'] = 'done'
					"""
					
				elif message == 'getRecordDetail':
					record = db.Query(Record).ancestor(db.Query(User).filter('username =', session.C).get()).filter('reference =', parameters["parameters"]["reference"]).get()
					recordVersion = db.Query(RecordVersion).ancestor(record).get()

					result['currentVersion'] = {}
					result['currentVersion']['reference'] =		recordVersion.reference
					result['currentVersion']['data'] =			recordVersion.data
					result['currentVersion']['header'] =		recordVersion.header
					result['currentVersion']['version'] =		recordVersion.version
					result['currentVersion']['creationDate'] =	str(recordVersion.creation_date)
					result['currentVersion']['updateDate'] =	str(recordVersion.update_date)
					result['currentVersion']['accessDate'] =	str(recordVersion.access_date)

					result['reference'] =						record.reference
					result['data'] =							record.data
					result['version'] =							record.version
					result['creationDate'] =					str(record.creation_date)
					result['updateDate'] =						str(record.update_date)
					result['accessDate'] =						str(record.access_date)
					result['oldestUsedEncryptedVersion'] =		"---"

				elif message == 'updateData':
					user = db.Query(User).filter('username =', session.C).get()
					user.update(parameters['parameters']['user'])

					for recordParameter in parameters['parameters']['records']:
						logging.info('reference =' + recordParameter['record']['reference'])
						record = db.Query(Record).ancestor(user).filter('reference =', recordParameter['record']['reference']).get()
						recordVersion = db.Query(RecordVersion).ancestor(record).get()
						
						recordVersion.update(recordParameter)

						recordVersion.put()
						recordVersion.parent().put()

					user.put();

					result['lock'] = user.lock
					result['result'] = 'done'

			 	elif message == 'deleteRecords':
					user = db.Query(User).filter('username =', session.C).get()
					user.update(parameters['parameters']['user'])

					for recordReference in parameters['parameters']['recordReferences']:
						record = db.Query(Record).ancestor(user).filter('reference =', recordReference).get()
						#recordVersion = db.Query(RecordVersion).ancestor(record).get()
						
						db.delete(db.Query(RecordVersion).ancestor(record))
						record.delete()
						
					user.put()

					result['lock'] = user.lock
					result['result'] = 'done'

				elif message == 'deleteUser':
					user = db.Query(User).filter('username =', session.C).get()
					db.delete(db.Query(RecordVersion).ancestor(user))
					db.delete(db.Query(Record).ancestor(user))
					user.delete()

				elif message == 'addNewOneTimePassword':
					user = db.Query(User).filter('username =', session.C).get()
					user.update(parameters['parameters']['user'])

					oneTimePassword = OneTimePassword(parent=user)
					oneTimePassword.update(parameters['parameters']['oneTimePassword'], "ACTIVE")
					oneTimePassword.put()
					
					user.put()

					result['lock'] = user.lock
					result['result'] = 'done'

				elif message == 'updateOneTimePasswords':
					user = db.Query(User).filter('username =', session.C).get()
					user.update(parameters['parameters']['user'])

					validOtpReferences = parameters['parameters']['oneTimePasswords']
					for currentOtp in db.Query(OneTimePassword).ancestor(user):
						if currentOtp.reference in validOtpReferences:
							pass
						else:
							currentOtp.delete()

					user.put()

					result['result'] = user.lock

				elif message == 'getOneTimePasswordsDetails':
					pass

				elif message == 'getLoginHistory':
					result["result"] = []

				elif message == 'upgradeUserCredentials':
					user = db.Query(User).filter('username =', session.C).get()

					user.updateCredentials(parameters['parameters']['credentials'])
					user.update(parameters['parameters']['user'])

					for oneTimePasswordReference in parameters['parameters']['oneTimePasswords']:
						oneTimePassword = db.Query(OneTimePassword).ancestor(user).filter("reference =", oneTimePasswordReference).get()
						
						if oneTimePassword != None:
							oneTimePassword.data = parameters['parameters']['oneTimePasswords'][oneTimePasswordReference]
							oneTimePassword.put()

					user.put()
					
					result['lock'] = user.lock
					result['result'] = 'done'
					
					"""
					$user = new user();
					$user->Get($_SESSION["userId"]);

					$otp = new onetimepassword();
					
					updateUserCredentials($parameters["parameters"]["credentials"], $user);
					updateUserData($parameters["parameters"]["user"], $user);

					$otpList = $parameters["parameters"]["oneTimePasswords"];
					foreach($otpList as $otpReference=>$otpData) {
						$otpList = $otp->GetList(array(array("reference", "=", $otpReference)));
						$currentOtp = $otpList[0];
						$currentOtp->data = $otpData;
						$currentOtp->Save();
					}

					$user->Save();

					$result["lock"] = $user->lock;
					$result["result"] = "done";
					"""

					#=============================================================

					"""
					java.util.Map	result;
		
					try {
						java.util.Map	credentials;
		
						if (someParameters.get("credentials") != null) {
							credentials = (java.util.Map)someParameters.get("credentials");
						} else {
							credentials = someParameters;
						}

						aUser.setUsername((java.lang.String)credentials.get("C"));
						aUser.setSrpS((java.lang.String)credentials.get("s"));
						aUser.setSrpV((java.lang.String)credentials.get("v"));
						aUser.setVersion((java.lang.String)credentials.get("version"));

						if (someParameters.get("user") != null) {
							com.clipperz.dataModel.EncoderHelper.updateWithMap(aUser, (java.util.Map)someParameters.get("user"));
						}

						if (someParameters.get("oneTimePasswords") != null) {
							java.util.Map	updatedOneTimePasswords;
							java.util.List	usersOneTimePasswords;
							int i,c;
				
							updatedOneTimePasswords = (java.util.Map)someParameters.get("oneTimePasswords");
							usersOneTimePasswords = com.clipperz.dataModel.OneTimePassword.oneTimePasswordsForUser(this.user());
							c = usersOneTimePasswords.size();
							for (i=0; i<c; i++) {
								com.clipperz.dataModel.OneTimePassword	currentOneTimePassword;
					
								currentOneTimePassword = (com.clipperz.dataModel.OneTimePassword)usersOneTimePasswords.get(i);
					
								if (updatedOneTimePasswords.get(currentOneTimePassword.getReference()) != null) {
									currentOneTimePassword.setData((java.lang.String)updatedOneTimePasswords.get(currentOneTimePassword.getReference()));
								}
							}
						}
			
						result = new java.util.Hashtable();
						this.dataContext().commitChanges();
						result.put("lock", this.user().getNewLock());
						result.put("result", "done");
					} catch(java.lang.Exception exception) {
						this.dataContext().rollbackChanges();
						logger.error(exception);
						throw exception;
					}
		
					return result;
					"""

				elif message == 'echo':
					result['result'] = parameters;

			else:
				result['error'] = "Wrong shared secret!"

		#----------------------------------------------------------------------

		elif method == 'logout':
			result['method'] = 'logout'

		#----------------------------------------------------------------------

		else:
			result['method'] = 'PRRRRRR'

		#----------------------------------------------------------------------

		self.saveSession(session)
		self.response.out.write(simplejson.dumps(result))

	#==========================================================================

	def addNewRecords (self, aSession, aUser, someParameters):
		result = {}
		
		#user = db.Query(User).filter('username =', aSession.C).get()
		aUser.update(someParameters['parameters']['user'])
		
		for recordParameter in someParameters['parameters']['records']:
			record = Record(parent=aUser)
			record.put()
			recordVersion = RecordVersion(parent=record)
			recordVersion.put()
			
			recordVersion.update(recordParameter)

			record.put()
			recordVersion.put()

		aUser.put();

		result['lock'] = aUser.lock
		result['result'] = 'done'
		
		return result
	
	#==========================================================================
	
	def getSession(self):
		#logging.info(">>> getSession (%d) => %s" % (db.Query(Session).count(), str(map(lambda v: v.sessionId, db.Query(Session).fetch(100)))) )
		result = None
		try:
			sessionId = self.request.cookies['sessionId']
		except:
			sessionId = None

		#logging.info("wannabe sessionId: " + str(sessionId))
		
		if sessionId != None:
			#query = db.Query(Session)
			#query.filter('sessionId =', sessionId)

			#result = query.get()
			
			#result = db.Query(Session).filter('sessionId =', str(sessionId)).filter('access_date >', (datetime.datetime.utcnow() - sessionTimeout)).get()
			result = db.Query(Session).filter('sessionId =', str(sessionId)).get()
			#logging.info("searching session on datastore. Found: " + str(result))

		if result == None:
			sessionId = str(uuid.uuid4())
			#logging.info("creating a new session with sessionId=" + str(sessionId))
			result = Session(sessionId=sessionId)

		result.access_date = datetime.datetime.utcnow()
		result.put()
		
		#logging.info("<<< getSession (%d)" % db.Query(Session).count())
		
		return result

	#==========================================================================

	def saveSession(self, aSession):
		#logging.info(">>> saveSession (%d)" % db.Query(Session).count())
		#self.response.set_cookie('sessionId', aSession.sessionId, max_age=360, path='/', domain='example.org', secure=True)
		aSession.put()
		self.response.headers.add_header('Set-Cookie', 'sessionId=' + str(aSession.sessionId), path='/')
		self.cleanOldSessions()
		#logging.info("<<< saveSession (%d)" % db.Query(Session).count())

	#==========================================================================

	def cleanOldSessions(self):
		query = db.Query(Session).filter('accessDate <', (datetime.datetime.utcnow() - sessionTimeout))

		expiredSessions = query.count();
		if expiredSessions != 0:
			#logging.info("deleting %d sessions" % expiredSessions)
			pass
			
		"""
		try:
			db.delete(query)
		except Exception, exception:
			logging.error("some issues raised while deleting the expired sessions")
			logging.error("exception type: " + str(type(exception)))
			logging.error("exception: " + str(exception))
		"""
		pass

#==============================================================================

def main():
	application = webapp.WSGIApplication([('/xhr', XHR), ('/dump', XHR), ('/.*', MainPage)], debug=True)
	wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
	main()

