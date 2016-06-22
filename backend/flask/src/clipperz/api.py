"""Clipperz API handler."""
import json
import random
import hashlib

from flask import jsonify, session, g
from datetime import datetime
from flask.ext.login import logout_user, current_user, login_user, \
    login_required
from sqlalchemy.orm.exc import NoResultFound
from clipperz import app, db
from .exceptions import InvalidUsage
from .models import User, Record, RecordVersion, OneTimePassword


# ==============================================================================
# Helpers
# ==============================================================================
def randomSeed():
    """Generate a random seed."""
    return hex(random.getrandbits(32*8))[2:-1]


def clipperzHash(aString):
    """Calculate a clipperz hash.

    sha256(sha256(aString))
    """
    firstRound = hashlib.sha256()
    firstRound.update(aString)
    result = hashlib.sha256()
    result.update(firstRound.digest())

    return result.hexdigest()
# ==============================================================================
# Method handlers
# ==============================================================================


class HandlerMixin:

    """Mixin for handling requests."""

    def handle_request(self, request):
        """Default method to handle a request."""
        parameters = json.loads(request.form['parameters'])
        app.logger.debug('raw parameters: %s', parameters)
        parameters = parameters['parameters']
        if 'message' in parameters:
            message = parameters['message']
        app.logger.debug('message: %s', message)
        app.logger.debug('parameters: %s', parameters)
        try:
            handler = getattr(self, message)
        except AttributeError:
            raise InvalidUsage(
                'This message handler is not yet implemented for this method',
                status_code=501)
        return handler(parameters, request)


class registration(HandlerMixin):

    """Registration handler."""

    def completeRegistration(self, parameters, request):
        """Complete a registration.

        Create a new user.
        """
        credentials = parameters['credentials']
        data = parameters['user']
        user = User()
        user.updateCredentials(credentials)
        user.update(data)
        db.session.add(user)
        db.session.commit()
        return jsonify(lock=user.lock,
                       result='done')


class handshake(HandlerMixin):

    """Handshake handler.

    This handles the logon process.
    """

    srp_n = '115b8b692e0e045692cf280b436735c77a5a9e8a9e7ed56c965f87db5b2a2ece3'
    srp_g = 2
    srp_n = long(srp_n, 16)

    def connect(self, parameters, request):
        """Process a connect request.

        Attempt to log in by processing the parameters.
        """
        result = {}
        session['C'] = parameters['parameters']['C']
        session['A'] = parameters['parameters']['A']
        app.logger.debug('username: %s', session['C'])

        user = User().query.filter_by(username=session['C']).one()

        if user is not None and session['A'] != 0:
            session['s'] = user.srp_s
            session['v'] = user.srp_v
            if 'otpid' in session:
                try:
                    otpId = session['otpId']

                    one_time_password = OneTimePassword().filter_by(
                        id=otpId
                    ).one()

                    if one_time_password.user.username != user.username:
                        one_time_password.reset('DISABLED')
                        raise Exception(("user mismatch between the current "
                                         "session and 'one time password' "
                                         "user"))
                    elif one_time_password.status != 'requested':
                        one_time_password.reset('DISABLED')
                        raise Exception(("Trying to use an 'one time password'"
                                         " in the wrong state"))

                    one_time_password.reset("USED")

                    result['oneTimePassword'] = one_time_password.reference
                    db.session.add(one_time_password)
                    db.session.commit()

                except Exception, detail:
                    app.logger.error("connect.optid: " + str(detail))

        else:
            # invalid user
            invalid = ('112233445566778899aabbccddeeff00112233445566778899'
                       'aabbccddeeff00')
            session['s'] = invalid
            session['v'] = invalid

        session['b'] = randomSeed()
        k = '0x64398bff522814e306a97cb9bfc4364b7eed16a8c17c5208a40a2bad2933c8e'
        k = long(k, 16)
        app.logger.debug('k: %s (%s)', k, hex(k))
        session['B'] = hex(k * long("0x%s" % session['v'], 16) +
                           pow(self.srp_g,
                               long("0x%s" % session['b'], 16),
                               self.srp_n)
                           )[2:-1]
        result['s'] = session['s']
        result['B'] = session['B']
        app.logger.debug('Session: %s', session)
        return jsonify({'result': result})

    def credentialCheck(self, parameters, request):
        """Check credentials.

        Handles the SRP process.
        """
        country = 'US'
        # hard-coded for development/personal use.
        result = {
            'accountInfo': {
                'features': [
                    'UPDATE_CREDENTIALS',
                    'EDIT_CARD',
                    'CARD_DETAILS',
                    'ADD_CARD',
                    'DELETE_CARD',
                    'OFFLINE_COPY',
                    'LIST_CARDS'
                ],
                'paramentVerificationPending': False,
                'currentSubscriptionType': 'EARLY_ADOPTER',
                'isExpiring': False,
                'latestActiveLevel': 'EARLY_ADOPTER',
                'payments': [],
                'featureSet': 'FULL',
                'latestActiveThreshold': -1.0,
                'referenceDate': datetime.now(),
                'isExpired': False,
                'expirationDate': datetime(4001, 1, 1)
            },
        }

        A = long("0x%s" % session['A'], 16)
        B = long("0x%s" % session['B'], 16)
        b = long("0x%s" % session['b'], 16)
        v = long("0x%s" % session['v'], 16)
        u = long("0x%s" % clipperzHash(str(A) + str(B)), 16)
        s = long("0x%s" % session['s'], 16)
        C = session['C']
        n = self.srp_n

        S = pow((A * pow(v, u, n)), b, n)
        K = clipperzHash(str(S))
        M1 = '{0}{1}{2}{3}{4}{5}{6}'.format(
            '5976268709782868014401975621485889074340014836557888656093758064',
            '39877501869636875571920406529',
            clipperzHash(str(C)),
            str(s),
            str(A),
            str(B),
            str(K)
        )
        M1 = clipperzHash(M1)
        if M1 == parameters['parameters']['M1']:
            session['K'] = K
            M2 = clipperzHash(str(A) + M1 + K)

            result['M2'] = M2
            result['connectionId'] = ''
            result['loginInfo'] = {}
            result['loginInfo']['current'] = {
                'date': datetime.now(),
                'ip': request.remote_addr,
                'browser': request.user_agent.browser,
                'operatingSystem': request.user_agent.platform,
                'disconnectionType': 'STILL_CONNECTED',
                'country': country
            },
            result['loginInfo']['latest'] = {}
            result['offlineCopyNeeded'] = False
            user = User().query.filter_by(username=session['C']).one()
            result['lock'] = user.lock
            login_user(user)
            session['User'] = user
        else:
            result['error'] = '?'

        result['s'] = session['s']
        result['B'] = session['B']
        return jsonify({'result': result})

    def oneTimePassword(self, parameters, request):
        """Handle one time password logins."""
        # "parameters": {
        # "message": "oneTimePassword",
        # "version": "0.2",
        # "parameters": {
        #   "oneTimePasswordKey": "03bd882...396082c",
        #   "oneTimePasswordKeyChecksum": "f73f629...041031d"
        # }
        # }
        result = {}

        try:
            key = parameters['parameters']['oneTimePasswordKey']
            checksum = parameters['parameters']['oneTimePasswordKeyChecksum']
            otp = OneTimePassword().query.filter_by(key_value=key).one()
            if otp.status == 'ACTIVE':
                if otp.key_checksum == checksum:
                    session['userId'] = otp.user.id
                    session['otpId'] = otp.id
                    result['data'] = otp.data
                    result['version'] = otp.version
                    otp.data = ''
                    otp.status = 'REQUESTED'
                else:
                    otp.data = ''
                    otp.status = 'DISABLED'
            db.session.add(otp)
            db.session.commit()
        except NoResultFound, details:
            app.logger.debug('OTP No Results Found: ', details)

        return jsonify({'result': result})


class message(HandlerMixin):

    """Handle messages once logged in."""

    @login_required
    def getUserDetails(self, parameters, request):
        """Get a user's details."""
        app.logger.debug(parameters)
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if srpSharedSecret != session['K']:
            raise InvalidUsage(
                'Your session is invalid, please re-authenticate',
                status_code=401)
        # Online results
        # {"result":
        #     {
        #         "header": "{\"records\":{\"index\":{\"383036...eeefbe48\":\"0\"},\"data\":\"zrhb3/791SDdb48v3vXfPzeDrv0Jhs4rAaOKHx+jDF6pwm/qi9DGSR0JwrprOgwv3bjYJgU2xHA8cuA0bPvABHSHK6fnGwvhSlyYjskY2Cy/WbRJhcA4kw+VUsOjZPRxtM8bSJnSxViAXsghTcya6+5M3MdMJHE=\"},\"directLogins\":{\"index\":{},\"data\":\"s7KYzHwKISmjYufv9h0mpTiM\"},\"preferences\":{\"data\":\"mf8fWjpOQjlV18ukEO9FN3LP\"},\"oneTimePasswords\":{\"data\":\"8tV1yRHv30lsl3FadG9YnTOo\"},\"version\":\"0.1\"}",  # NOQA
        #         "lock": "3D4B4501-D7A9-6E4F-A487-9428C0B6E79D",
        #         "version": "0.4",
        #         "recordsStats": {
        #             "383036...eeefbe48":{
        #                 "updateDate": "Sun, 12 April 2015 17:11:01 UTC",
        #                 "accessDate": "Sun, 12 April 2015 17:11:01 UTC"
        #                 }
        #             },
        #         "offlineCopyNeeded":true,
        #         "statistics":"ByYItDeZMdZ+e/pafp14bGrR"
        #     }
        # }
        # Dev results
        # {"result":
        #    {"header": "{\"records\":{\"index\":{\"843a95d8...5f734b\":\"1\"},\"data\":\"fKgc5Jt9JH/CibCIpcRmwyLuLIvufWchNJga7GoFcWT9K8LR+ai0BvzWBUxcPccivE9zPv2Swe5E8wPEIc+Lv0U73NobJEct7WqBcCdLxszBE1SokxPEZDUVdWVQtAiwgOS219inCFmI5CaB\"},\"directLogins\":{\"index\":{},\"data\":\"rnMQBB81ezh6JKNGXkDCyY+q\"},\"preferences\":{\"data\":\"9jzR9Goo5PGpXbAdmsXHuQGp\"},\"oneTimePasswords\":{\"data\":\"iXEUuQGskZhMyHEwU+3tRGQM\"},\"version\":\"0.1\"}",  # NOQA
        #     "recordStats": {
        #       "843a95d8...5f734b": {
        #           "updateDate": "Sun, 12 Apr 2015 13:08:44 GMT"
        #       }
        #    },
        #    "statistics": "",
        #    "version": "0.4"}}
        result = {}
        user = User().query.filter_by(username=session['C']).one()

        records_stats = {}
        for record in user.records:
            records_stats[record.reference] = {
                'updateDate': record.update_date,
                'accessDate': record.access_date
            }

        result['recordsStats'] = records_stats
        result['header'] = user.header
        result['statistics'] = user.statistics
        result['version'] = user.version
        result['offlineCopyNeeded'] = not user.offline_saved
        result['lock'] = user.lock
        return jsonify({'result': result})

    @login_required
    def saveChanges(self, parameters, request):
        """Save changes to a user's settings."""
        result = {}
        parameters = parameters['parameters']
        if ('user' not in parameters
                or 'records' not in parameters):
            app.logger.debug('saveChanges parameters: %s', parameters)
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)

        user_data = parameters['user']
        record_data = parameters['records']

        user = User().query.filter_by(username=session['C']).one()
        app.logger.debug('user_data: %s', user_data)
        user.update(user_data)
        db.session.add(user)

        if 'updated' in record_data:
            for entry in record_data['updated']:
                reference = entry['record']['reference']
                try:
                    record = Record().query.filter_by(reference=reference).one()
                except NoResultFound:
                    record = Record(user=user)
                    record_version = RecordVersion(record=record)
                    record_version.update(entry)
                    db.session.add(record)
                    db.session.add(record_version)

        if 'deleted' in record_data:
            for reference in record_data['deleted']:
                try:
                    record = Record().query.filter_by(reference=reference).one()
                    db.session.delete(record)
                except NoResultFound:
                    pass

        db.session.commit()
        result['lock'] = user.lock
        result['result'] = 'done'

        return jsonify({'result': result})

    @login_required
    def getRecordDetail(self, parameters, request):
        """Get details about a record."""
        # {
        #  "parameters": {
        #    "srpSharedSecret": "bf79ad3cf0c1...63462a9fb560",
        #    "message": "getRecordDetail",
        #    "parameters": {
        #      "reference": "e3a5856...20e080fc97f13c14c"
        #    }
        #  }
        # }
        app.logger.debug(parameters)
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        reference = parameters['parameters']['reference']
        result = {}

        record = Record().query.filter_by(reference=reference).one()
        app.logger.debug(record.current_record_version)
        record_versions = {}
        oldest_encryption_version = None
        versions = RecordVersion().query.filter_by(record=record).all()
        for record_version in versions:
            version_entry = {}
            version_entry['reference'] = record_version.reference
            version_entry['data'] = record_version.data
            version_entry['header'] = record_version.header
            version_entry['version'] = record_version.api_version
            version_entry['creationDate'] = record_version.creation_date
            version_entry['updateDate'] = record_version.update_date
            version_entry['accessDate'] = record_version.access_date
            try:
                previous_version = RecordVersion().query.filter_by(
                    id=record_version.previous_version_id).one()
                reference = previous_version.reference
                key = record_version.previous_version_key
                version_entry['previousVersion'] = reference
                version_entry['previousVersionKey'] = key
            except NoResultFound:
                pass
            if (not oldest_encryption_version
                    or oldest_encryption_version > record_version.api_version):
                oldest_encryption_version = record_version.api_version
            record_versions[record_version.reference] = version_entry

        result['reference'] = record.reference
        result['data'] = record.data
        result['version'] = record.api_version
        result['creationDate'] = str(record.creation_date)
        result['updateDate'] = str(record.update_date)
        result['accessDate'] = str(record.access_date)
        result['oldestUsedEncryptedVersion'] = oldest_encryption_version
        result['versions'] = record_versions
        result['currentVersion'] = record.current_record_version.reference
        record.current_record_version.access()
        record.access()
        db.session.add(record)
        db.session.add(record_version)
        db.session.commit()
        return jsonify({'result': result})

    @login_required
    def getOneTimePasswordsDetails(self, parameters, request):
        """Get details about a one time password."""
        # {
        #  "parameters": {
        #    "srpSharedSecret": "bf79ad3cf0c1...63462a9fb560",
        #    "message": "getOneTimePasswordsDetails",
        #    "parameters": {}
        #  }
        # }
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        result = {}

        otps = OneTimePassword().query.filter_by(user=current_user).all()
        for otp in otps:
            # {"e8541...af0c6b951":{"status":"ACTIVE"}}
            result[otp.reference] = {'status': otp.status}

        return jsonify({'result': result})

    @login_required
    def getLoginHistory(self, parameters, request):
        """Get login history.

        Not currently fully implemented.
        """
        # {
        #  "parameters": {
        #    "srpSharedSecret": "bf79ad3cf0c1...63462a9fb560",
        #    "message": "getOneTimePasswordsDetails",
        #    "parameters": {}
        #  }
        # }
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        result = {}

        user = User().query.filter_by(username=session['C']).one()

        if (user != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        return jsonify({'result': result})

    @login_required
    def addNewOneTimePassword(self, parameters, request):
        """Add a new one time password."""
        #  "parameters": {
        #    "message": "addNewOneTimePassword",
        #    "srpSharedSecret": "1e8e037a8...85680f931d45dfc20472cf9d1",
        #    "parameters": {
        #      "user": {
        #        "header": <header>
        #        "statistics": "WxHa6VSMmZunOjLCwAVQrkYI",
        #        "version": "0.4",
        #        "lock": "new lock"
        #      },
        #      "oneTimePassword": {
        #        "reference": "ffaec6f...7b123d39b8965e7e5",
        #        "key": "496dc431db...faec137698b16c",
        #        "keyChecksum": "f927c1...eb970552360a311dda",
        #        "data": "GcfCFsoSc5RT...MF8nstFXXHYSXF+Vyj4w=",
        #        "version": "0.4"
        #      }
        #    }
        #  }
        # }
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        result = {}
        parameters = parameters['parameters']

        user = User().query.filter_by(username=session['C']).one()

        if (user != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        user_data = parameters['user']

        app.logger.debug('user_data: %s', user_data)
        user.update(user_data)
        db.session.add(user)

        one_time_password = parameters['oneTimePassword']
        otp = OneTimePassword(
            reference=one_time_password['reference'],
            key_value=one_time_password['key'],
            key_checksum=one_time_password['keyChecksum'],
            data=one_time_password['data'],
            version=one_time_password['version'],
            user=user,
            status='ACTIVE'
        )
        db.session.add(otp)
        db.session.commit()

        return jsonify({'result': result})

    def echo(self, parameters, request):
        """Check the status of the session."""
        result = {}
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        user = User().query.filter_by(username=session['C']).one()

        if (user != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        user.offline_saved = True
        db.session.add(user)
        db.session.commit()
        return jsonify({'result': result})

    @login_required
    def deleteUser(self, parameters, request):
        """Delete a user and all of its records."""
        result = {}
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        user = User().query.filter_by(username=session['C']).one()

        if (user != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        db.session.delete(user)
        db.session.commit()
        return jsonify({'result': result})

    @login_required
    def upgradeUserCredentials(self, parameters, request):
        """Upgrade a user's credentials to a new password."""
        # {"parameters":{"message":"upgradeUserCredentials","srpSharedSecret":"36...d6","parameters":{"credentials":{"C":"59d02038fdb47cee5b7837a697bc8ff41cc66d8844c8fce844cdf45b0b08b1e4","s":"fe40513b99fbaca9bfe51b8d6e9b3eb42b1e01ce8b0ae32461bec0294c1030ed","v":"300b92f4a3e34034d78cd5081f8db36dbf2a4c5f7a41db6954518815a3554278","version":"0.2"},"user":{"header":"{\"records\":{\"index\":{},\"data\":\"VIIDc5vFNoIflyXF8syb8fRS\"},\"directLogins\":{\"index\":{},\"data\":\"9elg3tu2UqsJ0zbUAdQkLE69\"},\"preferences\":{\"data\":\"Sbwar35Ynd/XobuAm4K66lqj\"},\"oneTimePasswords\":{\"data\":\"tAcTsWVTwALSfxXvCChHi4FD\"},\"version\":\"0.1\"}","statistics":"","version":"0.4","lock":null}}}}  # NOQA
        result = {}
        if 'srpSharedSecret' not in parameters:
            raise InvalidUsage(
                'Mal-formed message format.',
                status_code=400)
        srpSharedSecret = parameters['srpSharedSecret']
        if (srpSharedSecret != session['K'] and session['User'] != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        user = User().query.filter_by(username=session['C']).one()

        if (user != g.user):
            raise InvalidUsage(
                'Your session is incorrect, please re-authenticate',
                status_code=401)

        parameters = parameters['parameters']
        user.updateCredentials(parameters['credentials'])
        user.update(parameters['user'])

        if 'oneTimePasswords' in parameters:
            for otpRef in parameters['oneTimePasswords']:
                try:
                    otp = OneTimePassword().query.filter_by(
                        reference=otpRef).one()
                    otp.data = parameters['oneTimePasswords'][otpRef]
                    db.session.add(otp)
                except NoResultFound:
                    pass

        db.session.add(user)
        db.session.commit()
        result['lock'] = user.lock
        result['result'] = 'done'
        return jsonify({'result': result})

    @login_required
    def getCertificatesStatus(self, parameters, request):
        """
        Provides support for BTC Certificate feature.

        No idea how it works.
        """
        return jsonify({'result': {}})


class logout(HandlerMixin):

    """Logout handler."""

    def handle_request(self, request):
        """Handle a logout request."""
        result = {}
        logout_user()
        session.clear()
        result['method'] = 'logout'
        return jsonify({'result': result})
