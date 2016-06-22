"""Clipperz models."""
import datetime

from flask.ext.login import UserMixin

from clipperz import app, db


class User(db.Model, UserMixin):

    """Clipperz User model."""

    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(255), unique=True, index=True)
    srp_s = db.Column(db.String(128))
    srp_v = db.Column(db.String(128))
    header = db.Column(db.Text())
    statistics = db.Column(db.Text())
    auth_version = db.Column(db.String(255))
    version = db.Column(db.String(255))
    lock = db.Column(db.String(255))
    records = db.relationship(
        'Record',
        backref='user',
        lazy='dynamic',
        cascade='save-update, merge, delete, delete-orphan')
    otps = db.relationship(
        'OneTimePassword',
        backref='user',
        lazy='dynamic',
        cascade='save-update, merge, delete, delete-orphan')
    offline_saved = db.Column(db.Boolean(), default=False)
    update_date = db.Column(db.DateTime(), nullable=True)

    def updateCredentials(self, credentials):
        """Update user credentials."""
        self.username = credentials['C']
        self.srp_s = credentials['s']
        self.srp_v = credentials['v']
        self.auth_version = credentials['version']

    def update(self, data):
        """Update user object."""
        self.header = data['header']
        self.statistics = data['statistics']
        self.version = data['version']
        if 'lock' in data:
            self.lock = data['lock']
        self.update_date = datetime.datetime.utcnow()
        self.offline_saved = False

    def __repr__(self):
        """User representation."""
        return '<User %r>' % (self.username)

# ------------------------------------------------------------------------------


class RecordVersion(db.Model):

    """
    Model a RecordVersion.

    RecordVersion store attributes associated with a specific version of a
    record.
    """

    id = db.Column(db.Integer(), primary_key=True)
    reference = db.Column(db.String(255), unique=True, index=True)
    header = db.Column(db.Text())
    data = db.Column(db.Text())
    api_version = db.Column(db.String(255))
    version = db.Column(db.Integer())
    previous_version_key = db.Column(db.String(255))
    previous_version_id = db.Column(db.Integer(),
                                    db.ForeignKey('record_version.id'))
    creation_date = db.Column(db.DateTime())
    update_date = db.Column(db.DateTime())
    access_date = db.Column(db.DateTime())

    record_id = db.Column(db.Integer(),
                          db.ForeignKey('record.id'),
                          nullable=False)

    record = db.relationship('Record',
                             backref=db.backref('record_versions',
                                                order_by=id,
                                                cascade='all,delete'))

    def __init__(self, record=None):
        """Initialize a record version."""
        if record:
            self.record = record
            self.record_id = record.id
        self.creation_date = datetime.datetime.utcnow()

    def update(self, someData):
        """Update a record version."""
        app.logger.debug(someData)
        recordVersionData = someData['currentRecordVersion']
        self.reference = recordVersionData['reference']
        self.data = recordVersionData['data']
        self.api_version = recordVersionData['version']
        self.version = self.record.version
        self.previous_version_key = recordVersionData['previousVersionKey']
        self.update_date = datetime.datetime.utcnow()
        self.access_date = datetime.datetime.utcnow()

        self.record.update(someData['record'], self)

    def access(self):
        self.access_date = datetime.datetime.now()
# ------------------------------------------------------------------------------


class Record(db.Model):

    """Model a record.

    A Record has multiple record versions.
    """

    id = db.Column(db.Integer(), primary_key=True)
    user_id = db.Column(db.ForeignKey('user.id'))
    reference = db.Column(db.String(255), unique=True, index=True)
    data = db.Column(db.Text())
    api_version = db.Column(db.String(255))
    version = db.Column(db.Integer(), default=0)
    creation_date = db.Column(db.DateTime())
    update_date = db.Column(db.DateTime())
    access_date = db.Column(db.DateTime())

    current_record_version = db.relationship(
        'RecordVersion',
        uselist=False,
        cascade='save-update, merge, delete, delete-orphan')

    def __init__(self, user=None):
        """Initialize a record."""
        if user:
            self.user_id = user.id
        self.version = 1
        self.creation_date = datetime.datetime.utcnow()

    def update(self, data, record_version):
        """Update a record."""
        self.reference = data['reference']
        self.data = data['data']
        self.api_version = data['version']
        self.update_date = datetime.datetime.now()
        self.access_date = datetime.datetime.now()
        self.current_record_version = record_version
        self.version += 1

    def access(self):
        self.access_date = datetime.datetime.now()

# ------------------------------------------------------------------------------


class OneTimePassword(db.Model):

    """Model a OneTimePassword.

    OneTimePasswords are used to log in to clipperz only once.
    """

    id = db.Column(db.Integer(), primary_key=True)
    user_id = db.Column(db.ForeignKey('user.id'))
    status = db.Column(db.String(255))
    reference = db.Column(db.String(255), unique=True)
    key_value = db.Column(db.String(255))
    key_checksum = db.Column(db.String(255))
    data = db.Column(db.Text())
    version = db.Column(db.String(255))
    creation_date = db.Column(db.DateTime())
    request_date = db.Column(db.DateTime())
    usage_date = db.Column(db.DateTime())

    def __init__(self):
        """Initialize a OneTimePassword."""
        self.creation_date = datetime.datetime.utcnow()

    def update(self, someParameters, aStatus):
        """Update a one time password."""
        self.reference = someParameters['reference']
        self.key_value = someParameters['key']
        self.key_checksum = someParameters['keyChecksum']
        self.data = someParameters['data']
        self.version = someParameters['version']
        self.status = aStatus

    def reset(self, aStatus):
        """Reset a one time password."""
        self.data = ""
        self.status = aStatus
        return self

# ------------------------------------------------------------------------------


class Session(db.Model):

    """Model a session."""

    id = db.Column(db.Integer(), primary_key=True)
    sessionId = db.Column(db.String(255))
    access_date = db.Column(db.DateTime())

    def __init__(self):
        """Initialize a session."""
        self.access_date = datetime.datetime.utcnow()
