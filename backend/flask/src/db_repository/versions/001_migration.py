from sqlalchemy import *
from migrate import *


from migrate.changeset import schema
pre_meta = MetaData()
post_meta = MetaData()
one_time_password = Table('one_time_password', post_meta,
    Column('id', Integer, primary_key=True, nullable=False),
    Column('user_id', Integer),
    Column('status', String),
    Column('reference', String),
    Column('key_value', String),
    Column('key_checksum', String),
    Column('data', Text),
    Column('version', String),
    Column('creation_date', DateTime),
    Column('request_date', DateTime),
    Column('usage_date', DateTime),
)

record = Table('record', post_meta,
    Column('id', Integer, primary_key=True, nullable=False),
    Column('user_id', Integer),
    Column('reference', String),
    Column('data', Text),
    Column('api_version', String),
    Column('version', Integer, default=ColumnDefault(0)),
    Column('creation_date', DateTime),
    Column('update_date', DateTime),
    Column('access_date', DateTime),
)

record_version = Table('record_version', post_meta,
    Column('id', Integer, primary_key=True, nullable=False),
    Column('reference', String),
    Column('header', Text),
    Column('data', Text),
    Column('api_version', String),
    Column('version', Integer),
    Column('previous_version_key', String),
    Column('previous_version_id', Integer),
    Column('creation_date', DateTime),
    Column('update_date', DateTime),
    Column('access_date', DateTime),
    Column('record_id', Integer, nullable=False),
)

session = Table('session', post_meta,
    Column('id', Integer, primary_key=True, nullable=False),
    Column('sessionId', String),
    Column('access_date', DateTime),
)

sessions = Table('sessions', post_meta,
    Column('key', String(length=250), primary_key=True, nullable=False),
    Column('value', LargeBinary, nullable=False),
)

user = Table('user', post_meta,
    Column('id', Integer, primary_key=True, nullable=False),
    Column('username', String),
    Column('srp_s', String(length=128)),
    Column('srp_v', String(length=128)),
    Column('header', Text),
    Column('statistics', Text),
    Column('auth_version', String),
    Column('version', String),
    Column('lock', String),
    Column('offline_saved', Boolean, default=ColumnDefault(False)),
    Column('update_date', DateTime),
)


def upgrade(migrate_engine):
    # Upgrade operations go here. Don't create your own engine; bind
    # migrate_engine to your metadata
    pre_meta.bind = migrate_engine
    post_meta.bind = migrate_engine
    post_meta.tables['one_time_password'].create()
    post_meta.tables['record'].create()
    post_meta.tables['record_version'].create()
    post_meta.tables['session'].create()
    post_meta.tables['sessions'].create()
    post_meta.tables['user'].create()


def downgrade(migrate_engine):
    # Operations to reverse the above upgrade go here.
    pre_meta.bind = migrate_engine
    post_meta.bind = migrate_engine
    post_meta.tables['one_time_password'].drop()
    post_meta.tables['record'].drop()
    post_meta.tables['record_version'].drop()
    post_meta.tables['session'].drop()
    post_meta.tables['sessions'].drop()
    post_meta.tables['user'].drop()
