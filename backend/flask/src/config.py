import datetime
import os
basedir = os.path.abspath(os.path.dirname(__file__))


CSRF_ENABLED = True


if os.environ.get('DATABASE_URL') is None:
    SQLALCHEMY_DATABASE_URI = ('sqlite:///' + os.path.join(basedir, 'app.db') +
                               '?check_same_thread=False')
else:
    SQLALCHEMY_DATABASE_URI = os.environ['DATABASE_URL']
SQLALCHEMY_MIGRATE_REPO = os.path.join(basedir, 'db_repository')
SQLALCHEMY_RECRD_QUERIES = True

ADMINS = ['you@example.com']


class Config(object):
    DEBUG = False
    TESTING = False
    SQLALCHEMY_ECHO = False
    WTF_CSRF_ENABLED = True
    SECRET_KEY = 'you-will-never-guess'
    sessionTimeout = datetime.timedelta(minutes=-2)


class DevelopmentConfig(Config):
    DEBUG = True
    SQLALCHEMY_ECHO = True


class TestingConfig(Config):
    TESTING = True
