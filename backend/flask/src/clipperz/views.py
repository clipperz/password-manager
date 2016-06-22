"""Clipperz views."""
from flask import session, request, g, send_from_directory
from clipperz import app, db, lm
from .models import User
from .api import *  # NOQA
from .exceptions import InvalidUsage
from flask.ext.login import login_required
from os.path import dirname


@lm.user_loader
def load_user(id):
    """Load a user.

    Converts a user id in to a User object.
    """
    return User.query.get(int(id))


@app.before_request
def before_request():
    """Store the current user."""
    g.user = current_user


@app.teardown_appcontext
def shutdown_session(exception=None):
    """Remove the session from the database."""
    db.session.remove()


@app.route('/beta/dump/<string:frontend_version>')
@app.route('/gamma/dump/<string:frontend_version>')
@app.route('/delta/dump/<string:frontend_version>')
@login_required
def dump(frontend_version):
    """Return JSON for a user's data."""
    user = User().query.filter_by(username=session['C']).one()

    if (user != g.user):
        raise InvalidUsage(
            'Your session is incorrect, please re-authenticate',
            status_code=401)

    user.offline_saved = True
    db.session.add(user)
    db.session.commit()
    user_data = {}
    user_data['users'] = {
        'catchAllUser': {
            '__masterkey_test_value__': 'masterkey',
            's': ('112233445566778899aabbccddeeff00112233445566778899'
                  'aabbccddeeff00'),
            'v': ('112233445566778899aabbccddeeff00112233445566778899'
                  'aabbccddeeff00'),
        }
    }

    records = {}
    for current_record in user.records:
        versions = {}
        for version in current_record.record_versions:
            versions[version.reference] = {
                'header':       version.header,
                'data':         version.data,
                'version':      version.api_version,
                'creationDate': str(version.creation_date),
                'updateDate':   str(version.update_date),
                'accessDate':   str(version.access_date)
            }

        records[current_record.reference] = {
            'data':             current_record.data,
            'version':          current_record.version,
            'creationDate':     str(current_record.creation_date),
            'updateDate':       str(current_record.update_date),
            'accessDate':       str(current_record.access_date),
            'currentVersion':   current_record.current_record_version,
            'versions':         versions
        }

    user_data['users'][user.username] = {
        's':                    user.srp_s,
        'v':                    user.srp_v,
        'version':              user.auth_version,
        'maxNumberOfRecords':   '100',
        'userDetails':          user.header,
        'statistics':           user.statistics,
        'userDetailsVersion':   user.version,
        'records':              records
    }

    offline_data_placeholder = (
        '_clipperz_data_ = {user_data}\n'
        'Clipperz.PM.Proxy.defaultProxy = new Clipperz.PM.Proxy.Offline();'
        '\n'
        'Clipperz.Crypto.PRNG.defaultRandomGenerator()'
        '.fastEntropyAccumulationForTestingPurpose();'
        '\n').format(user_data=user_data)

    with open(os.path.join(APP_ROOT,
                           '{0}/index.html'.format(frontend_version))) as f:
        offline_dump = f.read()

    offline_dump = offline_dump.replace('/*offline_data_placeholder*/',
                                        offline_data_placeholder)
    response = make_response(offline_dump)
    content_disposition = "attachment; filename='Clipperz.html'"
    response.headers['Content-Disposition'] = content_disposition

    return response


@app.route('/beta/<path:path>')
def beta(path):
    """Fallback for serving beta version."""
    here = dirname(__file__)
    file_path = "{0}/../beta/".format(here)
    return send_from_directory(file_path, path)


@app.route('/gamma/<path:path>')
def gamma(path):
    """Fallback for serving gamma version."""
    here = dirname(__file__)
    file_path = "{0}/../gamma/".format(here)
    return send_from_directory(file_path, path)


@app.route('/delta/<path:path>')
def delta(path):
    """Fallback for serving delta version."""
    here = dirname(__file__)
    file_path = "{0}/../delta/".format(here)
    return send_from_directory(file_path, path)


@app.route('/pm', methods=['GET', 'OPTIONS', 'POST'])
def pm():
    """Main request handler."""
    method = request.form['method']
    if method not in globals():
        app.logger.error(method)
        raise InvalidUsage('This method is not yet implemented',
                           status_code=501)
    app.logger.debug(method)
    handler = globals()[method]()
    return handler.handle_request(request)
