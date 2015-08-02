#!/usr/bin/env python
from __future__ import print_function
from setuptools import setup
from setuptools.command.test import test as TestCommand
import io
import os
import sys

here = os.path.abspath(os.path.dirname(__file__))


def read(*filenames, **kwargs):
    encoding = kwargs.get('encoding', 'utf-8')
    sep = kwargs.get('sep', '\n')
    buf = []
    try:
        for filename in filenames:
            with io.open(filename, encoding=encoding) as f:
                buf.append(f.read())
    except IOError:
        pass
    return sep.join(buf)

long_description = read('README.txt', 'CHANGES.txt')


class PyTest(TestCommand):
    def finalize_options(self):
        TestCommand.finalize_options(self)
        self.test_args = []
        self.test_suite = True

    def run_tests(self):
        import pytest
        errcode = pytest.main(self.test_args)
        sys.exit(errcode)

setup(
    name='clipperz',
    version='0.1.0',
    url='http://github.com/clipperz/password-manager/',
    license='Apache Software License',
    author='Jokajak',
    tests_require=['pytest'],
    install_requires=['Flask>=0.10.1',
                      'Flask-SQLAlchemy>=1.0',
                      'SQLAlchemy>=0.8.2',
                      'SQLAlchemy-migrate',
                      'Flask-Login',
                      'Flask-KVSession',
                      ],
    cmdclass={'test': PyTest},
    author_email='jokajak@gmail.com',
    description='Clipperz password manager server',
    long_description=long_description,
    packages=['clipperz'],
    include_package_data=True,
    platforms='any',
    test_suite='clipperz.test.test_clipperz',
    classifiers=[
        'Programming Language :: Python',
        'Development Status :: 4 - Beta',
        'Natural Language :: English',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Operating System :: OS Independent',
        'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
    ],
    extras_require={
        'testing': ['pytest'],
    }
)
