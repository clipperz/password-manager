#!/usr/bin/env sh
# Sets up flask development environment and activates it

virtualenv .

bin/python setup.py develop
bin/python clipperz.py
