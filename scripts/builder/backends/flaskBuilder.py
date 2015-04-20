#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import shutil
from scriptLanguageBuilder import ScriptLanguageBuilder


class FlaskBuilder(ScriptLanguageBuilder):

    def name(self):
        return "Flask builder"

    def relativePath(self):
        return 'flask'

    def createPackage(self):
        super(FlaskBuilder, self).createPackage()
