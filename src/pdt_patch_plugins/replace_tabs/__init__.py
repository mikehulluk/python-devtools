#!/usr/bin/python
# -*- coding: utf-8 -*-

import itertools

from yapsy.IPlugin import IPlugin

import sys

from StringIO import StringIO

import pdt
import pdt.patch_manager
from pdt.patch_manager import PatchSet, PatchManager
import subprocess
import re


import pythontidy


from pdt.plugins.errors import OutstandingChangesException




class PluginOne(IPlugin):
    def print_name(self):
        print 'This is plugin Tab-Replacer'

    def _do_plugin(self, args):
        print """Running 'removing tabs' plugin"""

        # Apply the changes:
        file_changes = {}
        for filename in args.file_targets:

            if not filename.endswith('.py'):
                continue

            with open(filename) as f:
                old_contents = f.read()
            new_contents = self.do_text(old_contents, args, filename=filename)

            if PatchManager.get_patches_for_filename(filename):
                raise OutstandingChangesException()

            if old_contents != new_contents:
                file_changes[filename] = (old_contents, new_contents)

        PatchManager.create_patchset(file_changes)
        
        


    def do_text(self, text, args, filename=None):
        if filename:
            print 'Checking file:', filename
        
        return text.replace('\t','    ')


    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        pass
        parser = argparser.add_parser('replace-tabs', help='* PythonTidy', parents=[parent_parser])
        parser.set_defaults(func=action_wrapper(self._do_plugin))
        
        
