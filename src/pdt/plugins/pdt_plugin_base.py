#!/usr/bin/python
# -*- coding: utf-8 -*-

from yapsy.IPlugin import IPlugin

from pdt.patch_manager import PatchSet, PatchManager
import subprocess
import re


from pdt.plugins.errors import OutstandingChangesException

class PatchPlugin(IPlugin):

    def build_arg_parser(self, argparser):
        raise NotImplementedError()

    def plugin_name(self,):
        raise NotImplementedError()

    #def print_name(self):
    #    print "This is plugin: %s" % self.plugin_name


    def _do_plugin(self, args):
        print """Running 'Python-whitespace' plugin"""

        # Apply the changes:
        file_changes = {}
        for filename in args.file_targets:

            with open(filename) as f:
                old_contents = f.read()
            new_contents = self.do_text(old_contents, args)

            if PatchManager.get_patches_for_filename(filename):
                raise OutstandingChangesException()

            if old_contents != new_contents:
                file_changes[filename] = (old_contents, new_contents)

        PatchManager.create_patchset(file_changes)


