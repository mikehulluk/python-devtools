
import itertools

from yapsy.IPlugin import IPlugin

import sys


import pdt
import pdt.patch_manager
from pdt.patch_manager import PatchSet, PatchManager
import subprocess
import re


from pdt.plugins.errors import OutstandingChangesException



class PluginOne(IPlugin):
    def print_name(self):
        print "This is plugin Replace"


    def _do_plugin(self, args):
        print """Running 'replace' plugin"""

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




    def do_text(self, text, args):
        print args.src_expr, type(args.src_expr)
        r = re.compile(args.src_expr, re.MULTILINE)
        return r.sub(args.target_expr, text)





    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser('replace', help='* Replace', parents=[parent_parser])
        parser.add_argument('src_expr')
        parser.add_argument('target_expr')
        parser.set_defaults(func=action_wrapper(self._do_plugin))


