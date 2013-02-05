
import itertools

from yapsy.IPlugin import IPlugin

import sys


import pdt
import pdt.patch_manager
from pdt.patch_manager import PatchSet, PatchManager
import subprocess


from pdt.plugins.errors import OutstandingChangesException


class PluginOne(IPlugin):
    def print_name(self):
        print "This is plugin Sed"


    def _do_plugin(self, args):
        print 'Running sed plugin'
        
       
        # Apply the changes:
        file_changes = {}
        #for filename in sorted(files):
        for filename in args.file_targets:
            
            with open(filename) as f:
                old_contents = f.read()
            new_contents = self.do_text(old_contents, args.sedexpr)
            
            if PatchManager.get_patches_for_filename(filename):
                raise OutstandingChangesException()
            
            if old_contents != new_contents:
                file_changes[filename] = (old_contents, new_contents)
            
        PatchManager.create_patchset(file_changes)
        
        


    def do_text(self, text, sedexpr):
        p = subprocess.Popen(['sed', sedexpr], stdout=subprocess.PIPE, stdin=subprocess.PIPE ) 
        p.stdin.write(text)
        out = p.communicate()[0]        
        return out
        
        


    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser('sed', help='* Sed')
        parser.add_argument('sedexpr')
        parser.set_defaults(func=action_wrapper(self._do_plugin))
        
        
