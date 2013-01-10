#!/usr/bin/python
# -*- coding: utf-8 -*-


import argparse




from pdtconfig import PDTProfileMgr

import re
import itertools
import sys
import os
import gzip

import yapsy.PluginManager


import yapsy.PluginManager


import pdt_patch_builtin



# Tesing:
# =============
import patch_manager
patch_manager.PatchManager()
# =============



    


def do_grin():
    pass
    
    
    


class PatchToolMgr(object):
   
    def __init__(self):
        pass
    
    
    
    def build_argparser(self, patch_parser):
     
        # Subcommand:
        sp_patch_subparsers = patch_parser.add_subparsers() 
        
        # Built-in commands:
        builtin_handlers = [
            pdt_patch_builtin.DoPatchApply(),
            pdt_patch_builtin.DoPatchDrop(),
            pdt_patch_builtin.DoPatchList(),
        ]
        for handler in builtin_handlers:
            handler.build_arg_parser(sp_patch_subparsers)
        
        # Plugin Commands:
        self._search_patch_plugins(argparser=sp_patch_subparsers)
    
    def _search_patch_plugins(self, argparser):
        self.simplePluginManager = yapsy.PluginManager.PluginManager()
        #self.simplePluginManager.setPluginPlaces(['/home/michael/hw_to_come/python-devtools/src/pdt_patch_plugins/',])
        self.simplePluginManager.collectPlugins()
        
         # Loop round the plugins and print their names.
        for plugin in self.simplePluginManager.getAllPlugins():
            plugin.plugin_object.print_name()
            
            # Hook the plugin into the menu system:
            plugin.plugin_object.build_arg_parser(argparser=argparser)
        
        
    
    



    

pdt_desc = """
PythonDevTools is a set of tools designed to make it easy to apply simple text operations on sets of files.
PDT performs 2 types of operations:
1. Searching, using 'grep' or 'grin'
2. Replacing, using patches on top of files that can then be applied. 
"""



def _build_argparser():
    
    # Some options that are common accross all parsers:
    target_parser = argparse.ArgumentParser(description='target_parser', add_help=False)
    target_parser.add_argument('--target',  action='append', default=None)
    

    # Main parser object
    parser = argparse.ArgumentParser(description=pdt_desc, add_help=True, parents=[target_parser], formatter_class=argparse.RawTextHelpFormatter)
    subparsers = parser.add_subparsers()





    # Global Searching:
    # =================
    sp_grep = subparsers.add_parser('grin', help='Use grep on the targetted files')
    
    # 'grin', if its available:
    # grin is nicely designed to play with other tools, so the 
    # argument parsing is straight forward.
    try:
        import grin
        sp_grin = subparsers.add_parser('grin', parents=[target_parser], help='Use grin on the targetted files')
        grin.get_grin_arg_parser(sp_grin)
        sp_grin.set_defaults(func=do_grin)
    except ImportError:
        pass
    
    
    # Working with patches:
    # =====================
    sp_patch = subparsers.add_parser('patch', help='Build, apply or manage a set of patches', formatter_class=argparse.RawTextHelpFormatter) 
    patch_tool_manager = PatchToolMgr()
    patch_tool_manager.build_argparser(sp_patch)
    

    
        
    return parser




def main():
    # Parse the arguments:
    parser = _build_argparser()
    args = parser.parse_args()

    # Resolve the targets:
    if not args.target:
        args.target = PDTProfileMgr.default_targets
    args.profile_targets = PDTProfileMgr.expand_target_list_to_profiles(args.target)

    # Get and execute the action-functor
    #args.func = lambda s:None
    functor = args.func
    functor(args)



if __name__ == '__main__':
    main()
