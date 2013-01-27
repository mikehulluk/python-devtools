#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
import re
import itertools
import sys
import os
import gzip

import yapsy.PluginManager

from pdtconfig import PDTProfileMgr
from pdt_filelocations import PDTFileLocations

import pdt_patch_builtin
import patch_manager


def do_profile_list(args):
    print 'Listing Profiles:'
    print 'Profiles:'
    for (profile_name, profile) in \
        sorted(PDTProfileMgr.profiles.items()):
        print ' - %s' % profile_name
    print 'Profile Groups:'
    for (profilegroup_name, profile) in \
        sorted(PDTProfileMgr.profile_groups.items()):
        print ' - %s' % profilegroup_name
    print 'Default Targets: ', PDTProfileMgr.default_targets


def do_grin():
    pass


class PatchFunctionWrapper(object):

    def __init__(self, pre_actions=None, post_actions=None):

        self.pre_actions = (pre_actions if pre_actions else [])
        self.post_actions = (post_actions if post_actions else [])

    def __call__(self, func):

        def mycall(args, *args_, **kwargs):
            for action in self.pre_actions:
                action(args=args)
            res = func(args, *args_, **kwargs)

            for action in self.post_actions:
                action(args=args)
            return res

        return mycall


class PatchToolMgr(object):

    def build_argparser(self, patch_parser):

        # Create a parent parser, which we can use to apply default arguments,
        # even though the actions are mostly handled by the plugins.
        # For example, we want to use 'apply'
        patch_function_wrapper = PatchFunctionWrapper()

        def check_apply(args):
            if args.apply:
                pdt_patch_builtin.DoPatchApply().do_apply(args)

        patch_function_wrapper.post_actions.append(check_apply)

        parent_parser = argparse.ArgumentParser('parent',
                add_help=False)
        parent_parser.add_argument('--apply', help='apply',
                                   action='store_true')

        # Create a parser for the subcommand:
        sp_patch_subparsers = patch_parser.add_subparsers()

        # Built-in commands:
        builtin_handlers = [pdt_patch_builtin.DoPatchApply(),
                            pdt_patch_builtin.DoPatchDrop(),
                            pdt_patch_builtin.DoPatchList()]
        for handler in builtin_handlers:
            handler.build_arg_parser(sp_patch_subparsers)

        # Plugin Commands:
        self._search_patch_plugins(argparser=sp_patch_subparsers,
                                   parent_parser=parent_parser,
                                   patch_function_wrapper=patch_function_wrapper)

    def _search_patch_plugins(self, argparser, parent_parser,
                              patch_function_wrapper):
        plugin_dirs = PDTFileLocations.get_plugin_locations()

        self.simplePluginManager = yapsy.PluginManager.PluginManager()
        self.simplePluginManager.setPluginPlaces(plugin_dirs)
        self.simplePluginManager.collectPlugins()

         # Loop round the plugins and print their names.
        for plugin in self.simplePluginManager.getAllPlugins():
            plugin.plugin_object.print_name()

            # Hook the plugin into the menu system:
            plugin.plugin_object.build_arg_parser(argparser=argparser,
                    parent_parser=parent_parser,
                    action_wrapper=patch_function_wrapper)


pdt_desc = \
    """
PythonDevTools is a set of tools designed to make it easy to apply simple text operations on sets of files.
PDT performs 2 types of operations:
1. Searching, using 'grep' or 'grin'
2. Replacing, using patches on top of files that can then be applied. 
"""


def _build_argparser():

    # Some options that are common accross all parsers:
    target_parser = argparse.ArgumentParser(description='target_parser'
            , add_help=False)
    target_parser.add_argument('--target', action='append',
                               default=None)

    # Main parser object
    parser = argparse.ArgumentParser(description=pdt_desc,
            add_help=True, parents=[target_parser],
            formatter_class=argparse.RawTextHelpFormatter)
    subparsers = parser.add_subparsers()

    # Profile management:
    # =================
    sp_profile_parser = subparsers.add_parser('profile',
            help='Information about registered profiles')
    sp_profile_subparsers = sp_profile_parser.add_subparsers()
    sp_profile_list = sp_profile_subparsers.add_parser('list',
            help='List availble profiles')
    sp_profile_list.set_defaults(func=do_profile_list)

    # Global Searching:
    # =================

    # add 'grin', if its available:
    # grin is nicely designed to play with other tools, so the
    # argument parsing is straight forward.
    try:
        import grin
        sp_grin = subparsers.add_parser('grin',
                parents=[target_parser],
                help='Use grin on the targetted files')
        grin.get_grin_arg_parser(sp_grin)
        sp_grin.set_defaults(func=do_grin)
    except ImportError:
        pass

    # Working with patches:
    # =====================
    sp_patch = subparsers.add_parser('patch',
            help='Build, apply or manage a set of patches',
            formatter_class=argparse.RawTextHelpFormatter)

    # Add all the patch-tools
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
    args.profile_targets = \
        PDTProfileMgr.expand_target_list_to_profiles(args.target)

    # Lets also include a list of all the target files as an argument:
    args.file_targets = list(set(itertools.chain(*[target.files
                             for target in args.profile_targets])))
    args.file_targets.sort()
    

    # Get and execute the action-functor
    functor = args.func
    functor(args)



if __name__ == '__main__':
    main()
