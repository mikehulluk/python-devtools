#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse

# import pdtconfig
from pdtconfig import PDTProfileMgr

from coreclasses import SubstData, SubstDataActioner, SubstDataOptions
from variable_hunter import find_and_replace_invalid_localvariable_names_in_profile
from pythontidywrapper import tidyfile
import re


def listprofiles(args):
    print 'Available Profiles:'
    for profile in PDTProfileMgr.profiles.values():
        print ' -- ', profile.name

    print 'Profile Groups:'
    for (groupname, group_items) in PDTProfileMgr.profile_groups.iteritems():
        print ' -- ', groupname, ':', ','.join(group_items)

    print 'Default target:', PDTProfileMgr.default_targets
    print 'Default target(Expanded):', [target.name for target in PDTProfileMgr.expand_target_list_to_profiles(PDTProfileMgr.default_targets) ]



def renamelocalvariables(args):
    print 'Renaming local variables:'
    for profile in args.profile_targets:
        find_and_replace_invalid_localvariable_names_in_profile(profile)


def subst_apply(args):
    print 'Applying renames'
    for profile in args.profile_targets:

        options = SubstDataOptions()
        if args.dangerous:
            options.prevent_colisions = False

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        SubstDataActioner.apply_all(substs,options=options)

def tidy(args):
    for profile in args.profile_targets:
        for filename in profile.files:
            tidyfile(filename)


def rename_global(args):
    for profile in args.profile_targets:

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        assert substs.nchanges == 0, "Don't try global renames with unapplyed renames outstanding"

        src = args.src_expr
        if args.match_word:
            src = r'\b%s\b' % src
        tgt = args.target_expr

        for filename in profile.files:
            substs.add_substitution(
                    filename=filename,
                    line_range=None, 
                    src_regex=src,
                    target_regex=tgt)

    if args.apply:
        for profile in args.profile_targets:
            substs = SubstData.from_file(profile.find_and_replace_filename_regular)
            options=SubstDataOptions()
            if args.dangerous:
                options.prevent_colisions = False
            SubstDataActioner.apply_all(substs,options=options)


def subst_clear(args):
    print 'Clearing renames'
    for profile in args.profile_targets:

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        substs.clear()
        substs.write_to_disk()


def grep(args):
    match_files = []
    for profile in args.profile_targets:

        expr = args.expression
        if args.match_word:
            expr = r'\b%s\b' % expr
        r = re.compile(expr)

        for filename in profile.files:
            with open(filename) as f:
                if r.search(f.read()):
                    match_files.append(filename)

    for mf in match_files:
        print re.escape(mf)




# A parser for the targets, which allows us flexibility in the 
# order we add the targets:
target_parser = argparse.ArgumentParser(description='target_parser',add_help=False)
#target_parser.add_argument('--target',  action='append', default=PDTProfileMgr.default_targets)
target_parser.add_argument('--target',  action='append', default=None)


# Main parser object
parser = argparse.ArgumentParser(description='Python Devtools', add_help=False, parents=[target_parser])
subparsers = parser.add_subparsers()

# Subcommands:
# ===============

# 'listprofiles' - List the profiles:
sp_list_profiles = subparsers.add_parser('listprofiles', )
sp_list_profiles.set_defaults(func=listprofiles)

# 'grep' - Simple grepping:
sp_grep = subparsers.add_parser('grep', parents=[target_parser])
sp_grep.add_argument('expression')
sp_grep.add_argument('-w', '--word', action='store_true', default=False, dest='match_word' )
sp_grep.set_defaults(func=grep)

# 'tidy' - calls python-tidy-wrapper on every file:
sp_grep = subparsers.add_parser('tidy', parents=[target_parser])
sp_grep.set_defaults(func=tidy)

# 'subst-clear'
sp_subst_clear = subparsers.add_parser('subst-clear', parents=[target_parser])
sp_subst_clear.set_defaults(func=subst_clear)

# 'subst-apply'
sp_subst_apply = subparsers.add_parser('subst-apply', parents=[target_parser])
sp_subst_apply.add_argument('--dangerous', action='store_true', default=False, dest='dangerous' )
sp_subst_apply.set_defaults(func=subst_apply)

# 'replace' - global search and replace:
sp_replace = subparsers.add_parser('replace', parents=[target_parser])
sp_replace.add_argument('src_expr')
sp_replace.add_argument('target_expr')
sp_replace.add_argument('-w', '--word', action='store_true', default=False, dest='match_word' )
sp_replace.add_argument('-a', '--apply', action='store_true', default=False, )
sp_replace.set_defaults(func=rename_global)

# 'refactor-local-variables'
sp_refactor_local_vars = subparsers.add_parser('refactor-local-variables', parents=[target_parser])
sp_refactor_local_vars.set_defaults(func=renamelocalvariables)


def main():
    # Parse the arguments:
    args = parser.parse_args()

    # Resolve the targets:
    if not args.target:
        args.target = PDTProfileMgr.default_targets
    args.profile_targets = PDTProfileMgr.expand_target_list_to_profiles(args.target)

    # Get and execute the action-functor
    functor = args.func
    functor(args)


main()
