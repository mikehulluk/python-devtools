#! /usr/bin/python
import argparse

import pdtconfig
from pdtconfig import PDTProfileMgr

from coreclasses import SubstData, SubstDataActioner, SubstDataOptions
from variable_hunter import find_and_replace_invalid_localvariable_names_in_profile
from pythontidywrapper import tidyfile




def listprofiles(args):
    print 'Profiles:'
    for profile in PDTProfileMgr.profiles.values():
        print ' -- ', profile.name


def renamelocalvariables(args):
    print 'Renaming local variables:'
    for profile_name in args.profile:
        profile = PDTProfileMgr.profiles[profile_name]
        find_and_replace_invalid_localvariable_names_in_profile(profile)

def applyrenames(args):
    print 'Applying renames'
    for profile_name in args.profile:
        profile = PDTProfileMgr.profiles[profile_name]

        options=SubstDataOptions()
        if args.dangerous:
            options.prevent_colisions = False

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        SubstDataActioner.apply_all(substs,options=options)

def tidy(args):
    print 'Tidying'
    for profile_name in args.profile:
        profile = PDTProfileMgr.profiles[profile_name]

        for filename in profile.files:
            tidyfile(filename)

def rename_global(args):
    #assert False
    for profile_name in args.profile:
        profile = PDTProfileMgr.profiles[profile_name]

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        assert substs.nchanges == 0, "Don't try global renames with unapplyed renames outstanding"

        src=r'''\bsomaLoc\b'''
        target='soma_loc'

        for filename in profile.files:
            substs.add_substitution(
                    filename=filename,
                    line_range=None, 
                    src_regex=src,
                    target_regex=target)

        options=SubstDataOptions()
        if args.dangerous:
            options.prevent_colisions = False
        
        SubstDataActioner.apply_all(substs,options=options)


def clearrenames(args):
    print 'Clearing renames'
    for profile_name in args.profile:
        profile = PDTProfileMgr.profiles[profile_name]

        substs = SubstData.from_file(profile.find_and_replace_filename_regular)
        substs.clear()
        substs.write_to_disk()




# The actions that the tool supports:
actions = {
    'listprofiles': listprofiles,
    'rename-locals': renamelocalvariables,
    'tidy': tidy,
    'apply': applyrenames,
    'clear': clearrenames,
    'rename-global': rename_global,
}



parser = argparse.ArgumentParser(description='Python Devtools')
parser.add_argument('--fromfile', action='append', default=[], help='')
parser.add_argument('--dangerous', help='', action="store_true", default=False)
parser.add_argument('action', action='store',  help='the action to perform: [%s]' % ', '.join(actions) )
parser.add_argument('profile', action='store', nargs='+',  help='the profiles to target') 








def main():

    args = parser.parse_args()

    # Check all the profiles requested actually exist:
    for profilename in args.profile:
        if not profilename in PDTProfileMgr.profiles:
            raise KeyError("No profile found: '%s'" % profilename)

    # Get the functor
    functor = actions[ args.action ]
    functor(args)




main()
