#! /usr/bin/python

import pdtconfig
from pdtconfig import PDTProfileMgr

import argparse





def listprofiles(*args, **kwargs):
    print 'Profiles:'
    for profile in PDTProfileMgr.profiles.values():
        print ' -- ', profile.name


actions = {
    'listprofiles': listprofiles,


        }



parser = argparse.ArgumentParser(description='Python Devtools')
parser.add_argument('--fromfile', action='append', default=[], help='process a list full of substituions')
parser.add_argument('action', action='store',  help='process a list full of substituions')








def main():
    #print 'Python Devtools'

    args = parser.parse_args()

    # Get the functor
    functor = actions[ args.action ]
    functor()
    
    #print args



main()
