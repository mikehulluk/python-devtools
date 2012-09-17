#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import glob
from configobj import ConfigObj
from validate import Validator
import sys

# Dynamically add 'glob2' to the path:
# (from ROOT/src/pdt to ROOT/glob2/)
local_path = os.path.dirname(__file__)
glob2_src = os.path.join(local_path, '../../glob2/src/')
sys.path.append(glob2_src)
import glob2


def _ensure_dir_exists(path):
    dirname = os.path.dirname(path)
    if not os.path.exists(dirname):
        os.makedirs(dirname)
    return path


class PDTProfile(object):

    def __init__(self, name, data_dict):
        self.data_dict = data_dict

        self.data_dict['name'] = name

        if not 'working_dir' in self.data_dict:
            self.data_dict['working_dir'] = os.path.join(PDTProfileMgr._pdtrcdir, '.profile-%s/'%name)

        # Check and create upfront:
        _ensure_dir_exists(self.data_dict['working_dir'])

    def __repr__(self):
        return '<Profile: %s (%s) >' % (self.name, self.data_dict)

    @property
    def name(self):
        return self.data_dict['name']

    @property
    def working_dir(self):
        return self.data_dict['working_dir']

    @property
    def find_and_replace_filename_regular(self):
        return os.path.join(self.working_dir, 'findandreplace.stash')

#    @property
#    def find_and_replace_filename_dangerous(self):
#        return os.path.join( self.working_dir, 'findandreplace-dangerous.stash')

    @property
    def unmerged_changes(self):
        pass

    @property
    def files(self):
        src_files = os.path.expanduser(self.data_dict['source_files'])
        files = glob2.glob(src_files)
        files = [filename for filename in files
                 if os.path.isfile(filename)]
        #print files
        return files


class PDTProfileMgr(object):

    _pdtrcdir = os.path.expanduser('~/.python-devtools/')
    _pdtrc = os.path.expanduser('~/.python-devtools/.pdtrc')


    profiles = None
    profile_groups = {}
    default_targets = []

    @classmethod
    def _resolve_target_list_item(cls, item):
        if item in cls.profiles:
            return [cls.profiles[item], ]
        elif item in cls.profile_groups:
            return cls.expand_target_list_to_profiles(cls.profile_groups[item])
        else:
            print 'Unable to resolve target:', item
            assert False

    @classmethod
    def expand_target_list_to_profiles(cls, target_list):
        targets = []
        for target_str in target_list:
            targets.extend(cls._resolve_target_list_item(target_str))
        return sorted(list(set(targets)))

    @classmethod
    def _load_profile(cls, profile_filename):
        profile_name = os.path.splitext(os.path.split(profile_filename)[-1])[0]
        profile_data = ConfigObj(infile=profile_filename)
        cls.profiles[profile_name] = PDTProfile(profile_name, profile_data)


    @classmethod
    def load_rc_file(cls):
        pdtrcdata = ConfigObj(infile=cls._pdtrc)

        if not pdtrcdata:
            return

        # Default profile names:
        if 'default_targets' in pdtrcdata:
            cls.default_targets = pdtrcdata['default_targets']

        if 'profile_groups' in pdtrcdata:
            cls.profile_groups = pdtrcdata['profile_groups']

    @classmethod
    def _init(cls):
        _ensure_dir_exists(cls._pdtrcdir)

        # Load from .rc file:
        cls.load_rc_file()

        # Load profiles:
        cls.profiles = {}
        profile_filenames = glob.glob(cls._pdtrcdir + '/*.profile')
        for profile_filename in profile_filenames:
            cls._load_profile(profile_filename)


PDTProfileMgr._init()
