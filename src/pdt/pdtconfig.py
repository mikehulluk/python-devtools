#!/usr/bin/python
# -*- coding: utf-8 -*-

import os

import configobj# import ConfigObj


# glob2 is copied along with this package,
# so lets make sure its on the path:
#import dependancies_setup
import glob2



from pdt.filelocations import PDTFileLocations
import pdt.util

class PDTProfile(object):

    def __init__(self, name, data_dict):
        self.data_dict = data_dict

        self.data_dict['name'] = name

        if not 'working_dir' in self.data_dict:
            self.data_dict['working_dir'] = \
                os.path.join(PDTProfileMgr._pdtrcdir, '.profile-%s/'
                             % name)

        # Check and create upfront:
        pdt.util.ensure_dir_exists(self.data_dict['working_dir'])

    def __repr__(self):
        return '<Profile: %s (%s) >' % (self.name, self.data_dict)

    @property
    def name(self):
        return self.data_dict['name']

    @property
    def working_dir(self):
        return self.data_dict['working_dir']

    @property
    def unmerged_changes(self):
        pass


    @classmethod
    def _get_file_list(self, src_term):
        src_terms = src_term.split(';')
        all_files = []
        for src_term in src_terms:
            src_files = glob2.glob( os.path.expanduser(src_term) )
            files = [filename for filename in src_files if os.path.isfile(filename)]
            all_files.extend(files)
        print all_files
        return all_files


    @property
    def source_files(self):
        src_expr = self.data_dict['source_files']
        return PDTProfile._get_file_list(src_expr)
    @property
    def build_files(self):
        src_expr = self.data_dict.get('build_files', '')
        return PDTProfile._get_file_list(src_expr)

        #src_terms = self.data_dict['source_files'].split(';')
        #all_files = []
        #for src_term in src_terms:
        #    src_files = glob2.glob( os.path.expanduser(src_term) )
        #    files = [filename for filename in src_files if os.path.isfile(filename)]
        #    all_files.extend(files)
        #print all_files
        #return all_files




class PDTProfileMgr(object):

    _pdtrcdir = PDTFileLocations.get_rc_dir()
    _pdtrc = PDTFileLocations.get_rc_file()

    profiles = {}
    profile_groups = {}
    default_targets = []

    @classmethod
    def _resolve_target_list_item(cls, item):
        if item in cls.profiles:
            return [cls.profiles[item]]
        elif item in cls.profile_groups:
            return cls.expand_target_list_to_profiles(cls.profile_groups[item])
        else:
            print 'Unable to resolve target: %s' % item
            assert False

    @classmethod
    def expand_target_list_to_profiles(cls, target_list):
        targets = []

        # Allow either a string or a list:
        if isinstance(target_list, basestring):
            target_list = [target_list]

        for target_str in target_list:
            targets.extend(cls._resolve_target_list_item(target_str))
        return sorted(list(set(targets)))

    @classmethod
    def load_rc_file(cls):
        pdtrcdata = configobj.ConfigObj(infile=cls._pdtrc)


        if not pdtrcdata:
            err =  "Nothing defined in %s\n" % (PDTFileLocations.get_rc_file(),)
            err += "Try adding:\n"
            raise RuntimeError(err)

        # Default profile names:
        if 'default_targets' in pdtrcdata:
            cls.default_targets = pdtrcdata['default_targets']

        if 'profile_groups' in pdtrcdata:
            cls.profile_groups = pdtrcdata['profile_groups']

        # Load the profiles:
        if not 'profiles' in pdtrcdata:
            raise RuntimeError('No-profiles defined in %s' % cls._pdtrc)
        for (profile_name, profile_data) in pdtrcdata['profiles'
                ].iteritems():
            cls.profiles[profile_name] = PDTProfile(profile_name,
                    profile_data)

    @classmethod
    def _init(cls):
        pdt.util.ensure_dir_exists(cls._pdtrcdir)

        # Load from .rc file:
        cls.load_rc_file()


PDTProfileMgr._init()
