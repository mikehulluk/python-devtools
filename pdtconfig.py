

import os
import glob
from configobj import ConfigObj
from validate import Validator
import sys


# Dynamically add 'glob2' to the path:
local_path = os.path.dirname(__file__)
glob2_src = os.path.join(local_path, 'glob2/src/')
sys.path.append(glob2_src)
import glob2



def _ensure_dir_exists(path):
    dirname = os.path.dirname(path)
    if not os.path.exists(dirname):
        os.makedirs(dirname)
    return path




#def _double_star_glob(glob_expr):
#    pass


class PDTProfile(object):
    def __init__(self, name, data_dict):
        self.data_dict = data_dict

        self.data_dict['name'] = name

        if not 'working_dir' in self.data_dict:
            self.data_dict['working_dir'] = os.path.join(PDTProfileMgr._pdtrc, '.profile-%s/'%name)

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
        return os.path.join( self.working_dir, 'findandreplace.stash')

#    @property
#    def find_and_replace_filename_dangerous(self):
#        return os.path.join( self.working_dir, 'findandreplace-dangerous.stash')


    @property
    def unmerged_changes(self):
        pass

    @property
    def files(self):
        src_files = os.path.expanduser( self.data_dict['source_files']  )
        files = glob2.glob(src_files)
        files = [filename for filename in files if os.path.isfile(filename) ]
        return files





class PDTProfileMgr(object):

    _pdtrc = os.path.expanduser('~/.python-devtools/')
    _configspec = "/home/michael/hw_to_come/python-devtools/configspec.ini"



    profiles = None

    @classmethod
    def get_projects(cls,):
        pass

    @classmethod
    def _load_profile(cls, profile_filename):
        print 'Loading profile:', profile_filename
        profile_name = os.path.splitext(os.path.split(profile_filename)[-1])[0]
        profile_data = ConfigObj(infile=profile_filename, configspec=cls._configspec)
        cls.profiles[profile_name] = PDTProfile(profile_name, profile_data)



    @classmethod
    def _init(cls,):
        _ensure_dir_exists(cls._pdtrc)

        cls.profiles = {}
        profile_filenames = glob.glob(cls._pdtrc + '/*.profile')
        for profile_filename in profile_filenames:
            cls._load_profile(profile_filename)




PDTProfileMgr._init()
