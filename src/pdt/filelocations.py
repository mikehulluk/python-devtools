#!/usr/bin/python
# -*- coding: utf-8 -*-

import os


class PDTFileLocations(object):


    @classmethod
    def get_rc_dir(cls):
        return os.path.expanduser('~/.python-devtools/')

    @classmethod
    def get_rc_file(cls):
        return os.path.expanduser('~/.python-devtools/.pdtrc')


    _pdtrcdir = os.path.expanduser('~/.python-devtools/')
    _pdtrc = os.path.expanduser('~/.python-devtools/.pdtrc')



    @classmethod
    def get_patch_sqlalchemy_url(cls):
        sqlfilename = os.path.join(cls.get_rc_dir(),
                                   'patches.db')
        return 'sqlite:///%s' % sqlfilename

    @classmethod
    def get_plugin_locations(cls):
        return cls._get_site_plugin_locations() \
            + cls._get_local_plugin_locations() \
            + cls._get_custom_plugin_locations()

    @classmethod
    def _get_site_plugin_locations(cls):
        local_path = os.path.dirname(__file__)
        plugin_dir = os.path.normpath(os.path.join(local_path,
                '../pdt_patch_plugins/'))
        return [plugin_dir]

    @classmethod
    def _get_local_plugin_locations(cls):
        plugin_dir = os.path.join(cls.get_rc_dir(),
                                  'pdt_patch_plugins/')
        return [plugin_dir]

    @classmethod
    def _get_custom_plugin_locations(cls):
        # TODO, read from config file
        return []


