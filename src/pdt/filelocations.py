#!/usr/bin/python
# -*- coding: utf-8 -*-

import os


class PDTFileLocations(object):

    _pdtrcdir = os.path.expanduser('~/.pdt/')

    @classmethod
    def get_rc_dir(cls):
        if not os.path.exists(PDTFileLocations._pdtrcdir):
            os.makedirs(PDTFileLocations._pdtrcdir)
        return PDTFileLocations._pdtrcdir

    @classmethod
    def get_rc_file(cls):
        return os.path.join( PDTFileLocations.get_rc_dir(), 'pdtrc')


    @classmethod
    def get_patch_sqlalchemy_url(cls):
        sqlfilename = os.path.join(cls.get_rc_dir(), '.patches.sql')
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
        plugin_dir = os.path.join(cls.get_rc_dir(), 'pdt_patch_plugins/')
        return [plugin_dir]

    @classmethod
    def _get_custom_plugin_locations(cls):
        # TODO, read from config file
        return []


