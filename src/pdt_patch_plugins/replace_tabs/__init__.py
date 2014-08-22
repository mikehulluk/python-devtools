#!/usr/bin/python
# -*- coding: utf-8 -*-

import pdt
import pdt.patch_manager
import pdt.plugins.pdt_plugin_base


class Plugin(pdt.plugins.pdt_plugin_base.PatchPlugin):

    def plugin_name(self):
        return 'tab-replace'

    def do_text(self, text, args, filename=None):
        if filename:
            print 'Checking file:', filename
        return text.replace('\t','    ')


    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser(self.plugin_name(), help='* PythonTidy', parents=[parent_parser])
        parser.set_defaults(func=action_wrapper(self._do_plugin))


