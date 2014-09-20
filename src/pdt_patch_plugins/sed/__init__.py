

import pdt
import pdt.patch_manager
import subprocess
import pdt.plugins.pdt_plugin_base

import shlex

class Plugin(pdt.plugins.pdt_plugin_base.PatchPlugin):

    def plugin_name(self):
        return 'sed'

    def do_text(self, text, sedexpr):
        sedexpr = sedexpr.sedexpr
        print 'sedexpr:', sedexpr
        print type(sedexpr)

        p = subprocess.Popen(['sed'] +  shlex.split(sedexpr) , stdout=subprocess.PIPE, stdin=subprocess.PIPE )
        p.stdin.write(text)
        out = p.communicate()[0]
        return out

    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser(self.plugin_name(), help='* Sed')
        parser.add_argument('sedexpr')
        parser.set_defaults(func=action_wrapper(self._do_plugin))
