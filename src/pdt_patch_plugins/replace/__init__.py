

import pdt
import pdt.patch_manager
import re



import pdt.plugins.pdt_plugin_base
class Plugin(pdt.plugins.pdt_plugin_base.PatchPlugin):

    def plugin_name(self):
        return 'replace'

    def do_text(self, text, args):
        print args.src_expr, type(args.src_expr)
        r = re.compile(args.src_expr, re.MULTILINE)
        return r.sub(args.target_expr, text)

    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser('replace', help='* Replace', parents=[parent_parser])
        parser.add_argument('src_expr')
        parser.add_argument('target_expr')
        parser.set_defaults(func=action_wrapper(self._do_plugin))


