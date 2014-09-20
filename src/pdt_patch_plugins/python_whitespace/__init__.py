



import pdt
import pdt.patch_manager
import re

#r1 = re.compile(r'''^(.*[^\s].*)[ ][ ]+''',re.MULTILINE)
#r2 = re.compile(r'''\(\s+''',re.MULTILINE)
#r3 = re.compile(r'''\[\s+''',re.MULTILINE)
r4 = re.compile(r'''^([^\n]\s*\S)\s+\)''',re.MULTILINE)
#r5 = re.compile(r'''(?:^.*?[^\s].*?)\s+\]''',re.MULTILINE)

#r6 = re.compile(r''',\)''',re.MULTILINE)



import pdt.plugins.pdt_plugin_base
class Plugin(pdt.plugins.pdt_plugin_base.PatchPlugin):

    def plugin_name(self):
        return 'python-whitespace'


    def do_text(self, text, args):
        #text = r1.sub(r'''\1 ''',text)

        #text = r2.sub(r'''(''',text)
        #text = r3.sub(r'''[''',text)
        text = r4.sub(r'''\1)''',text)
        #text = r5.sub(r''']''',text)

        #text = r6.sub(r''')''',text)
        return text

    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser(self.plugin_name(), help='* Whitespace', parents=[parent_parser])
        parser.set_defaults(func=action_wrapper(self._do_plugin))
        #parser.add_argument('src_expr')
        #parser.add_argument('target_expr')


