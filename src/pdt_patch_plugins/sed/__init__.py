 

from yapsy.IPlugin import IPlugin

class PluginOne(IPlugin):
    def print_name(self):
        print "This is plugin Sed"




    #def _do_plugin():
    def _do_plugin(*args, **kwargs):
        assert False



    def build_arg_parser(self, argparser):
        parser = argparser.add_parser('sed', help='* Sed')
        parser.set_defaults(func=self._do_plugin)
        
        
