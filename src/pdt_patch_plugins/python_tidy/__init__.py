 


#from pdt.pdt_plugin_base import SimpleTextChangePlugin



class PluginOne(SimpleTextChangePlugin):
    def print_name(self):
        print "This is plugin: Python-tidy"



    def _do_plugin(*args, **kwargs):
        
        
        print 'Doing plugin'
        
    def build_arg_parser(self, argparser):
        parser = argparser.add_parser('python-tidy', help='* Python-Tidy')
        parser.set_defaults(func=self._do_plugin)


    #def action_file(self, filename):
        
    
