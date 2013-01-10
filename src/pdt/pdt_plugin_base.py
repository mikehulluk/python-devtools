
from yapsy.IPlugin import IPlugin



class PatchPlugin(IPlugin):
    
    def build_arg_parser(self, argparser):
        raise NotImplementedError()


