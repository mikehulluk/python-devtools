
class DoPatchApply(object):
    
    def do_apply(self, args):
        print 'Applying Patches'
    
    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('apply', help='Apply all outstanding patches')
        patch_apply.set_defaults(func=self.do_apply)
        

class DoPatchDrop(object):
    
    def do_drop(self, args):
        print 'Dropping Patches'
    
    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('drop', help='Drop all outstanding patches')
        patch_apply.set_defaults(func=self.do_drop)
        
class DoPatchList(object):
    
    def do_list(self, args):
        print 'List Patches'
    
    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('list', help='List all outstanding patches')
        patch_apply.set_defaults(func=self.do_list)        

class DoPatchRefreshPatches(object):
    
    def do_refresh(self, args):
        print 'List Patches'
    
    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('refresh', help='Refresh all outstanding patches')
        patch_apply.set_defaults(func=self.do_list)        

