
from patch_manager import session, Patch, PatchSet, PatchManager


class DoPatchApply(object):

    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('apply', help='Apply all outstanding patches')
        patch_apply.set_defaults(func=self.do_apply)

    def do_apply(self, args):
        print 'Applying Patches'

        # Find all the targetted files:
        target_files = list( set([p.target_filename for p in session.query(Patch).all()] ) )
        target_files.sort()

        for target_filename in target_files:
            self._do_apply_filename(target_filename)


    def _do_apply_filename(self,target_filename):
        print 'Applying to: %s' % target_filename
        
        patches = PatchManager.get_patches_for_filename(target_filename) 
        patch_datas = [ p.patch_data for p in patches]
        print ' - Patches Found: %d'%(len(patches))

        # Apply the patches:
        PatchManager.apply_patchs(target_filename, patch_datas)
    
        # Remove these patches from the database:
        for p in patches:
            session.delete(p)
        session.commit()
        PatchManager.clear_empty_patchsets()
        
        
        
        
        
        




class DoPatchDrop(object):


    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('drop', help='Drop all outstanding patches')
        patch_apply.set_defaults(func=self.do_drop)

    def do_drop(self, args):
        print 'Dropping all patches'
        for o in session.query(PatchSet).all():
            print ' - ', o.name
            session.delete(o)
        session.commit()
        print 'Done'



class DoPatchList(object):

    def do_list(self, args):
        print 'Outstanding Patches: (%d)' % session.query(PatchSet).count()
        for patchset in session.query(PatchSet).all():
            print patchset

    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('list', help='List all outstanding patches')
        patch_apply.set_defaults(func=self.do_list)

class DoPatchRefreshPatches(object):

    def do_refresh(self, args):
        print 'List Patches'

    def build_arg_parser(self, argparser):
        patch_apply = argparser.add_parser('refresh', help='Refresh all outstanding patches')
        patch_apply.set_defaults(func=self.do_list)

