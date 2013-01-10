import os



class PatchData(object):
    def __init__(self, patch_id, filename, date, comment):
        self.patch_id = patch_id
        self.filename = filename
        self.date = date
        self.comment = comment



class PatchIndexError(Exception):
    pass


class PatchManager(object):

    # Singleton/Borg class:
    _instance = None
    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(PatchManager, cls).__new__(
                                cls, *args, **kwargs)
        return cls._instance



    scratch_dir = '/home/michael/.python-devtools/active-patches/'
    patch_index_file = '/home/michael/.python-devtools/patches-list.txt'


    def __init__(self):
        self.patchs = []

        # Load existing patches:
        if os.path.exists(self.patch_index_file):
            self.patchs = self._load_patchlist(self.patch_index_file)


    @classmethod
    def _load_patchlist(self, index_file):

        with open(index_file) as f:
            lines = [l.strip() for l in f.readlines()]
        lines = [l for l in lines if l and not l.startswith('#')]
        results = [ PatchData(*l.split(',',3 )) for l in lines ]
        return results

    @classmethod
    def _save_patchlist(self):
        print 'Saving Current Patch-Index'
        pass



    def _add_patchfile_to_index(self, patch_data):
        assert not patch_data in self.patches
        self.patches.append(patch_data)

        self._save_patchlist()




