#!/usr/bin/python
# -*- coding: utf-8 -*-

import datetime
import difflib
import subprocess
import filecmp
import sqlalchemy

from pdt.util import TmpDir
from pdt.patch_objects import Patch, PatchSet, session
from pdt.errors import PatchApplyError


class PatchManager(object):

    @classmethod
    def clear_empty_patchsets(cls):
        for ps in session.query(PatchSet).all():
            if len(ps.patches) == 0:
                session.delete(ps)
        session.commit()

    @classmethod
    def get_patches_for_filename(cls, filename):
        return [p for p in session.query(Patch).all()
                if p.target_filename == filename]


    @classmethod
    def create_patchset(cls, patching_data, name=None):
        """ patching_data is a dict  filename-> (old,new)"""

        if name == None:
            name = 'Unamed'

        print 'Building Patchset'

        patchset = PatchSet(name=name,
                            release_date=datetime.datetime.now())

        for (filename, (original, modified)) in patching_data.items():

            try:
                diffs = list(difflib.unified_diff(original.splitlines(True), modified.splitlines(True)))
                patch = ''.join(diffs)
                print ' - (%d) %s' % (len(diffs), filename)

                patch = Patch(target_filename=filename, patch_data=patch)
                patchset.patches.append(patch)
            
                session.add(patchset)
                session.commit()
            except sqlalchemy.exc.ProgrammingError:
                print("Error Adding patchset for: %s"%filename)
                session.rollback()


        session.add(patchset)
        session.commit()

        return patchset

    @classmethod
    def apply_patchs(cls, src_filename, patches):
        # Do everything in a temp-dir
        with TmpDir() as working_dir:

            # Write all the patches to files:
            patch_files = []
            for (i, patch) in enumerate(patches):
                patch_name = working_dir + '/patch-%03d' % i
                with open(patch_name, 'w') as f:
                    f.write(patch)
                patch_files.append(patch_name)

            # Copy the original file:
            new_filename = working_dir + '/working.txt'
            with open(new_filename, 'w') as fout:
                with open(src_filename) as fin:
                    fout.write(fin.read())

            # Apply the patches to the new file:
            for patch_file in patch_files:
                print '  Applying patch', patch_file
                try:
                    subprocess.check_call([
                        'patch',
                        '--quiet',
                        '-u',
                        new_filename,
                        '-i',
                        patch_file,
                        ])
                except subprocess.CalledProcessError:
                    dmp_file = '/tmp/pdt-patch-error-%s.patch'%src_filename.replace("/","__")
                    with open(dmp_file,'w') as wf:
                        with open(patch_file) as rf:
                            wf.write( rf.read() )

                    raise PatchApplyError()

            # Merge the new changes:
            if filecmp.cmp(src_filename, new_filename, shallow=False) == True:
                print 'no changes to:', src_filename
                return

            subprocess.check_call(['meld', src_filename, new_filename])


