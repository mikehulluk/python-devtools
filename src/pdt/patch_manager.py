import os


from sqlalchemy import create_engine, ForeignKey
from sqlalchemy import Column, Date, Integer, String
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship, backref


import datetime
import difflib
import shutil
import tempfile
import subprocess
import filecmp



sqlfilename='patches.db'
from sqlalchemy.orm import sessionmaker


#engine = create_engine('sqlite:///%s'%sqlfilename, echo=True)
engine = create_engine('sqlite:///%s'%sqlfilename, echo=False)
Base = declarative_base()


class Patch(Base):
    """"""
    __tablename__ = "patches"
 
    id = Column(Integer, primary_key=True)
    target_filename = Column(String)  
    patch_data = Column(String)
    patch_set = Column(Integer, ForeignKey("patchsets.id")) #, backref='patches')
 
    #----------------------------------------------------------------------
    def __init__(self, target_filename, patch_data):
        self.target_filename = target_filename
        self.patch_data = patch_data


class PatchSet(Base):
    __tablename__ = "patchsets"
    id = Column(Integer, primary_key=True)
    name = Column(String)
    #date_added = Column(String)
    release_date = Column(Date)
    patches = relationship("Patch", cascade='all,delete,delete-orphan')

    def __str__(self):
        return  'Patchset: %s (%d patches)' % (self.name, len(self.patches) )







# create tables
Base.metadata.create_all(engine)

Session = sessionmaker(bind=engine)
session = Session()



class TmpDir(object):
    def __enter__(self,):
        self.working_dir = tempfile.mkdtemp()
        return self.working_dir

    def __exit__(self,exc_type, exc_val, exc_tb):
        shutil.rmtree(self.working_dir)




class PatchManager(object):
    
    
    @classmethod
    def clear_empty_patchsets(cls):
        for ps in session.query(PatchSet).all():
            if len(ps.patches)==0:
                session.delete(ps)
        session.commit()
    
    @classmethod
    def get_patches_for_filename(cls, filename):
        return [p for p in session.query(Patch).all() if p.target_filename==filename]
    
    @classmethod
    def create_dummy_patchset(cls,):
        ps1 = PatchSet(
                name='Test1', 
                release_date = datetime.datetime.now()
                )

        p1 = Patch(target_filename='Testfile1',
                   patch_data='PATCH_DATA1')
        p2 = Patch(target_filename='Testfile2',
                   patch_data='PATCH_DATA2')
        ps1.patches.extend([p1,p2])

        session.add(ps1)
        session.commit()
        

    @classmethod
    def create_patchset(cls, patching_data, name=None):
        """ patching_data is a dict  filename-> (old,new)"""
        
        if name == None:
            name = 'Unamed'
        
        
        print 'Building Patchset'
        
        patchset = PatchSet(
                name=name, 
                release_date = datetime.datetime.now()
                )
        
        
        for filename, (original,modified) in patching_data.items():
            
            diffs = list( difflib.unified_diff(original.splitlines(True), modified.splitlines(True) ) )
            patch = ''.join(diffs)
            print ' - (%d) %s' %(len(diffs), filename)
            
            patch = Patch(target_filename=filename, patch_data=patch )
            patchset.patches.append(patch)
                
        session.add(patchset)
        session.commit()

        return patchset


    @classmethod
    def apply_patchs(cls, src_filename, patches):
        # Do everything in a temp-dir
        with TmpDir() as working_dir:

            # Write all the patches to files:
            patch_files = []
            for (i,patch) in enumerate(patches):
                patch_name = working_dir + '/patch-%03d'%i
                with open(patch_name,'w') as f:
                    f.write(patch)
                patch_files.append(patch_name)

            # Copy the original file:
            new_filename = working_dir + '/working.txt'
            with open(new_filename,'w') as fout:
                with open(src_filename) as fin:
                    fout.write(fin.read())

            # Apply the patches to the new file:
            for patch_file in patch_files:
                print '  Applying patch', patch_file
                subprocess.check_call(['patch','--quiet', '-u', new_filename, '-i', patch_file])
            
            # Merge the new changes:
            if filecmp.cmp(src_filename, new_filename, shallow=False) == True:
                print 'no changes to:', src_filename
                return

            subprocess.check_call(['meld', src_filename, new_filename])




