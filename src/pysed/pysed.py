#! /usr/bin/python

import glob
import yapsy.PluginManager
import difflib
import subprocess
import tempfile
import shutil
import filecmp
import os
from configobj import ConfigObj



def filter_expect_single(seq, func):
    r = [ o for o in seq if func(o)]
    assert len(r)==1
    return r[0]

def isiterable(obj):
    try:
        for i in obj:
            return True
    except TypeError: 
        return False

def normalise_to_list(objs):
    if objs is None:
        return []
    if isinstance(objs, basestring):
        return [objs]
    if isiterable(objs):
        return objs
    return [objs]
    

class TmpDir(object):
    def __enter__(self,):
        self.working_dir = tempfile.mkdtemp()
        return self.working_dir

    def __exit__(self,exc_type, exc_val, exc_tb):
        shutil.rmtree(self.working_dir)

class PySedError(Exception):
    pass

class PySed(object):
    _default_rc_filename = '~/.python-devtools/.pysedrc'


    # The constructor reads the configuration:
    def __init__(self, rcfilename=None): 
        if rcfilename is None:
            rcfilename = PySed._default_rc_filename

        rcfilename = os.path.expanduser(rcfilename)

        if not os.path.exists(rcfilename):
            raise PySedError("Can't open the specified rcfile: %s"% rcfilename)
        self.config = ConfigObj(infile=rcfilename)
    
        # Setup the plugin architecture:
        self.simplePluginManager = yapsy.PluginManager.PluginManager()
        self.simplePluginManager.setPluginPlaces(["/home/michael/.python-devtools/plugins/pysed/",'/home/michael/hw/python-devtools/plugins/pysed/'])
        self.simplePluginManager.collectPlugins()


    def pysed_build_patches(self, src_filename, plugin_names=None, plugin_groupnames=None):
        assert False

    def apply_interactive(self, src_filenames, plugin_names=None, plugin_groupnames=None):
        pass


    #pysedobj.apply_interactive(lyx_files, plugin_groups='LyxStd')




def pysedfile( filename, plugin_names, plugin_mgr ):

    # Read the original text:
    with open(filename) as f:
        src_text = f.read()


    patches = []

    # Filter the relevant plugins:
    for (plugin_name, plugin_kwargs) in plugin_names:
        plug_info = filter_expect_single(plugin_mgr.getAllPlugins(), lambda o: o.name==plugin_name)

        # Get the new text
        mod_text = plug_info.plugin_object.action_text(src_text, **plugin_kwargs)

        # Create the diffs:
        diffs = difflib.unified_diff(src_text.splitlines(True), mod_text.splitlines(True) )
        patch = ''.join(diffs)
        patches.append(patch)

    return patches










def pysed( filenames, plugin_names, require_clean=False, do_apply=True ):
    # Build the manager
    simplePluginManager = yapsy.PluginManager.PluginManager()
    simplePluginManager.setPluginPlaces(["/home/michael/.python-devtools/plugins/pysed/",'/home/michael/hw/python-devtools/plugins/pysed/'])
    simplePluginManager.collectPlugins()


    for filename in filenames:
        print 'pysedding:', filename

        patches = pysedfile( filename, plugin_names, simplePluginManager)

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
                with open(filename) as fin:
                    fout.write(fin.read())

            # Apply the patches to the new file:
            for patch_file in patch_files:
                print '  Applying patch', patch_file
                subprocess.check_call(['patch','--quiet', '-u', new_filename, '-i', patch_file])

            # Merge the new changes:
            if filecmp.cmp(filename, new_filename, shallow=False) == True:
                print 'no changes to:', filename
                continue

            subprocess.check_call(['meld', filename, new_filename])




if __name__=='__main__':

    pysedobj = PySed()
    lyx_files = sorted(glob.glob('/home/michael/hw/hw-writing/chapters/*.lyx') )
    #def pysed_build_patches(self, src_filename, plugin_names=None, plugin_groupnames=None):
    
    pysedobj.apply_interactive(lyx_files, plugin_groupnames='LyxStd')



    lyx_files = sorted(glob.glob('/home/michael/hw/hw-writing/chapters/*.lyx') )
    plugin_names = [ 
            ('LyxRef', {} ),
            ('LyxWhitespace', {} )   
            ]

    pysed(lyx_files, plugin_names)
    print 'Done'
