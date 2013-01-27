#! /usr/bin/python

import yapsy.PluginManager
import difflib
import subprocess
import tempfile
import shutil
import filecmp
import os
from configobj import ConfigObj
import itertools



# Utility methods:
def filter_expect_single(seq, func):
    r = [ o for o in seq if func(o)]
    assert len(r)==1
    return r[0]

def filter_expect_one_or_none(seq, func):
    r = [ o for o in seq if func(o)]
    if len(r)==0:
        return None
    assert len(r)==1
    return r[0]

def isiterable(obj):
    try:
        for i in obj:
            return True
    except TypeError:
        return False


# Ensures that a sequence is iterable,
# if we are passed a non-iterable, or a string,  return a list with
# that object, otherwise return the original object
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
        self.config = ConfigObj(infile=rcfilename, )
        self.config.list_values = False

        # Setup the plugin architecture:
        self.simplePluginManager = yapsy.PluginManager.PluginManager()
        self.simplePluginManager.setPluginPlaces(["/home/michael/.python-devtools/plugins/pysed/",'/home/michael/hw/python-devtools/plugins/pysed/'])
        self.simplePluginManager.collectPlugins()

    def get_plugingroup(self, name):
        conf_groups = self.config.get('groups',None)
        if not conf_groups or not name in conf_groups:
            return None
        apply_str = conf_groups[name]['apply_plugins']

        res = []
        for obj in apply_str:
            print obj, type(obj)
            objtoks = obj.split()
            if len(objtoks) == 1:
                res.append( [objtoks[0], {} ] )
            else:
                # TODO, someway of parsing options
                raise NotImplementedError('Support for kw args not yet sppurted')
        return res


    def get_plugin(self, name):
        plug_info = filter_expect_one_or_none(self.simplePluginManager.getAllPlugins(), lambda o: o.name==name)
        return plug_info


    def _resolve_plugin_name_to_obj(self, name, kwarg=None):
        if kwarg is None:
            kwarg = {}
        plug_obj = self.get_plugin(name=name)
        plug_grp = self.get_plugingroup(name=name)
        if plug_obj and plug_grp:
            raise PySedError('Found both a group and a plugin with the name: %s' % name)

        if plug_obj:
            return [ (plug_obj, kwarg)  ]

        if plug_grp:
            ret = []
            for child in plug_grp:
                # Resolve the child, depending on the parameters:
                if isinstance( child, basestring):
                    (childname, childkwargs) = child, kwarg
                if len(child)==1:
                    (childname, childkwargs) = child[0], kwarg
                elif len(child) == 2:
                    (childname, childkwargs) = child

                ret.extend( itertools.chain(*[self._resolve_plugin_name_to_obj(childname, childkwargs) ] ))
            return ret

        raise PySedError('No group or plugin found with the name: %s' % name)


    def apply_interactive(self, src_filenames, plugin_names=None):
        src_filenames = normalise_to_list(src_filenames)
        plugin_names = normalise_to_list(plugin_names)

        plugin_names_resolved = [ self._resolve_plugin_name_to_obj(name=name) for name in plugin_names]
        plugin_names_resolved = list(itertools.chain(*plugin_names_resolved))



        # Apply the changes:
        for src_filename in src_filenames:
            self._apply_interactive_to_file(src_filename=src_filename, plugins=plugin_names_resolved)


    def _apply_interactive_to_file(self, src_filename, plugins):

        # Make a set of patches:
        patches = self._apply_interactive_to_file_build_patches( src_filename=src_filename, plugins=plugins)

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



    def _apply_interactive_to_file_build_patches(self, src_filename, plugins):

        # Read the original text:
        with open(src_filename) as f:
            src_text = f.read()

        patches = []
        # Filter the relevant plugins:
        for (plugin_obj, plugin_kwargs) in plugins:

            # Get the new text
            mod_text = plugin_obj.plugin_object.action_text(src_text, **plugin_kwargs)

            # Create the diffs:
            diffs = difflib.unified_diff(src_text.splitlines(True), mod_text.splitlines(True) )
            patch = ''.join(diffs)
            patches.append(patch)

        return patches





def main():

    import argparse

    parser = argparse.ArgumentParser(description='Apply PySed to a bunch of files')
    parser.add_argument('src_files', metavar='src_files',  nargs='+',
                       help='the scr files')
    parser.add_argument('--plugin', dest='plugins', action='append',
                       help='the plugins to be run on the code')

    
    rgs = parser.parse_args()
    pysedobj = PySed()
    pysedobj.apply_interactive(rgs.src_files, plugin_names=rgs.plugins)


if __name__=='__main__':
    main()



