#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import shutil
import difflib
import re

import itertools
from substfileply import parse_string


class RemapData(object):
    def __init__(self,):
        self.previous_remaps = {}
        self.not_remapped = ['ax','cPickle', 'np','i','j','pq','os','re']







class SubstData(object):

    def __init__(self, filename):
        self.substitutions = {}
        self.filename = filename

    def add_substitution(self, filename, line_range, src_regex, target_regex):
        if not filename in self.substitutions:
            self.substitutions[filename] = []
        self.substitutions[filename].append( (line_range, src_regex, target_regex) )

    def write_to_disk(self):
        filename = os.path.expanduser(self.filename)
        with open(filename,'w') as fobj:
            for filename, substs in self.substitutions.iteritems():
                fobj.write("\n>> '%s'"% filename.replace("'","\\'") )
                for (line_range, src,target)  in substs:
                    line_range_str = '[%d:%d]'%( line_range[0], line_range[1]) if line_range is not None else ''
                    fobj.write("\n  ** %s '%s' '%s' " %( line_range_str, src, target)) 

    def clear(self):
        self.substitutions = {}

    @property
    def nfiles(self):
        return len(self.substitutions)

    @property
    def nchanges(self):
        return len(list(itertools.chain(*self.substitutions.values())))

    def __str__(self):
        s = '<SubstData object - %d changes to %d files (Stored at: %s)>' % (self.filename, self.nchanges, self.nfiles )
        return s

    @property
    def filenames(self):
        return self.substitutions.keys()

    @classmethod
    def from_file(self, filename):
        filename = os.path.expanduser(filename)

        if not os.path.exists(filename):
            return SubstData(filename)

        with open(filename, 'r') as fobj:
            raw_data = parse_string(fobj.read())

        obj = SubstData(filename)
        for (filename, transforms) in raw_data:
            for (line_range, src_regex, target_regex) in transforms:
                obj.add_substitution(
                        filename=filename,
                        line_range=line_range,
                        src_regex=src_regex,
                        target_regex=target_regex)
        return obj


class SubstDataDangerousSubstException(RuntimeError):

    pass


class SubstDataOptions(object):

    def __init__(self, prevent_colisions=True):

        self.prevent_colisions = prevent_colisions


class SubstDataActioner(object):

    @classmethod
    def apply_all(cls, substdata, options):
        for filename in substdata.substitutions.keys()[:]:
            try:
                cls.apply_changes_to_file(substdata,filename, options=options)
                del substdata.substitutions[filename]
                substdata.write_to_disk()
            except SubstDataDangerousSubstException:
                print 'Dangerous subsitions missed in', filename
                pass

    @classmethod
    def apply_changes_to_file(cls, substdata, filename, options):
        assert filename in substdata.substitutions,'Not substituions for file'

        with open(filename) as fsrc:
            orig_txt = fsrc.readlines()
            working_txt = orig_txt[:]

        for subst in substdata.substitutions[filename]:
            working_txt = cls.applysinglesubstitiontotextlines(working_txt, subst, options=options)

        op = ''.join(working_txt)
        orig = ''.join(orig_txt)

        # Are there any changes??
        if op == orig:
            return

        new_filename = filename + '.new'
        with open(new_filename, 'w') as fnew:
            fnew.write(op)
        os.system("meld '%s' '%s'" % (filename, new_filename))

    @classmethod
    def applysinglesubstitiontotextlines(cls, blk, subst, options):
        (line_range, src_regex, target_regex) = subst

        if line_range is None:
            line_range = (0, len(blk))

        pre = blk[:line_range[0]]
        post = blk[line_range[1]:]
        middle = blk[line_range[0]:line_range[1]]

        # Make the substitions:
        middle_new = [re.sub(src_regex, target_regex, line) for line in middle]

        if options.prevent_colisions and middle != middle_new:
            for line in middle:
                if re.search(r'''\b%s\b''' % target_regex, line):
                    raise SubstDataDangerousSubstException('You would overwrite an existing variable name! (%s)' % (target_regex) )

        merge = pre + middle_new + post

        return merge


