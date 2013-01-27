#!/usr/bin/python
# -*- coding: utf-8 -*-

import tempfile
import shutil


class TmpDir(object):

    def __enter__(self):
        self.working_dir = tempfile.mkdtemp()
        return self.working_dir

    def __exit__(self, exc_type, exc_val, exc_tb):
        shutil.rmtree(self.working_dir)


