#!/usr/bin/python
# -*- coding: utf-8 -*-

import tempfile
import shutil
import os

class TmpDir(object):
    def __enter__(self):
        self.working_dir = tempfile.mkdtemp()
        return self.working_dir

    def __exit__(self, exc_type, exc_val, exc_tb):
        shutil.rmtree(self.working_dir)


def ensure_dir_exists(path):
    dirname = os.path.dirname(path)
    if not os.path.exists(dirname):
        os.makedirs(dirname)
    return path
