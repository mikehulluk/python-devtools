#! /usr/bin/python

import sys
import os
import shutil



def tidyfile(filename):
    print 'Tidying File:', filename
    
    # Run PythonTidy
    new_filename = filename + '.new'
    cmd = 'PythonTidy %s %s'%(filename, new_filename)
    os.system(cmd)

    with open(filename) as f1:
        with open(new_filename) as f2:
            if f1.read() == f2.read():
                print 'Identical Files'
                return
    
    # Run meld:
    cmd = 'meld %s %s'%(filename, new_filename)
    os.system(cmd)
    os.unlink(new_filename)


def main():
    for i,filename in enumerate(sys.argv[1:]):
        print 'File: %d/%d' % (i, len(sys.argv))
        tidyfile(filename)

if __name__ == '__main__':
    main()



