#!/usr/bin/python 
import re
import subprocess
import os

search_loc = '/home/mhtest/hw/'
def find_occurences(text):
    cmd = 'find %s -name "*.py" -exec grep -l "%s" {} \; ' % (search_loc, text)
    op = subprocess.check_output(cmd, shell=True)
    return [ l for l in op.split("\n") if l]

def meld_replace_in_file(src_file, orig, target):
    tmp_file = src_file + '.new'
    with open(src_file, 'r') as f_in:
        old_text = f_in.read() 
        new_text  = re.sub(r'''\b%s\b''' % orig, target, old_text)
        if old_text == new_text:
            return
        with open(tmp_file, 'w') as f_out:
            f_out.write(new_text)

    
    os.system("meld '%s' '%s'"%(src_file, tmp_file))



import sys

src = sys.argv[1]
tgt = sys.argv[2]

files = find_occurences('''%s'''%src)
#files = find_occurences('''\<%s\>'''%src)
print files
for f in files:
    meld_replace_in_file(f, src, tgt)




#def replace_var(var_name):
#    files = find_occurences('\<'+var_name+'\>')
#    new_var_name = suggest_new_name(var_name)
#    if len(files) == 1:
#        pass
#        #print 'easy replace', var_name, '->', new_var_name, files[0]
#        #meld_replace_in_file(files[0], var_name, new_var_name)
#    else:
#        print 'Complex Replace:', '%02d' %len(files), var_name #, '->', new_var_name, files[0]
#
#
#
#with open('pylint_out.html', 'r') as r:
#    txt = r.read()
#    
#matches = re.findall(r'''Invalid name "([a-zA-Z0-9_]*)''', txt)
#matches = sorted( list(matches))
#
#for m in matches:
#    if m.startswith('t_'):
#        continue
#    if matches.count(m) == 1:
#        replace_var(m)
#        #print 'Single:',m
#    #else:
#    #    print 'Multi:',m
#
##Invalid name "r"
