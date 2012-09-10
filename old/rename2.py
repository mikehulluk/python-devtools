import re
import subprocess
import os

search_loc = '/home/mhtest/hw/'
def find_occurences(text):
    cmd = 'find %s -name "*.py" -exec grep -l "%s" {} \; ' % (search_loc, text)
    #subprocess.call(cmd
    op = subprocess.check_output(cmd, shell=True)
    return [ l for l in op.split("\n") if l]

first_cap_re = re.compile('(.)([A-Z][a-z]+)')
all_cap_re = re.compile('([a-z0-9])([A-Z])')
def _convert_camelcase_to_underscore(name):
    s1 = first_cap_re.sub(r'\1_\2', name)
    return all_cap_re.sub(r'\1_\2', s1).lower()

def suggest_new_name(old_name):
    res =  _convert_camelcase_to_underscore(old_name)
    res = res.replace("__", "_")
    return res



def meld_replace_in_file(src_file, orig, target):
    tmp_file = src_file + '.new'
    with open(src_file, 'r') as f_in:
        new_text  = re.sub(r'''\b%s\b''' % orig, target, f_in.read() )
        with open(tmp_file, 'w') as f_out:
            f_out.write(new_text)
    os.system('meld %s %s' % (src_file, tmp_file))




def replace_var(var_name):
    files = find_occurences('\<'+var_name+'\>')
    new_var_name = suggest_new_name(var_name)
    if len(files) == 1:
        pass
        #print 'easy replace', var_name, '->', new_var_name, files[0]
        #meld_replace_in_file(files[0], var_name, new_var_name)
    else:
        print 'Complex Replace:', '%02d' % len(files), var_name #, '->', new_var_name, files[0]



with open('pylint_out.html', 'r') as r:
    txt = r.read()
    
matches = re.findall(r'''Invalid name "([a-zA-Z0-9_]*)''', txt)
matches = sorted( list(matches))

for m in matches:
    if m.startswith('t_'):
        continue
    if matches.count(m) == 1:
        replace_var(m)
        #print 'Single:', m
    #else:
    #    print 'Multi:', m

#Invalid name "r"
