
import subprocess
import types
import sys
import inspect
import re
import ast
import _ast
import morphforge.stdimports
import morphforgecontrib.stdimports
print "\n".join(['','',''])


prefix = 'morphforge'
modules = [ (k,v) for (k,v) in sys.modules.iteritems() if k.startswith(prefix) and isinstance(v,types.ModuleType) ]









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
    res = res.replace("__","_")
    return res












op_data = []

global_renames = []
local_renames = []

def is_special(txt):
    if txt in ["Cell", "CurrentClamp", "GapJunction", "MembraneMechanism" ,"PostSynapticMechanism" ,"PreSynapticMechanism" ,"Simulation" ,"SimulationSettings" ,"Synapse",'VoltageClamp','V', 't_TAG']:
        return True

    return txt.startswith("__")

func_regex = re.compile( r"""^_?[a-z][a-z0-9]*(_[a-z0-9]+)*_?$""" )
def valid_func_name(name):
    if is_special(name):
        return True

    return func_regex.match(name) is not None

func_arg_regex = func_regex
def valid_func_arg_name(name):
    if is_special(name):
        return True
    return func_arg_regex.match(name) is not None

varname_regex = func_regex
def valid_var_name(name):
    return varname_regex.match(name) is not None



import itertools

def visit_assgn_lhs(n):
    if isinstance(n, _ast.Assign):
        return itertools.chain( *[ visit_assgn_lhs(t) for t in n.targets])
    if isinstance(n, _ast.Tuple):
        print n._fields
        return itertools.chain( *[visit_assgn_lhs(t) for t in n.elts] )
    if isinstance(n, _ast.Name):
        return [n.id]
    if isinstance(n, _ast.Attribute):
        return []
    if isinstance(n,_ast.Subscript):
        return []
    print n
    assert False

def get_assigned_variable_names( src_code):
    print src_code
    var_names = []
    ast_tree = compile(src_code, filename="<string>", mode='exec', flags=ast.PyCF_ONLY_AST)
    for n in ast.walk(ast_tree):
        if isinstance(n, _ast.Assign):
            var_names.extend( visit_assgn_lhs(n))
    return set(var_names)

def inspect_function( func_name, func, is_method=False):
    print "ValidFuncName",valid_func_name(func_name)
    op_data.append( [ valid_func_name(func_name), func_name, 'Function-name;'] )
    (args, varargs, keywords, defaults) = inspect.getargspec(func)
    src = inspect.getsource(func)
    var_names = get_assigned_variable_names(("""class dummy(object):\n"""if is_method else "")+ src)
    for a in args:
        if not isinstance(a, basestring):
            continue
        print '    Arg:',a, valid_func_arg_name(a)
        pass
        op_data.append( [ valid_func_arg_name(a), func_name, 'Argument: %s' %a] )

        if not valid_func_arg_name(a):
            global_renames.append( a )
    for varname in var_names:
        op_data.append( [ valid_var_name(varname), func_name, 'Variable: %s' %varname] )
        if not valid_var_name(varname):
            lns, line_start = inspect.getsourcelines(func)
            line_end = line_start + len(lns)
            #print line_start, line_end
            #assert False
            local_renames.append( (varname,inspect.getsourcefile(func),line_start,line_end) )

    if not valid_func_name(func_name):
        #if not func_name.startswith("__"):
            global_renames.append( func_name )




visited = set([])
def inspect_obj(module_obj,objname,obj):
    print 'Name',objname
    if obj in visited:
        return
    visited.add(obj)
    for name, sub_obj in sorted( inspect.getmembers(obj) ):
        if name == '__builtins__':
            continue
        if inspect.getmodule (sub_obj) != module_obj:
            continue
        if inspect.isclass(sub_obj):
            print 'Classes:'
            inspect_obj(module_obj,name, sub_obj)
        if inspect.ismethod(sub_obj):
            inspect_function(name,sub_obj, is_method=True)
        elif inspect.isfunction(sub_obj):
            inspect_function(name,sub_obj)
        else:
            pass

for modulesname,moduleobj in modules:
    print 'MODULE:', modulesname
    inspect_obj(moduleobj, objname=modulesname, obj=moduleobj)



for d in op_data:
    if d[0] == True:
        continue

    # Custom skipping
    if d[1] == 't_TAG':
        continue
    print d



#sys.exit(0)




print '\n'.join(['','',''])

#global_renames_names = set( [ f[0] for f in global_renames])
for fnc in sorted(set(global_renames) ):
    fnc_new = suggest_new_name(fnc)
    ocs =  find_occurences(r"\\<%s\\>"%fnc)
    ocs_new =  find_occurences(r"\\<%s\\>"%fnc_new)
    #print [o.replace(search_loc,"") for o in ocs]


    if len(ocs_new) != 0:
        print 'WOULD OVERWRITE:%s (Originally:%s)'%(fnc_new,fnc)
        pass
    else:
        #print 'Simple Sub: %s->%s'%(fnc,fnc_new)
        print r"""find . -name "*.py"  -exec perl -p -i -e 's/\b%s\b/%s/g' {} \; """%(fnc,fnc_new)

print len(global_renames)


def read_file(filename):
    with open(filename) as f:
        t = f.read()
    return t

def string_in_text(mystr, txt):
    #print mystr, type(mystr)
    #print mystr, type(mystr)
    if re.compile(mystr).search(txt):
        return True
    return False


OK_changes = []
dangerous_changes = []
i=0
#global_renames_names = set( [ f[0] for f in global_renames])
for j,(fnc, filename, lstart,lend) in enumerate(sorted(local_renames )) :
    if len(fnc) == 1:
        continue

    #i=i-1
    print fnc, type(fnc)
    fnc_new = suggest_new_name(fnc)
    print " %s --> %s" %(fnc, fnc_new)
    print ' -- filename: ', filename

    orig = read_file(filename).split("\n")
    pre, content, post = orig[:lstart-1], orig[lstart-1:lend-1], orig[lend-1:]
    #print "PRE:",pre
    #print "CONTENT:",content
    #print "POST:",post

    if string_in_text(fnc, "\n".join(pre)):
        print "Varname used in preceeding text! Not safe since we don't know scope"
        dangerous_changes.append( (filename, fnc, fnc_new,lstart, lend))
        continue
    if string_in_text(fnc_new, "\n".join(pre)):
        print "NEW Varname used in preceeding text! Not safe since we don't know scope"
        dangerous_changes.append( (filename, fnc, fnc_new,lstart, lend))
        continue
    
    if string_in_text(fnc, "\n".join(post)):
        print "Varname used in post text! Not safe since we don't know scope"
        dangerous_changes.append( (filename, fnc, fnc_new,lstart, lend))
        continue
    if string_in_text(fnc_new, "\n".join(post)):
        print "NEW Varname used in post text! Not safe since we don't know scope"
        dangerous_changes.append( (filename, fnc, fnc_new,lstart, lend))
        continue

    print "OK TO CHANGE!"
    OK_changes.append( (filename, fnc, fnc_new))

    #ocs =  find_occurences(r"\\<%s\\>"%fnc)
    #ocs_new =  find_occurences(r"\\<%s\\>"%fnc_new)
    ##print [o.replace(search_loc,"") for o in ocs]



    #tmp_file = '/tmp/file1.txt'
    #with open(tmp_file,'w') as f:
    #    f.write(new_contents)

    #import os
    #os.system("meld %s %s"% (filename, tmp_file) )

    #i=i+1

    #if i>20:

    #    break

    #if len(ocs_new) != 0:
    #    print 'WOULD OVERWRITE:%s (Originally:%s)'%(fnc_new,fnc)
    #    pass
    #else:
    #    #print 'Simple Sub: %s->%s'%(fnc,fnc_new)
    #    print r"""find . -name "*.py"  -exec perl -p -i -e 's/\b%s\b/%s/g' {} \; """%(fnc,fnc_new)

print len(local_renames)


# Deal with the simple changes:
for filename,fnc,fnc_new in OK_changes:
    assert False
    print filename, fnc, fnc_new
    new_section = re.sub(r'\b%s\b' %fnc, fnc_new, "\n".join(content))
    new_contents = "\n".join(pre) + "\n" + new_section + "\n" +"\n".join(post)
    with open(filename,'w') as f:
        f.write(new_contents)

# Deal with the simple changes:
for filename,fnc,fnc_new, lstart,lend in dangerous_changes:
    #assert False
    print filename, fnc, fnc_new
    orig = read_file(filename).split("\n")
    pre, content, post = orig[:lstart-1], orig[lstart-1:lend-1], orig[lend-1:]
    new_section = re.sub(r'\b%s\b'%fnc, fnc_new, "\n".join(content))
    assert new_section != "\n".join(content)
    new_contents = "\n".join(pre) + "\n" + new_section + "\n" +"\n".join(post)

    tmp_file = '/tmp/file1.txt'
    with open(tmp_file,'w') as f:
        f.write(new_contents)

    import os
    os.system("meld %s %s"% (filename, tmp_file) )



print "All:", len(local_renames)
print "Simple:", len(OK_changes)



# Command to trim whitespace:
#find . -not -path '.git' -iname '*.py' -exec  sed -i  -E 's/[[:space:]]*$//' '{}' \;
