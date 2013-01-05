import ast
import sys

from coreclasses import SubstData, SubstDataActioner, SubstDataOptions


import morphforge.stdimports
mf_modules = [ m for m in sys.modules if m.startswith('morphforge') ]
#print mf_modules


class MyNodeVisitor(ast.NodeVisitor):

    def __init__(self, filename, subst_data):
        super(MyNodeVisitor, self).__init__()
        self.filename = filename
        self.subst_data = subst_data

    def visit_ImportFrom(self, node):

        module_name = node.module
        lineno = node.lineno
        # Built-ins, or fully qualified imports:
        if module_name in sys.modules:
            return 
        
        
        f_module = '.'.join( self.filename.replace('/home/michael/hw_to_come/morphforge/src/','').split('/')[:-1] )
        print filename
        print f_module
        
        res2 = f_module + "." + module_name
        if res2 in sys.modules:
            self.subst_data.add_substitution(  filename=self.filename, line_range=[lineno-1,lineno+1], src_regex=r'\b%s\b'%module_name, target_regex=res2 )
            return
                
        possible_resolutions = [m for m in mf_modules if m.endswith(module_name) ]
        if len(possible_resolutions) == 1:
            res = possible_resolutions[0]
            print 'Simple-Sub:', "%s -> %s" %(module_name, res )
            
            self.subst_data.add_substitution(  filename=self.filename, line_range=[lineno-1,lineno+1], src_regex=r'\b%s\b'%module_name, target_regex=res )

            
        else:
            print 'Ambiguous:', filename, module_name
            print possible_resolutions
        
        
        
        
import pdtconfig        
import glob2

profile = pdtconfig.PDTProfileMgr.profiles['morphforge']

src_files = glob2.glob("/home/michael/hw_to_come/morphforge/src/morphforge/**/*.py")
for filename in src_files:
       
    with open(filename) as f:
        data = f.read()
        t = ast.parse(data, filename)
        
    substs = SubstData(None)
    MyNodeVisitor(filename=filename, subst_data=substs).visit(t)
    SubstDataActioner.apply_all(substs,options=SubstDataOptions(prevent_colisions=False))

#self.visit(t)
