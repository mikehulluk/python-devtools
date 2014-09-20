import ast
from collections import defaultdict



class MyVisitor(ast.NodeVisitor):
    def __init__(self,):
        self.func_scope_stack = [None,]
        self.current_line_no = 1

        self.variable_names = defaultdict(set)
        self.function_line_ranges = {}
        self.function_parent_scopes = {}

        self.is_storing = False

    def generic_visit(self,node):
        # Keep a track of the current line number:
        try:
            self.current_line_no = node.lineno
        except AttributeError:
            pass
        # Visit the node:
        super(MyVisitor,self).generic_visit(node)
        #print node

    def visit_FunctionDef(self,node):
        print "Func:",node, node._fields
        self.func_scope_stack.append(node)
        start_line = node.lineno
        for f in ast.iter_child_nodes(node):
            self.visit(f)
        self.func_scope_stack = self.func_scope_stack[:-1]
        end_line = self.current_line_no

        # Record the line numbers:
        self.function_line_ranges[node] = (start_line,end_line)

        # Record what functions we are in:
        self.function_parent_scopes[node] = self.func_scope_stack


    def visit_Name(self,node):
        #print 'Name:', node
        #print node._fields
        #print node.id, 'in', self.func_scope_stack
        if self.is_storing:
            pos = (node.lineno,node.col_offset)
            self.variable_names[(node.id,self.func_scope_stack[-1])].add(pos)

    def visit_Assign(self,node):
        self.is_storing = True
        print node._fields
        print node.targets
        #assert False
        for f in ast.iter_child_nodes(node):
            print f
            self.visit(f)
        self.is_storing = False




valid_names = ['None']

import re
valid_name_re = re.compile(r'''^[a-z_][a-z0-9_]{2,30}$''')
def is_valid_name(name):
    if name in valid_names:
        return True
    return valid_name_re.match(name)














def find_potential_variable_renames(src_filename):
    src_file = open(src_filename).read()
    A = ast.parse(src_file,'??')

    v = MyVisitor()
    v.visit(A)
    #print A
    #print v.variable_names
    #print v.function_line_ranges
    #print v.function_parent_scopes






    # Find which variables are actualy the same in different functions,
    #  -- i.e. closures. Lets make a dictionary of 'root-function, variable-name' ->  'sub-function'
    all_variables_in_func_and_children = defaultdict(set)


    # Visit all the variables in this function
    for ((name, function),positions) in v.variable_names.items() :
        parental_scopes = [parent_func for parent_func in v.function_parent_scopes[function] if (name,parent_func) in v.variable_names]

        # OK, there is an enclosing function scope, with a variable by the
        # same name. So, lets join the references to it by copying this
        # reference into the enclosing scope.
        if parental_scopes:
            root_func = parental_scopes[0]

            for pos in positions:
                v.variable_names[(name,root_func)].add(pos)
            del v.variable_names[(name,function)]

        # Add this variable name to all the enclosing scopes, which we need to
        # make sure names don't collide:
        for parent in parental_scopes:
            all_variables_in_func_and_children[parent].add(name)
        all_variables_in_func_and_children[function].add(name)





    # Find invalid variable names:
    variable_renames = { (name,func):positions for (name,func),positions in v.variable_names.items() if not is_valid_name(name) }


    # Merge into
    # { filename : {
    #                full_function_name : {
    #                   'existing_variables' : [ ]
    #                   'invalid_variables' : {
    #                            'var1' : [positions],
    #                            'var2' : [positions],
    #                                           }
    #                                           }
    #                       ]
    #                }

    file_data = {}
    functions = set(f for (n,f) in v.variable_names.keys())
    for function in functions:
        invalid_vars = { name:list(positions) for ((name, _function),positions) in variable_renames.items() if function==_function}
        
        if not invalid_vars:
            continue

        file_data[function.name] = {
                'existing_variables' : list(all_variables_in_func_and_children[function]),
                'invalid_variables' : invalid_vars,
                }

    return file_data



src_dir = '/home/michael/dev/NeuroUnits/src/'
src_filenames = [
    'neurounits/codegen/python_functor/functor_generator.py',
    'neurounits/codegen/python_functor/simulate_component.py'
    ]

merged_data = { src_filename : find_potential_variable_renames(src_dir+src_filename) for src_filename in src_filenames}


import json
out_file = '/home/michael/dart/test_app1/web/renames.json'
with open(out_file, 'w') as f:
    pass
    txt = json.dumps(merged_data, sort_keys=True, indent=4, separators=(',', ': '))
    f.write(txt)


#import wx
#app = wx.App() #frame = wx.Frame(None, -1, 'simple.py') #frame.Show()
#app.MainLoop()










#
