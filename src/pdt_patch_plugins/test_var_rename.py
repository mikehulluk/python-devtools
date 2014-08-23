import ast
from collections import defaultdict

src_file = open('/home/michael/dev/NeuroUnits/src/neurounits/codegen/python_functor/functor_generator.py').read()
A = ast.parse(src_file,'??')


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
        for f in ast.iter_child_nodes(node):
            self.visit(f)
        self.is_storing = False



v = MyVisitor()
v.visit(A)
#print A
#print v.variable_names
#print v.function_line_ranges
#print v.function_parent_scopes






# Find which variables are actualy the same in different functions,
#  -- i.e. closures. Lets make a dictionary of 'root-function, variable-name' ->  'sub-function'
variable_scopes = {}
all_variables_in_func_and_children = defaultdict(set)

#Visit each function, starting with the deepest 'function-within-function-within-function'
for func,parents in sorted( v.function_parent_scopes.items(), key=lambda x: len(x[1]) ):
    pass

    # Visit all the variables in this function
    for ((name, function),positions) in v.variable_names.items() :
        if function != func:
            continue


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
        # amke sure names don't collide:
        for parent in parental_scopes:
            all_variables_in_func_and_children[parent].add(name)
        all_variables_in_func_and_children[func].add(name)




valid_names = ['p']





import re
valid_name_re = re.compile(r'''^[a-z_][a-z0-9_]{2,30}$''')
def is_valid_name(name):
    if name in valid_names:
        return True
    return valid_name_re.match(name)

variable_renames = {}
for (name,func),positions in v.variable_names.items():
    print name, func, len(positions)
    if is_valid_name(name):
        continue

    variable_renames[(name,func)] = positions

    print 'Invalid name found', name
    print "  -- Cant' map to:", all_variables_in_func_and_children[func]

    print func._fields




# Convert into text-only format:
variable_renames_text = { "%s %s"%(name,func.name): list(positions) for ((name,func),positions) in variable_renames.items() }
print variable_renames_text



import json
data = {
    'variable_renames':variable_renames_text,
    #'all_variables_in_func_and_children': all_variables_in_func_and_children
    }
out_file = '/home/michael/Desktop/renames.json'
with open(out_file, 'w') as f:
    pass
    txt = json.dumps(data, sort_keys=True, indent=4, separators=(',', ': '))
    f.write(txt)


#import wx
#app = wx.App()
#frame = wx.Frame(None, -1, 'simple.py')
#frame.Show()
#app.MainLoop()










#
