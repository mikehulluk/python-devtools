


import ast
import __builtin__

class Scope(object):
    def __init__(self, name, parent_scope):
        self.name = name
        self.defined_variables = set()
        self.parent_scope = parent_scope

    def is_variable_declared(self, variable_name):
        return variable_name in self.defined_variables
    def declare_variable(self, variable, parameter=False):
        self.defined_variables.add(variable)
    def declare_variables(self, variables, parameter=False):
        for variable in variables:
            self.defined_variables.add(variable)

    def full_name(self):
        if self.parent_scope == None:
            return self.name if self.name else ''
        else:
            return self.parent_scope.full_name() + '.' + self.name

class MyNodeVisitor(ast.NodeVisitor):

    def __init__(self):
        super(MyNodeVisitor,self).__init__()
        self.assign_on = False
        self.scope_stack = []
        self.current_scope = None
        self.scopes = []

    def is_variable_in_current_scope(self, variable_name):
        return self.current_scope.is_variable_declared(variable_name)

    def is_variable_in_parent_scope(self, variable_name):
        for scope in self.scope_stack:
            if scope.is_variable_declared(variable_name):
                return True
        return False

    def is_variable_builtin(self, variable_name):
        return variable_name in __builtin__.__dict__

    def visit(self, node):
        super(MyNodeVisitor, self).visit(node)

    def visit_FunctionDef(self, node):
        self.current_scope.declare_variable(node.name)
        new_scope = Scope(node.name, parent_scope=self.current_scope)
        self.scopes.append(new_scope)
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        # Handle arguments:
        arg_names = [arg.id for arg in node.args.args]
        new_scope.declare_variables(arg_names, parameter=True)
        if node.args.kwarg:
            new_scope.declare_variable(node.args.kwarg, parameter=True)

        self.generic_visit(node)

        # Finish up:
        self.current_scope = self.scope_stack.pop()

    def visit_Lambda(self, node):
        #self.current_scope.declare_variable(node.name)
        new_scope = Scope('<lambda>', parent_scope=self.current_scope)
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        # Handle arguments:
        arg_names = [arg.id for arg in node.args.args]
        new_scope.declare_variables(arg_names, parameter=True)

        self.generic_visit(node)

        # Finish up:
        self.current_scope = self.scope_stack.pop()

    def visit_Module(self, node):
        new_scope = Scope(None, parent_scope=None)
        self.current_scope = new_scope
        self.generic_visit(node)
        # Finish up:



    def visit_Name(self, node):
        self.generic_visit(node)
        if self.assign_on:
            self.current_scope.declare_variable(node.id)
        else:
            if not self.is_variable_in_current_scope(node.id) and not self.is_variable_in_parent_scope(node.id) and not self.is_variable_builtin(node.id):
                print "Haven't declared: '%s' anywhere" % node.id

    def visit_Assign(self, node):
        self.assign_on = True 
        self.generic_visit(node)
        self.assign_on = False

    def visit_For(self, node):
        self.assign_on=True
        self.visit(node.target)
        self.assign_on=False

    def visit_ListComp(self, node):
        self.assign_on=True
        for gen in node.generators:
            self.visit(gen.target)
        self.assign_on=False


    def visit_Import(self, node):
        for impname in node.names:
            if impname.asname:
                self.current_scope.declare_variable(impname.asname)
            else:
                self.current_scope.declare_variable(impname.name)

    def visit_ImportFrom(self, node):
        for impname in node.names:
            if impname.asname:
                self.current_scope.declare_variable(impname.asname)
            else:
                self.current_scope.declare_variable(impname.name)

    def visit_ClassDef(self, node):
        self.current_scope.declare_variable(node.name)

        self.current_scope.declare_variable(node.name)
        new_scope = Scope(node.name, parent_scope=self.current_scope)
        self.scopes.append(new_scope)
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        self.generic_visit(node)

        # Finish up:
        self.current_scope = self.scope_stack.pop()


def analyse_file(filename):
    with open(filename) as f:
        t = ast.parse(f.read(), filename)
    myvisitor = MyNodeVisitor()
    myvisitor.visit(t)

    return myvisitor
    print filename

#print myvisitor.scopes
#for scope in myvisitor.scopes:
#    print scope.full_name()
#    print '  ', scope.defined_variables

import os

filenames_out = []
for (dirpath,dirnames, filenames) in os.walk('/home/mhtest/hw/morphforge/src/morphforge/'):
    for fname in filenames:
        if not fname.endswith('.py'):
            continue
        full_name =  os.path.join(dirpath,fname)
        #print full_name
        filenames_out.append(full_name)


vars = list()
for filename in filenames_out:
    print filename
    v = analyse_file(filename = filename)
    for scope in v.scopes:
        vars.extend(list(scope.defined_variables))

print set(vars)

    


analyse_file(filename = '/home/mhtest/hw/morphforge/src/morphforge/simulation/neuron/core/neuronsimulation.py')
analyse_file(filename = '/home/mhtest/hw/morphforge/src/morphforge/simulation/neuron/core/neuronsimulationenvironment.py')
analyse_file(filename = '/home/mhtest/hw/morphforge/src/morphforge/simulation/neuron/core/neuronsimulationsettings.py')
    #print filenames



