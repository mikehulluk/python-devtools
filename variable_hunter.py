


import ast
import __builtin__
import os
import re
import itertools
from coreclasses import RemapData, SubstData
from itertools import izip
import colorama
colorama.init()
from colorama import Fore, Back, Style
import sys




class ScopeType:
    Class = 'Class'
    Function = 'Function'
    Lambda = 'Lambda'
    Module = 'Module'

class VariableTyoe:
    Param = 'Param'
    Local = 'Local'


class Scope(object):
    def __init__(self, name, parent_scope, scope_type, start_line, module_visitor):
        self.name = name
        self._declared_variables = set()
        self._parameters = set()
        self._known_objects = set()
        self._parent_scope = parent_scope
        self._scope_type = scope_type
        self._start_line = start_line
        self._finish_line = None
        self._module_visitor = module_visitor

    @property
    def local_variables(self):
        return [v for v in self._declared_variables if not v in self._parameters ]

    def is_variable_declared(self, variable_name):
        return variable_name in self._declared_variables or variable_name in self._known_objects or variable_name in self._parameters

    def declare_variable(self, variable):
        self._declared_variables.add(variable)
    def declare_variables(self, variables):
        for variable in variables:
            self._declared_variables.add(variable)

    def declare_object(self, obj_name):
        self._known_objects.add(obj_name)
    def declare_parameter(self, obj_name):
        self._parameters.add(obj_name)

    def declare_parameters(self, obj_names):
        for obj_name in obj_names:
            self._parameters.add(obj_name)

    
    def filename(self):
        return self._module_visitor._filename
    def full_name(self):
        if self._parent_scope == None:
            return self.name if self.name else ''
        else:
            return self._parent_scope.full_name() + '.' + self.name

    def get_contents(self):
        start_line = self._start_line[0]
        finish_line = self._finish_line[0]
        return '\n'.join( self._module_visitor._data.splitlines()[start_line-1:finish_line] )

class MyNodeVisitor(ast.NodeVisitor):


    def __init__(self, filename):
        super(MyNodeVisitor,self).__init__()
        self.assign_on = False
        self.scope_stack = []
        self.current_scope = None
        self.scopes = []
        self._last_lineposition = None

        self._filename = filename
        self._data = None
        with open(filename) as f:
            self._data = f.read()
            t = ast.parse(self._data, filename)
        self.visit(t)

    # Helper methods for resolving scope:
    # #######################################
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

    def generic_visit(self, node):
        #print node.__dict__

        if isinstance(node, (ast.expr, ast.stmt)):
            if not self._last_lineposition or node.lineno >= self._last_lineposition[0]:
                #print "Uupdating Last linenoe:", node.lineno, node
                self._last_lineposition = (node.lineno, node.col_offset)

        super(MyNodeVisitor, self).generic_visit(node)



    def visit_ClassDef(self, node):
        self.current_scope.declare_object(node.name)

        self.current_scope.declare_variable(node.name)
        new_scope = Scope(node.name, parent_scope=self.current_scope, scope_type=ScopeType.Class, start_line=(node.lineno, node.col_offset), module_visitor=self)
        self.scopes.append(new_scope)
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        self.generic_visit(node)

        # Finish up:
        
        self.current_scope._finish_line = self._last_lineposition
        self.current_scope = self.scope_stack.pop()

    def visit_FunctionDef(self, node):
        self.current_scope.declare_object(node.name)
        new_scope = Scope(name=node.name, parent_scope=self.current_scope, scope_type=ScopeType.Function, start_line=(node.lineno, node.col_offset), module_visitor=self )
        self.scopes.append(new_scope)
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        # Handle arguments:
        print node.name
        arg_names = [arg.id for arg in node.args.args]
        new_scope.declare_parameters(arg_names)
        if node.args.kwarg:
            new_scope.declare_parameter(node.args.kwarg)
        if node.args.vararg:
            new_scope.declare_parameter(node.args.vararg)

        self.generic_visit(node)

        # Finish up:
        print 'lastline', self._last_lineposition
        self.current_scope._finish_line = self._last_lineposition
        self.current_scope = self.scope_stack.pop()

    def visit_Lambda(self, node):
        new_scope = Scope('<lambda>', parent_scope=self.current_scope, scope_type=ScopeType.Lambda, start_line=(node.lineno, node.col_offset), module_visitor=self )
        self.scope_stack.append(self.current_scope)
        self.current_scope = new_scope

        # Handle arguments:
        arg_names = [arg.id for arg in node.args.args]
        new_scope.declare_parameters(arg_names)

        self.generic_visit(node)

        # Finish up:
        self.current_scope._finish_line = self._last_lineposition
        self.current_scope = self.scope_stack.pop()





    def visit_Module(self, node):
        new_scope = Scope(None, parent_scope=None, scope_type=ScopeType.Module, start_line=None, module_visitor=self)
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
                self.current_scope.declare_object(impname.asname)
            else:
                self.current_scope.declare_object(impname.name)

    def visit_ImportFrom(self, node):
        for impname in node.names:
            if impname.asname:
                self.current_scope.declare_object(impname.asname)
            else:
                self.current_scope.declare_object(impname.name)



def analyse_file(filename):
    myvisitor = MyNodeVisitor(filename)
    return myvisitor



def get_filenames(root_dir):

    filenames_out = []
    for (dirpath,dirnames, filenames) in os.walk(root_dir):
        for fname in filenames:
            if not fname.endswith('.py'):
                continue
            full_name =  os.path.join(dirpath,fname)
            filenames_out.append(full_name)
    return filenames_out




var_regex = re.compile('''^[a-z_][a-z0-9_]{2,30}$''')
def is_valid_variable_name(varname):
    if varname.startswith('t_'):
        return True
    if varname[0] in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_':
        return True

    if not var_regex.match(varname):
        return False
    return True





#

def as_pairs(seq):
    return itertools.izip(seq[::2], seq[1::2])


def print_and_highlight_variable_in_block(blk, variable):
    print Fore.YELLOW
    spl = re.split(r'''\b(%s)\b'''%variable, blk)
    pairs = as_pairs(spl) 
    for (a,b) in pairs:
        sys.stdout.write( '%s%s%s%s%s%s' %( Fore.YELLOW, a, Style.BRIGHT, Fore.BLUE, b, Style.RESET_ALL ) )
    print '%s%s'%( Fore.YELLOW, spl[-1] )
    print
    print














def resolve_variable_in_scopes(var_name, var_scopes, subst_data):

        # Clear the screen:
        print chr(27) + "[2J"

        print Fore.GREEN, 
        print 'Resolving variables called: "%s"' % (var_name)
        print '#####################################'

        for index,vs in enumerate(var_scopes):
            print Fore.GREEN
            print ('<%d>'%index), 'In scope:', Fore.RED, Style.BRIGHT, vs.filename(), vs.full_name(), Style.RESET_ALL
            blk = vs.get_contents()
            print_and_highlight_variable_in_block(blk, var_name)


        user_input = raw_input('Change which occurances [NONE]:')
        if user_input  in ['-', '*']:
            indices = range(len(var_scopes))
        elif user_input == '':
            indices =[]
        else:
            indices = [ int(i) for i in user_input.replace(',',' ').split() ]

        if not indices:
            return

        print 'Changing in indices:', indices

        change_to = raw_input('Change to:')


        for index in indices:
            scope = var_scopes[index]
            subst_data.add_substitution(
                    filename = scope.filename(),
                    line_range = ( scope._start_line[0], scope._finish_line[0] ),
                    src_regex = r'''\b%s\b''' % var_name,
                    target_regex = change_to.strip() )

        subst_data.write_to_disk()

def resolve_variable_everywhere(var_name, scopes, subst_data):
        var_scopes = [scope for scope in scopes if var_name in scope.local_variables]

        # Is this variable rename already handled?

        while var_scopes:
            sub_var_scopes = []
            while sum( [len(s.get_contents().split('\n')) for s in sub_var_scopes]) < 20 and var_scopes:
                sub_var_scopes.append(var_scopes.pop())
            resolve_variable_in_scopes(var_name, sub_var_scopes, subst_data=subst_data)






def find_and_replace_invalid_localvariable_names(files, subst_data, remap_data):

    # Don't apply new substitutions to files with existing changes to be made:
    files = [filename for filename in files if not filename in subst_data.filenames]

    visitors = []
    for f in files:
        try:
            visitors.append( MyNodeVisitor(f) )
        except:
            print 'Problem parsing:', f
            raise
            pass

    scopes = list( itertools.chain(*[v.scopes for v in visitors])  )
    variables = set(itertools.chain(*[ s.local_variables for s in scopes]))
    vars = list(variables)

    all_vars = set(vars)
    print len(all_vars)
#


    for varname in sorted(all_vars):
        if varname in remap_data.not_remapped:
            continue
        if not is_valid_variable_name(varname):
            resolve_variable_everywhere(varname, scopes=scopes, subst_data=subst_data)




def find_and_replace_invalid_localvariable_names_in_profile(profile):
    print 'Renaming local variables in profile:', profile
    find_and_replace_invalid_localvariable_names(
            files=profile.files, 
            remap_data = RemapData(),
            subst_data = SubstData.from_file(profile.find_and_replace_filename_regular),
            )




#if __name__ == '__main__':
#    remap_data = RemapData()
#    subst_data = SubstData.from_file('~/Desktop/substsNew.txt')
#
#    files = get_filenames('/home/michael/hw_to_come//morphforge/src/morphforge/')
#    find_and_replace_invalid_localvariable_names(files=files, subst_data=subst_data, remap_data=remap_data)


