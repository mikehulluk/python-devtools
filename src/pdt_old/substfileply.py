
import ply

sample = r""">> '/home/michael/hw_to_come//morphforge/src/morphforge/traces/methods/trace_methods_std_filters.py'
  ** [82:97] '\ba\b' 'polycoeff_num' 
  ** [60:76] '\ba\b' 'polycoeff_num'
>> '/home/michael/hw_to_come//morphforge/src/morphforge/traces/methods/trace_methods_std_filters.py'
  ** [82:97] '\ba\b' 'polycoeff_num' 
  ** [60:76] '\ba\b' 'polycoeff_num'
"""

import ply.lex as lex
import re


def t_STRING(t):
    #r"""'[^']*'"""
    #r"""'[^']*'"""
    r"""(?<!\\)(?:\\\\)*'((?:\\.|[^\\'])*)'"""
    t.value = t.value[1:-1]
    t.value = t.value.replace("\'","'")
    return t
def t_NUMBER(t):
    r"""-?\d+"""
    t.value = int(t.value)
    return t


# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    raise RuntimeError()

t_DOUBLEARROW = '>>'
t_DOUBLESTAR = '\*\*'
t_COLON = ':'
t_SQUAREOPEN = '\['
t_SQUARECLOSE = '\]'
t_ignore = ' \t\n'

tokens = [
    'STRING',
    'NUMBER',
    'DOUBLEARROW',
    'DOUBLESTAR',
    'COLON',
    'SQUAREOPEN',
    'SQUARECLOSE',
    ]


def p_file0(p):
    r""" file : empty"""
    p[0] = []

def p_file1(p):
    r""" file : file filesubstblock"""
    p[0] = p[1] + [p[2]]


def p_empty(p):
    r'''empty : '''
    p[0] = None

def p_subst_block0(p):
    r""" filesubstblock : DOUBLEARROW STRING substlines"""
    print p[2]
    p[0] = (p[2], p[3])


def p_subst_block1(p):
    r""" substlines : empty"""
    p[0] = []

def p_subst_block2(p):
    r""" substlines : substlines substline"""
    p[0] = p[1] + [p[2]]

def p_subst_line0(p):
    r""" substline : DOUBLESTAR line_range STRING STRING """
    p[0] = (p[2], p[3], p[4])


def p_subst_linerange0(p):
    r""" line_range : empty"""
    p[0] = None

def p_subst_linerange1(p):
    r""" line_range : SQUAREOPEN NUMBER COLON NUMBER SQUARECLOSE"""
    p[0] = (p[2],p[4])



def parse_string(src):
    import ply.yacc
    import ply.lex
    lexer = ply.lex.lex()
    parser = ply.yacc.yacc()

    x = parser.parse(src, lexer=lexer)
    return x
