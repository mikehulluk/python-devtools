#!/usr/bin/python
# -*- coding: utf-8 -*-

import itertools

from yapsy.IPlugin import IPlugin

import sys

import pdt
import pdt.patch_manager
from pdt.patch_manager import PatchSet, PatchManager
import subprocess

import re
import re
from functools import partial


def sub_func(m, text, replacement=None):

    # Check for special lyx lines:
    line_start = text.rfind('\n', 0, m.start())
    line_end = text.find('\n', m.start() + 1)
    if line_start != -1:
        line = text[line_start:line_end].strip()
        if line[0] in ['\\','#'] or line.replace('.',' ').replace('{',' ').strip().split()[0] in ['key','reference','name','filename','ref']:
            return text[m.start(): m.end()] 
    else:
        return text[m.start(): m.end()]
        
    # Add the space and return  
    if replacement is not None:
        return replacement  
    return '%s %s' % ( m.groups()[0], m.groups()[1] )





        
        




class Actions(object):
    
    @classmethod
    def RemapCitations(self, text):
        # Convert all citations to the correct form:
        text = re.sub(r"""^LatexCommand cite$""", r"""LatexCommand citep""", text, flags=re.MULTILINE )
        text = re.sub(r"""^LatexCommand citet$""", r"""LatexCommand citep""",text, flags=re.MULTILINE )

        # Ensure that there is a space between [a-zA-Z] and citation.
        spacing = re.compile( r"""([a-zA-Z])(\n\\begin_inset CommandInset citation)""", re.MULTILINE|re.DOTALL)
        text = spacing.sub(r"""\1 \2""", text)
        return text


    @classmethod
    def FixWhitespace(self, text):
        
        # Ensure a space after commas, period, colon and semi-colon
        text = re.sub(r"""(,)([^ \\\n0-9])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(\.)([^ \\\n0-9])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(;)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(:)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        
        # Ensure there is no space before commas, period, colon and semi-colon
        text = re.sub(r""" (,)""", ',', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (\.)(?!\./)""", '.', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (;)""", ';', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (:)""", ':', text, re.MULTILINE|re.DOTALL)
        
        #Two back to back punctuation marks:
        text = re.sub(r"""[,.;:][ ]*[,.;:]""", partial(sub_func, text=text, replacement='XXXXX'), text, re.MULTILINE|re.DOTALL)


        # whitespace around '(' and ')'
        text = re.sub(r"""([a-zA-Z0-9])(\()""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(\))([a-zA-Z0-9])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(\()([ ])""", partial(sub_func, text=text, replacement='('), text, re.MULTILINE|re.DOTALL)
        
        
        #text = re.sub(r"""([^a-zA-Z0-9])(\))""", partial(sub_func, text=text, replacement='XXXXXX'), text, re.MULTILINE|re.DOTALL)
        
        return text


    @classmethod
    def FixAc(self, text):


        orig = r"""\begin_inset Flex CT - acronym
status collapsed

\begin_layout Plain Layout

dINs
\end_layout

\end_inset
"""
        repl = r"""
\begin_inset Flex MH - dINs
status collapsed

\begin_layout Plain Layout

\end_layout

\end_inset
"""
        return text.replace( orig,repl)


    @classmethod
    def FixAbbreviations(self, text):
        
        
        text.decode('ascii')
        #repl.decode('ascii')
        
        
        remap = [
            ('dINs','dINs'),
            ('dIN','dIN'),
            ('cINs','cINs'),
            ('cIN','cIN'),
            ('tINs','tINs'),
            ('tIN','tIN'),
            ]
        
        for frm_,to_ in remap:
            
            repl = r"""
\\begin_inset Flex MH - %s
status collapsed

\\begin_layout Plain Layout

\end_layout

\end_inset
""" %(to_)
            
        
            r1 = re.compile(r"""(?<!\\begin_layout Subsection)(\n[^\\\n]*?)\b(%s)\b"""%frm_,)
            text = r1.sub( r'\1' + repl, text)
            
            
        #print text
        
        #print text
        text.decode('ascii')
        
        return text
        



#class OutstandingChangesException(Exception):
#    pass

from pdt.plugins.errors import OutstandingChangesException


class PluginOne(IPlugin):
    def print_name(self):
        print "This is plugin Lyx"


    def _do_plugin(self, args):
        print 'Running lyx plugin'
        
       
        # Apply the changes:
        file_changes = {}
        for filename in args.file_targets:
            print 'Filenames', filename
            with open(filename) as f:
                old_contents = f.read()
            new_contents = self.do_text(old_contents)
            
            if PatchManager.get_patches_for_filename(filename):
                raise OutstandingChangesException()
            
            if old_contents != new_contents:
                file_changes[filename] = (old_contents, new_contents)
            
        PatchManager.create_patchset(file_changes)
        
        


    def do_text(self, text, ):
        #text = Actions.RemapCitations(text)
        #text = Actions.FixWhitespace(text)
        text = Actions.FixAbbreviations(text)
        #text = Actions.FixAc(text)
        #FixAc
        return text
        
        


    def build_arg_parser(self, argparser, parent_parser, action_wrapper):
        parser = argparser.add_parser('lyx', help='* Lyx plugin', parents=[parent_parser])
        parser.set_defaults(func=action_wrapper( self._do_plugin ) )
