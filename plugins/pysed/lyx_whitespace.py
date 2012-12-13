
from yapsy.IPlugin import IPlugin
import re
from functools import partial

def sub_func(m, text, replacement=None):
    
    
    
    # Check for specil lyx lines:
    line_start = text.rfind('\n', 0, m.start())
    line_end = text.find('\n', m.start()+1,)
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




class LyxWhitespaceFormatter(IPlugin):

    def action_text(self, text):
        
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
        
        
