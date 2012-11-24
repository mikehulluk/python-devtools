
from yapsy.IPlugin import IPlugin
import re
from functools import partial

def sub_func(m, text):
    
    # Check for specil lyx lines:
    line_start = text.rfind('\n', 0, m.start())
    line_end = text.find('\n', m.start()+1,)
    if line_start != -1:
        line = text[line_start:line_end].strip()
        if line[0] in ['\\','#'] or line.split()[0] in ['key','reference','name','filename','ref']:
            return ''.join(m.groups()) 
    else:
        return ''.join(m.groups())
        
    # Add the space and return    
    return '%s %s' % ( m.groups()[0], m.groups()[1] )


class LyxWhitespaceFormatter(IPlugin):

    def action_text(self, text):
        
        # Ensure a space after commas, period, colon and semi-colon
        text = re.sub(r"""(,)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(\.)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(;)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        text = re.sub(r"""(:)([^ \\\n])""", partial(sub_func, text=text), text, re.MULTILINE|re.DOTALL)
        
        # Ensure there is no space before commas, period, colon and semi-colon
        text = re.sub(r""" (,)""", ',', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (\.)""", '.', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (;)""", ';', text, re.MULTILINE|re.DOTALL)
        text = re.sub(r""" (:)""", ':', text, re.MULTILINE|re.DOTALL)
        
        return text
        
        
