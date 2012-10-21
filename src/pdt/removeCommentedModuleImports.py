assert False





import re
import shutil
import os


rModule1 = re.compile(r"""[#] (\s)* from (.*) import (.*)""", re.VERBOSE)
rModule2 = re.compile(r"""[#] (\s)* import (.*) """, re.VERBOSE)

def removeFromFile( filename, pretend=True):

    output = []
    changes = False
    with open(filename) as f:
        lines = f.readlines()
        for l in lines:

            # Skip the matches:
            if rModule1.match(l) or rModule2.match(l):
                print l
                changes = True
            # Add everything else:
            else:
                output.append(l)

    if pretend:
        print 'Rewriting File:', filename
        return

    if changes:
        shutil.move(filename, filename+".back")
        with open(filename,'w') as f:
            f.write( "".join(output ) )


for root, dirs, files in os.walk('.'):
    #if root[0] == '.':
    #    continue

    for f in files:
        fName = os.path.join( root,f )
        if fName.endswith('.py'):
            removeFromFile(fName, pretend=False)


