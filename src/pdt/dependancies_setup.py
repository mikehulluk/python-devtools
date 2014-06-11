
import os
import sys


# Dynamically add 'glob2' to the path:
# (from ROOT/src/pdt to ROOT/glob2/)
local_path = os.path.dirname(__file__)
glob2_src = os.path.join(local_path, '../../dependancies/glob2/src/')
sys.path.append(glob2_src)


local_path = os.path.dirname(__file__)
yapsy_src = os.path.join(local_path, '../../dependancies/yapsy/package/')
sys.path.append(yapsy_src)

# Check it all works:
import glob2
import yapsy


