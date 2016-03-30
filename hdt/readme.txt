


# Issues:
# -------
Each editting operation operates on the original file, to produce a patch. 
For example:
File: contents.txt
Op1: contents.txt -> (s/foo/Foo/) -> patch1
Op2: contents.txt -> (s/bar/Bar/) -> patch2
Then applying does:
contents.txt -> (+patch1) -> (+patch2) -> out.txt

This means that if Op1 and Op2 edit the same lines, then this will fail!
Whether this is good or bad, is to be decided!

