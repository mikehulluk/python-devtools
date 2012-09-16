



import argparse

parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('--fromfile', action='append', default=[], help='process a list full of substituions')



from coreclasses import SubstData, SubstDataActioner, SubstDataOptions


def do_fromfile(filename, args):
    print 'FromFile', filename
    substs = SubstData.from_file(filename)
    SubstDataActioner.apply_all(substs, options=SubstDataOptions())
    

def main():
    print 'Safe-Substituter'

    args = parser.parse_args()

    # From a file:
    for fromfile in args.fromfile:
        do_fromfile(fromfile, args)


main()

