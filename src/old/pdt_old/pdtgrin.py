
import bisect
import fnmatch
import gzip
import itertools
import os
import re
import shlex
import stat
import sys

import argparse



def mod_grin_main(argv=None):
    try:
        if argv is None:
            # Look at the GRIN_ARGS environment variable for more arguments.
            env_args = shlex.split(os.getenv('GRIN_ARGS', ''))
            argv = [sys.argv[0]] + env_args + sys.argv[1:]
        parser = get_grin_arg_parser()
        args = parser.parse_args(argv[1:])
        if args.context is not None:
            args.before_context = args.context
            args.after_context = args.context
        args.use_color = args.force_color or (not args.no_color and
            sys.stdout.isatty() and
            (os.environ.get('TERM') != 'dumb'))

        regex = get_regex(args)
        g = GrepText(regex, args)
        openers = dict(text=open, gzip=gzip.open)
        for filename, kind in get_filenames(args):
            report = g.grep_a_file(filename, opener=openers[kind])
            sys.stdout.write(report)
    except KeyboardInterrupt:
        raise SystemExit(0)
    except IOError as e:
        if 'Broken pipe' in str(e):
            # The user is probably piping to a pager like less(1) and has exited
            # it. Just exit.
            raise SystemExit(0)
        raise
