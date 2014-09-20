import sys
import os




# add 'grin', if its available:
    # grin is nicely designed to play with other tools, so the
    # argument parsing is straight forward.



def do_grin(args):
    import grin

    if args.context is not None:
        args.before_context = args.context
        args.after_context = args.context
    args.use_color = args.force_color or (not args.no_color and
        sys.stdout.isatty() and
        (os.environ.get('TERM') != 'dumb'))

    regex = grin.get_regex(args)
    g = grin.GrepText(regex, args)

    for filename in args.source_files:
        report = g.grep_a_file(filename, opener=open)
        sys.stdout.write(report)




class DoGrinObj(object):

    @classmethod
    def build_arg_parser(cls, argparser, parent_parsers):
        try:
            import grin
            sp_grin = argparser.add_parser('grin',
                    parents=parent_parsers,
                    help='Use grin on the targetted files')
            grin.get_grin_arg_parser(sp_grin)
            sp_grin.set_defaults(func=do_grin)
        except ImportError:
            pass
