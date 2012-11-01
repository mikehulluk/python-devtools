import re
import bisect
import collections

from dajaxice.decorators import dajaxice_register
from django.utils import simplejson


from models import MonitoredFile


@dajaxice_register
def autocomplete_change(req, search_string):
    print 'In autocomplete_change'

    if len(search_string) < 3:
        return simplejson.dumps({
            'search_string':search_string,
        'res': 'THE RESULTS ARE: '+search_string,
        'matches': {},
        })

    try:
        max_find = 200
        max_return = 10
        nmatches_total, matches_dict = search_for_text_naive(search_string,max_find=max_find, max_return=max_return)

        if nmatches_total==max_find:
            nmatches_total = ('over %d'%max_find)

    except re.error:
        return simplejson.dumps({
            'search_string':search_string,
            'error':'Invalid Regular Expression',
            })

    # Sucessful search:
    return simplejson.dumps({
         'search_string':search_string,
         'matches': matches_dict.items(),
         'numfound': nmatches_total,
         })

@dajaxice_register
def full_search(req, search_string):
    print 'In full_search'


    try:
        max_find = None
        max_return = None
        nmatches_total, matches_dict = search_for_text_naive(search_string,max_find=max_find, max_return=max_return)

    except re.error:
        return simplejson.dumps({
            'search_string':search_string,
            'error':'Invalid Regular Expression',
            })

    # Sucessful search:
    return simplejson.dumps({
         'search_string':search_string,
         'matches': matches_dict.items(),
         'numfound': nmatches_total,
         })

def _get_file_spans(search_string, max_find):
    r = re.compile(search_string)

    file_char_spans = collections.defaultdict(list)
    for mf in MonitoredFile.objects.all():

        for m in r.finditer( mf.filecontents ):
            file_char_spans[mf].append( m.span() )
        if max_find is not None and len(file_char_spans) >= max_find:
            break
    return file_char_spans



def search_for_text_naive(search_string,max_find, max_return):
    r = re.compile(search_string)

    #max_return = 10
    #max_find = 323


    file_char_spans = _get_file_spans(search_string=search_string, max_find=max_find)

    nfoundtotal = len(file_char_spans)



    results = {}
    for index,(mf, char_spans) in enumerate(file_char_spans.items()):

        if max_find is not None and index >= max_return:
            break

        # Simplify access
        contents = mf.filecontents
        full_filename = mf.full_filename

        # Calculate the positions of the lines:
        line_positions = [m_.start() for m_ in re.finditer('\n', contents)]

        def get_line_no(string_index):
            return bisect.bisect_left(line_positions, string_index)

        pre_context_lines = 1
        post_context_lines = 1 #+ 1

        merge_close_contexts = 2

        # Calculate the lines numbers for each match
        lines = [ (get_line_no(start),get_line_no(end)) for (start,end) in char_spans ]

        # Add some context around the blocks:
        context_spans = [ (start-pre_context_lines, end+post_context_lines) for (start,end) in lines ]
        context_spans.sort()

        # Merge adjacent spans:
        if merge_close_contexts is not None:
            merge_right = [ (context_spans[i][1] +merge_close_contexts >= context_spans[i+1][0]) for i in range(len(context_spans)-1)] + [False]

            print 'CS Orig', context_spans
            print 'MR', merge_right

            new_spans = []
            curr_left = None
            for i,(span,merge_right) in enumerate( zip( context_spans, merge_right)):

                if curr_left is None:
                    curr_left = span[0]

                if merge_right:
                    pass
                else:
                    new_spans.append( (curr_left,span[1]) )
                    curr_left = None

            context_spans = new_spans

        print context_spans

        # Now, lets clip to make sure we don't try to reference lines
        # off the page
        def _clip(lineno):
            return min( max(lineno,0), len(line_positions) )
        context_spans = [ (_clip(start),_clip(end)) for (start,end) in context_spans]
        print 'Clipped,', context_spans


        # Lets turn line numbers into strings:
        def get_text_lines(start,end):
            return ''.join( ['%s\n' % l for l in all_lines[start:end+1]] )

        all_lines = contents.split('\n')
        context_lines = [ ((start,end), get_text_lines(start,end))  for (start,end) in context_spans]

        # Lets highlight the matches in the strings
        def highlight_in_context(s):
            def repl_func(m):
                return "<span class='grinhighlight'>%s</span>" % s[m.start():m.end()]
            return r.sub(repl_func, s)

        context_lines = [ (linenos,highlight_in_context(block)) for (linenos,block) in context_lines]


        #match_data = ((start_line,end_line), context)

        results[full_filename] = context_lines #[ ((0,0), txt ) for txt in context_lines]


    print '%d results found' % (len(results))
    #print results
    return nfoundtotal, results











def search_for_text(search_string):
    from django.db import connection
    cursor = connection.cursor()
    # Data modifying operation - commit required
    #cursor.execute("UPDATE bar SET foo = 1 WHERE baz = %s", [self.baz])
    #transaction.commit_unless_managed()
    """
    SELECT title
    FROM pgweb
    WHERE to_tsvector('english', body) @@ to_tsquery('english', 'friend');
    """
    # Data retrieval operation - no commit required
    cursor.execute("SELECT full_filename  FROM pdtweb_monitoredfile", [])


    cursor.execute( """
    SELECT full_filename
    FROM pdtweb_monitoredfile
    WHERE to_tsvector('english', filecontents) @@ to_tsquery('english', %s)
    LIMIT 10

    """, [search_string])




    #cursor.execute("SELECT full_filename  FROM pdtweb_monitoredfiles WHERE baz = %s", [self.baz])
    #row = cursor.fetchone()
    rows = list(cursor.fetchall())
    print len(rows)
    return rows
    #print row
    #print rows
    #return

