from dajaxice.decorators import dajaxice_register
#\from ajaxsite.blog.forms import ContactForm
from django.utils import simplejson

@dajaxice_register
def autocomplete_change(req, search_string):
    print 'In autocomplete_change'

    if len(search_string) < 3:
        filenames = ['...']

    else:
        filenames = search_for_text(search_string)
        filenames[:10]
    
    print filenames
    return simplejson.dumps({
        'res': 'THE RESULTS ARE'+search_string,
        'matches': filenames,
        })




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
    WHERE to_tsvector('english', filecontents) @@ to_tsquery('english', %s)""", [search_string])
    


    #cursor.execute("SELECT full_filename  FROM pdtweb_monitoredfiles WHERE baz = %s", [self.baz])
    #row = cursor.fetchone()
    rows = list(cursor.fetchall())
    print len(rows)
    return rows
    #print row
    #print rows
    #return
