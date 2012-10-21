

import itertools


from django.core.management import setup_environ
import settings
setup_environ(settings)

from pdtweb.models import MonitoredFile, Profile


from pdt.pdtconfig import PDTProfileMgr




#for (profilename, profile) in PDTProfileMgr.profiles.iteritems():
#    print profile
#    print profile.files

filenames = list( itertools.chain(*[profile.files for profile in PDTProfileMgr.profiles.values() ] ) )

#filenames = [f.strip() for f in _filenames.split('\n') if f.strip()]














        
def update_file(filename):
    try:
        obj = MonitoredFile.objects.get(full_filename=filename)
    except MonitoredFile.DoesNotExist:
        obj = MonitoredFile.create(full_filename=filename)
        obj.save()
    obj.ensure_up_to_date()
        
        
        
        

        
        

def main():
    
    # Scan for changes in all tracked files:
    print 'Updating all files:'
    for filename in filenames:
        update_file(filename)
    print 'OK'
    
    print 'Starting Monitoring'
    #initialise_notify()
    #initialise_notify()
    
    
    
if __name__=='__main__':
    main()
    
    
    



import os
from pyinotify import WatchManager, Notifier, ThreadedNotifier, EventsCodes, ProcessEvent

class PTmp(ProcessEvent):
    def process_IN_CREATE(self, event):
        print "Create: %s" %  os.path.join(event.path, event.name)

    def process_IN_DELETE(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)
    
    def process_IN_MODIFY(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)
        
        
