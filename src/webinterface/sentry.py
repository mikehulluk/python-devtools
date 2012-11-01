

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
    #print 'Updating all files:'
    #for filename in filenames:
    #    update_file(filename)
    #print 'OK'

    print 'Starting Monitoring'

    watch_routine()
    #initialise_notify()
    #initialise_notify()






import os
from pyinotify import WatchManager, Notifier, ThreadedNotifier, EventsCodes, ProcessEvent

previous_modification_time

class PTmp(ProcessEvent):
    def process_IN_CREATE(self, event):
        print "Create: %s" %  os.path.join(event.path, event.name)

    def process_IN_DELETE(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)

    def process_IN_MODIFY(self, event):
        print "Modify: %s" %  os.path.join(event.path, event.name)







def watch_routine():
    wm = WatchManager()
    notifier = Notifier(wm, PTmp())

    mask = EventsCodes.ALL_FLAGS['IN_DELETE'] | EventsCodes.ALL_FLAGS['IN_CREATE']  | EventsCodes.ALL_FLAGS['IN_ACCESS']  |EventsCodes.ALL_FLAGS['IN_MODIFY']# watched events
    #wdd = wm.add_watch('/tmp', mask, rec=True)
    wdd = wm.add_watch('sentry.py', mask, rec=True)
    wdd = wm.add_watch('../', mask, rec=True)

    while True:  # loop forever
        try:
            # process the queue of events as explained above
            notifier.process_events()
            if notifier.check_events():
                # read notified events and enqeue them
                notifier.read_events()
            # you can do some tasks here...
        except KeyboardInterrupt:
            # destroy the inotify's instance on this interrupt (stop monitoring)
            notifier.stop()
            break



if __name__=='__main__':
    main()







