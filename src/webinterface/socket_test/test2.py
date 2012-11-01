




import os
import threading
import time
import SocketServer


import json
from pyinotify import WatchManager, ThreadedNotifier, EventsCodes, ProcessEvent
from BaseHTTPServer import HTTPServer

import BaseHTTPServer

import urlparse

import collections


from sorted_collection import SortedCollection

class Globals:
    msgs_lock = threading.Lock()
    new_msg_event = threading.Event()

    msgs = SortedCollection(key= lambda msg:msg.msg_id)


Msg = collections.namedtuple('Msg',['msg_id','timestamp', 'msg_type','msg_details'])

class MsgTypes:
    Heartbeat='Heartbeat'
    FileChange='FileChange'
    Shutdown='Shutdown'
    Initialise='Initialise'


def time_string():
    return time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime())






class PTmp(ProcessEvent):
    def process_IN_CREATE(self, event):
        print "Create: %s" %  os.path.join(event.path, event.name)
        add_msg( MsgTypes.FileChange, msg_details= 'Created: %s' %  os.path.join(event.path, event.name) )

    def process_IN_DELETE(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)
        add_msg( MsgTypes.FileChange, msg_details= 'Deleted: %s' %  os.path.join(event.path, event.name) )

    def process_IN_MODIFY(self, event):
        print "Modify: %s" %  os.path.join(event.path, event.name)
        add_msg( MsgTypes.FileChange, msg_details= 'Modified: %s' %  os.path.join(event.path, event.name) )

    def process_IN_ATTRIB(self, event):
        print "Attrib: %s" %  os.path.join(event.path, event.name)
        add_msg( MsgTypes.FileChange, msg_details= 'Attrib: %s' %  os.path.join(event.path, event.name) )




















def add_msg(msg_type, msg_details=None):

    with Globals.msgs_lock:
        last_msg_id = Globals.msgs[-1].msg_id if Globals.msgs else 0
        msg = Msg(msg_id=last_msg_id+1,timestamp=time_string(), msg_type=msg_type, msg_details=msg_details)
        Globals.msgs.insert(msg)

    # Check the list hasn't become unwieldy long
    # TODO, this could be more efficient:
    with Globals.msgs_lock:
        while len(Globals.msgs) > 100:
            Globals.msgs.remove(Globals.msgs[0])


    # Trigger the Event:
    print 'Triggering Events'
    Globals.new_msg_event.set()
    Globals.new_msg_event.clear()










def heartbeat_messages():
    while(True):
        time.sleep(2)
        add_msg( MsgTypes.Heartbeat, None )
        print 'Heartbeat!'







def main():

    # Add a dummy message to the queue:
    add_msg( MsgTypes.Initialise )



    # Setup the HTTP server:
    SocketServer.TCPServer.allow_reuse_address = True
    HOST, PORT = "localhost", 9000
    server = ThreadedHTTPServer((HOST, PORT), MyTCPHandler)
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.daemon = True
    server_thread.start()

    # Setup the heartbeat:
    heartbeat_thread = threading.Thread(target=heartbeat_messages)
    heartbeat_thread.daemon = True
    heartbeat_thread.start()


    # Setup pyinotify so files are being watched:
    wm = WatchManager()
    notifier = ThreadedNotifier(wm, PTmp())
    notifier.start()

    mask = EventsCodes.ALL_FLAGS['IN_DELETE'] | EventsCodes.ALL_FLAGS['IN_CREATE'] | EventsCodes.ALL_FLAGS['IN_ATTRIB']   |EventsCodes.ALL_FLAGS['IN_MODIFY']# watched events
    #mask = EventsCodes.ALL_FLAGS['IN_DELETE'] | EventsCodes.ALL_FLAGS['IN_CREATE'] | EventsCodes.ALL_FLAGS['IN_MODIFY']# watched events
    wdd = wm.add_watch('../sentry.py', mask, rec=True)
    wdd = wm.add_watch('../', mask, rec=True)


    try:
        while True:
            time.sleep(5)

    except:
        # Turn off pyinotify:
        notifier.stop()

        print 'Exception caught: shutting down connections'
        add_msg(MsgTypes.Shutdown)
        time.sleep(0.5)
        print 'Terminating...'

        raise







class ThreadedHTTPServer(SocketServer.ThreadingMixIn, HTTPServer):
    daemon_threads = True

class MyTCPHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    """
    The RequestHandler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """

    @classmethod
    def extract_variables(cls, path):
        if '?' in path:
            path, tmp = path.split('?', 1)
            return urlparse.parse_qs(tmp)
        else:
            return {}


    def do_GET(self):
        params = MyTCPHandler.extract_variables(self.path)
        client_latest_msg = int( params['last_update'][0] )

        with Globals.msgs_lock:
            msgs_to_send = Globals.msgs.find_all_gt(client_latest_msg)[:]

        # Outstanding messages, lets return immediately:
        if msgs_to_send:
            last_msg = msgs_to_send[-1]
            response = json.dumps({'return':'ok','time':time_string(), 'msgs':[m._asdict() for m in msgs_to_send], 'last_id':last_msg.msg_id } )
            self.request.sendall(response)
            return

        # Otherwise, lets wait for a new event
        Globals.new_msg_event.wait()

        with Globals.msgs_lock:
            msgs_to_send = Globals.msgs.find_all_gt(client_latest_msg)

        # Outstanding messages, lets return immediately:
        response = json.dumps({'return':'ok','time':time_string(), 'msgs':[m._asdict() for m in msgs_to_send], 'last_id':msgs_to_send[-1][0]  } )
        self.request.sendall(response)
        return


if __name__ == "__main__":
    main()

