




import os
import sys
import threading
import time
import SocketServer


import json
from pyinotify import WatchManager, ThreadedNotifier, EventsCodes, ProcessEvent




# There is a bug in this; that is if messages are sent in close proximity, then the first might 
# be sent, and a connection between client-server might not be re-established unti





open_handles = set()
open_handles_lock = threading.Lock()

msgs = []
msgs_to_send = []
msgs_to_send_lock = threading.Lock()
msgs_lock = threading.Lock()

handles_copying_data = set()
handles_copying_data_lock = threading.Lock()


updating_subscriber_lock = threading.Lock()

condition = threading.Event()

last_transmission = time.time()



#is_shutting_down = False


def time_string():
    return time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime())


class MyTCPHandler(SocketServer.BaseRequestHandler):
    """
    The RequestHandler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """

    def handle(self):
        #if is_shutting_down:
        #    time_str = strftime("%Y-%m-%d %H:%M:%S", gmtime())
        #    response = json.dumps({'time':time_str, 'messages':(MsgTypes.Shutdown, None)})
        #    self.request.sendall(response)
        #    return


        # self.request is the TCP socket connected to the client
        #self.data = self.request.recv(1024).strip()



        # Add this handle to the list of active handles:
        with updating_subscriber_lock:
            with open_handles_lock:
                open_handles.add(self)

        # The thread will sit here, waiting for something to happen
        condition.wait()
        #print '\nProcessing Thread', self

        ## OK, lets copy all the information we will need:
        with msgs_to_send_lock:
            msg_copy = msgs_to_send[:]
            print 'len msgs_copy', len(msg_copy)

        # Now, let the original thread know we are done
        with handles_copying_data_lock:
            handles_copying_data.remove(self)


        # Add carry on returning the data to the client:
        #print 'Thread returning', self
        response = json.dumps({'return':'ok','time':time_string(), 'messages':msg_copy})
        self.request.sendall(response)
        return


class ThreadedTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    daemon_threads = True
    pass










class MsgTypes:
    Heartbeat='Heartbeat'
    FileChange='FileChange'
    Shutdown='Shutdown'










def _update_subscribers():
        global msgs, msgs_to_send
        global handles_copying_data
        global open_handles_lock
        global open_handles
        global last_transmission



        with updating_subscriber_lock:

            with msgs_lock:
                if not msgs:
                    return

                data_msgs = [m for m in msgs if m[0] != MsgTypes.Heartbeat]
                if data_msgs:
                    print 'Pushing %d msgs' % len(data_msgs)


            with open_handles_lock:

                # Lets look at which children we are expecting
                # to respond to:
                with handles_copying_data_lock:
                    handles_copying_data.clear()
                    handles_copying_data.update(open_handles)
                    open_handles.clear()

                with msgs_lock:
                    with msgs_to_send_lock:
                        msgs_to_send = msgs[:]
                        msgs=[]
                        print 'MsgsToSend',len(msgs_to_send)

                # Trigger all the handler to wake up:
                condition.set()
                condition.clear()

                tstart = time.time()
                while True:
                    time.sleep(0.1)
                    with handles_copying_data_lock:
                        if not handles_copying_data:
                            break
                        print 'Waiting on thread:', handles_copying_data

                        # Timeout:
                        if time.time() - tstart > 5:
                            print 'Thread not finishing after 5s. Finishing anyway.'
                            handles_copying_data.clear()
                            break
                    time.sleep(0.2)

                # Clear the message list
                with msgs_lock:
                    msgs_to_send = []

            # Sleep, to allow connections to restablish (HACK, see top)
            time.sleep(2)



def update_subscribers(msg_type, details=None):

        global msgs, msgs_lock
        global last_transmission


        # Don't worry about a heartbeat, if we transmitted a message recently:
        if (not time.time() - last_transmission > 1 ) and msg_type==MsgTypes.Heartbeat:
            return

        with msgs_lock:
            msgs.append( (msg_type, details) )

        # This allows multiple messages to stack up
        time.sleep(0)

        _update_subscribers()
        last_transmission = time.time()







class PTmp(ProcessEvent):
    def process_IN_CREATE(self, event):
        print
        print "Create: %s" %  os.path.join(event.path, event.name)
        threading.Thread(target=update_subscribers, kwargs={
                    'msg_type':MsgTypes.FileChange,
                    'details': 'Created: %s' %  os.path.join(event.path, event.name)
                        }).start()


    def process_IN_DELETE(self, event):
        print
        print "Remove: %s" %  os.path.join(event.path, event.name)
        threading.Thread(target=update_subscribers, kwargs={
                    'msg_type':MsgTypes.FileChange,
                    'details': 'Deleted: %s' %  os.path.join(event.path, event.name)
                        }).start()

    def process_IN_MODIFY(self, event):
        print
        print "Modify: %s" %  os.path.join(event.path, event.name)
        threading.Thread(target=update_subscribers, kwargs={
                    'msg_type':MsgTypes.FileChange,
                    'details': 'Modified: %s' %  os.path.join(event.path, event.name)
                        }).start()

    def process_IN_ATTRIB(self, event):
        print
        print "Attrib: %s" %  os.path.join(event.path, event.name)
        threading.Thread(target=update_subscribers, kwargs={
                    'msg_type':MsgTypes.FileChange,
                    'details': 'Attrib: %s' %  os.path.join(event.path, event.name)
                        }).start()








def main_loop():

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

            print '\rConnections to %d clients. (%s)' % (len(open_handles), time_string()),
            sys.stdout.flush()
            update_subscribers(msg_type=MsgTypes.Heartbeat)
            time.sleep(5)

    except:
        notifier.stop()
        raise











def main():


    # For testing: sometimes it takes some time for the
    # port to be released after shutdown.
    SocketServer.TCPServer.allow_reuse_address = True

    HOST, PORT = "localhost", 9008
    server = ThreadedTCPServer((HOST, PORT), MyTCPHandler)


    try:

        # Setup a thread that manages the HTTP requests. These requests are
        # held open, until some new information.
        server_thread = threading.Thread(target=server.serve_forever)
        server_thread.daemon = True
        server_thread.start()

        main_loop()

    except KeyboardInterrupt:
        update_subscribers(msg_type=MsgTypes.Shutdown)
        condition.set()
        condition.clear()


        #condition.set()
        raise






if __name__ == "__main__":
    try:
        main()
    except:
        print 'Top level exception handler'
        raise




