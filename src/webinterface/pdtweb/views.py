# Create your views here.
from django.shortcuts import render_to_response
from django.template import RequestContext


from models import MonitoredFile, Profile




def home(request):
    
    data = {'allfiles':MonitoredFile.objects.all(),'request':request }
    return render_to_response('grin.html', data, context_instance=RequestContext(request) )



    
