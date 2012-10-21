# Create your views here.
from django.shortcuts import render_to_response
from django.template import RequestContext


from models import MonitoredFile, Profile




def home(request):
    
    data = {'allfiles':MonitoredFile.objects.all() }
    return render_to_response('home.html', RequestContext(request,data) )



    
