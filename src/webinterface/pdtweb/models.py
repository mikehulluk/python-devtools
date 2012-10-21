from django.db import models

# Create your models here.
import hashlib

class MonitoredFile(models.Model):
    
        
    full_filename = models.TextField(max_length=None)
    
    filecontents = models.TextField()
    filehash = models.CharField(max_length=100)
    is_dirty = models.BooleanField(default=True)


    def ensure_up_to_date(self,):
        pass
        
    @classmethod
    def get_fs_hash(cls, full_filename):
        contents = str(open(full_filename).read())
        m = hashlib.new('sha1')
        m.update(contents)
        return m.hexdigest()
        

    @classmethod
    def create(cls, full_filename):
        print 'Creating:', full_filename
        contents = str(open(full_filename).read())
        m = hashlib.new('sha1')
        m.update(contents)
        return cls(full_filename=full_filename, 
                    filecontents=contents,
                    filehash=m.hexdigest(),
                    is_dirty = False,
                    )
    


class Profile(models.Model):
    name = models.TextField(max_length=None)
    files = models.ManyToManyField(MonitoredFile)
    
    
