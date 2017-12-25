from __future__ import unicode_literals
from django.db import models

class annotation(models.Model):
    context = models.CharField(max_length=100)
    IRI = models.URLField(null=True, max_length=200)
    motivation = models.CharField(max_length=100)
    creator = models.URLField(null=True)


class body(models.Model):
    annotation = models.ForeignKey('annotation', on_delete=models.CASCADE, null=True)
    IRI = models.URLField(null=True, max_length=200)
    type = models.CharField(max_length=50, null=True)
    text = models.CharField(max_length=200, null=True)

    @property
    def info(self):
        info={}
        info['type'] = self.type
        if(self.type == 'image'):
            info['IRI'] = self.IRI
        elif(self.type == 'text'):
            info['textualBody'] = self.text
            pass
        return info

class target(models.Model):
    annotation = models.ForeignKey('annotation', on_delete=models.CASCADE, null=True)
    type = models.CharField(max_length=100, null = True)
    IRI = models.URLField(null=False, max_length=200)
    x = models.IntegerField(default=0)
    y = models.IntegerField(default=0)
    start = models.IntegerField(default=0)
    end = models.IntegerField(default=0)

    @property
    def selector(self):
        info = {}
        if(self.type == 'image'):
            info['type'] = 'imageSelector'
            info['x'] = self.x
            info['y'] = self.y
        elif(self.type == 'text'):
            info['type'] = 'textSelector'
            info['start'] = self.start
            info['end'] = self.end
        return info



