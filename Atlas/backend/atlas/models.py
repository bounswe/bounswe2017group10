from __future__ import unicode_literals

from django.db import models


# Create your models here.

class User(models.Model):
    username= models.CharField(max_length=200)
    age = models.IntegerField(default=0)
    def __str__(self):
        return self.username


class Item(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    item_name = models.CharField(max_length=200)
