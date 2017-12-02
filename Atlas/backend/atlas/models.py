from __future__ import unicode_literals

from django.db import models

# Create your models here.

class User(models.Model):
    username= models.CharField(max_length=200)
    age = models.IntegerField(default=0)
    def __str__(self):
        return self.username

class tag(models.Model):
    name = models.CharField(max_length=100)

class image_media_item(models.Model):
    url = models.URLField()
    created_time = models.DateField(auto_now_add =True)
    updated_time = models.DateField(auto_now =True)
    main = models.BooleanField(default=False)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage',on_delete=models.CASCADE,null=True)

class video_media_item(models.Model):
    url = models.URLField()
    created_time = models.DateField(auto_now_add=True)
    updated_time = models.DateField(auto_now=True)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)

class sound_media_item(models.Model):
    url = models.URLField()
    created_time = models.DateField(auto_now_add=True)
    updated_time = models.DateField(auto_now=True)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)

class favorite_items(models.Model):
    user = models.ForeignKey('authentication.Account',on_delete=models.CASCADE,null=True)
    item = models.ForeignKey('Cultural_Heritage',on_delete=models.CASCADE,null=True)

class Cultural_Heritage(models.Model):
    user = models.ForeignKey('authentication.Account',on_delete =models.PROTECT)
    title = models.CharField(max_length = 200)
    description = models.TextField(blank=True)
    continent = models.CharField(max_length = 200)
    country = models.CharField(max_length = 200)
    city = models.CharField(max_length = 200)
    public_accessibility = models.BooleanField(default=True)
    created_time = models.DateField(auto_now_add =True)
    updated_time = models.DateField(auto_now =True)
    tags = models.ManyToManyField('tag',blank=True)
    favorited_amount = models.IntegerField(default=0,editable=False)
    longitude =models.DecimalField(max_digits=9, decimal_places=6,null=True)
    latitude =models.DecimalField(max_digits=9, decimal_places=6,null=True)
    start_year = models.IntegerField(null=True)
    end_year = models.IntegerField(null=True)
    place_name = models.CharField(max_length=350,null=True)

    @property
    def is_favorite(self):
        if self.user:
            return favorite_items.objects.filter(item=self.pk,user=self.user.pk).count() > 0
        return False

class comment(models.Model):
    user = models.ForeignKey('authentication.Account', on_delete=models.CASCADE)
    text = models.TextField(blank=False)
    created_time = models.DateTimeField(auto_now_add =True)
    updated_time = models.DateTimeField(auto_now =True)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)




    @property
    def user_info(self):
        user= {}
        user['username']=self.user.username
        user['picture']=self.user.profile_picture
        return user


