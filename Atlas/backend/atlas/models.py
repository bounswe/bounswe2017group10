from __future__ import unicode_literals
from .constants import *
from django.db import models


# Create your models here.

class tag(models.Model):
    name = models.CharField(max_length=MAX_TAG_SIZE)


class image_media_item(models.Model):
    url = models.URLField()
    created_time = models.DateField(auto_now_add=True)
    updated_time = models.DateField(auto_now=True)
    main = models.BooleanField(default=False)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)


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
    user = models.ForeignKey('authentication.Account', on_delete=models.CASCADE, null=True)
    item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)


class Cultural_Heritage(models.Model):
    user = models.ForeignKey('authentication.Account', on_delete=models.PROTECT)
    title = models.CharField(max_length=MAX_TITLE_SIZE)
    description = models.TextField(blank=True)
    continent = models.CharField(max_length=MAX_CONTINENT_SIZE)
    country = models.CharField(max_length=MAX_COUNTRY_SIZE)
    city = models.CharField(max_length=MAX_CITY_SIZE)
    public_accessibility = models.BooleanField(default=True)
    created_time = models.DateField(auto_now_add=True)
    updated_time = models.DateField(auto_now=True)
    tags = models.ManyToManyField('tag', blank=True)
    favorited_amount = models.IntegerField(default=0, editable=False)
    longitude = models.DecimalField(max_digits=MAX_lOCATION_DIGITS, decimal_places=LOCATION_DECIMAL_PLACES, null=True)
    latitude = models.DecimalField(max_digits=MAX_lOCATION_DIGITS, decimal_places=LOCATION_DECIMAL_PLACES, null=True)
    start_year = models.IntegerField(null=True)
    end_year = models.IntegerField(null=True)
    place_name = models.CharField(max_length=MAX_PLACE_NAME_SIZE, null=True)
    hidden_tags = models.ManyToManyField('hidden_tag', blank=True)

    @property
    def user_info(self):
        user = {}
        user['username'] = self.user.username
        user['profile_picture'] = self.user.profile_picture
        return user

class comment(models.Model):
    user = models.ForeignKey('authentication.Account', on_delete=models.CASCADE)
    text = models.TextField(blank=False)
    created_time = models.DateTimeField(auto_now_add=True)
    updated_time = models.DateTimeField(auto_now=True)
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)

    @property
    def user_info(self):
        user = {}
        user['username'] = self.user.username
        user['picture'] = self.user.profile_picture
        return user


class hidden_tag(models.Model):
    name = models.CharField(max_length=MAX_HIDDEN_TAG_SIZE)


class item_visit(models.Model):
    cultural_heritage_item = models.ForeignKey('Cultural_Heritage', on_delete=models.CASCADE, null=True)
    user = models.ForeignKey('authentication.Account', on_delete=models.CASCADE)
    duration = models.IntegerField(default=0)  # This will be stored in seconds.
    last_updated_time = models.DateTimeField(auto_now=True)
