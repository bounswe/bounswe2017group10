from __future__ import absolute_import, unicode_literals
from .celery import app
from .util import hidden_tag_extractor
from rest_framework import serializers
from rest_framework.utils import  model_meta
from .models import Cultural_Heritage,hidden_tag


class hidden_tag_serializer(serializers.ModelSerializer):
    class Meta:
        model =  hidden_tag
        fields = '__all__'

@app.task
def add(x, y):
    return x + y

@app.task
def extract_hidden_tags(item_id):
    item= Cultural_Heritage.objects.get(pk=item_id)
    description = item.description
    hidden_tags = hidden_tag_extractor.extract_keywords(hidden_tag_extractor, text=description)
    for tag in hidden_tags:
        new_tag, created = hidden_tag.objects.get_or_create(name=tag)
        if created:
            item.hidden_tags.add(new_tag)
    item.save()
