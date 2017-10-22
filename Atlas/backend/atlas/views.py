from .models import User,Cultural_Heritage,image_media_item as image_item
from rest_framework import generics
from django.http import HttpResponse, JsonResponse
from django.core import serializers
from .serializers import cultural_heritage_serializer,image_media_item_serializer
from django.shortcuts import get_object_or_404
from rest_framework.response import Response
from rest_framework import status
from jwt_auth.compat import json
from rest_framework.test import APIRequestFactory


def users(request):
    users_list= serializers.serialize('json',User.objects.order_by('-age')[:5])
    return HttpResponse(users_list,content_type='application/json')

class cultural_heritage_item(generics.ListCreateAPIView):

    queryset = Cultural_Heritage.objects.all()
    serializer_class = cultural_heritage_serializer

    def perform_create(self,serializer):
        serializer.save()


    def create(self, request, *args, **kwargs):
        #Get the user from the request so that we can add it to cultural heritage item model.
        #Pk is the id of the user.
        request.data['user'] = request.user.pk

        return super(cultural_heritage_item, self).create(request,*args,**kwargs)


class ImageInterceptorMixin(object):
    @property
    def current_heritage_item(self):
        cultural_heritage_id = self.kwargs.get('heritage_id')
        return get_object_or_404(Cultural_Heritage,pk=cultural_heritage_id)


class image_media_item(ImageInterceptorMixin,generics.CreateAPIView):
    queryset = image_item.objects.all()
    serializer_class = image_media_item_serializer
    def create(self, request, *args, **kwargs):
        try:
            for image_item_data in request.data['images']:
                 request.data['cultural_heritage_item'] = self.current_heritage_item.pk
                 for k,v in image_item_data.items():
                     request.data[k] = v
                 result =super(image_media_item, self).create(request, *args, **kwargs)
            return result
        except:
           return Response("images must be a list.", status=status.HTTP_400_BAD_REQUEST)

class cultural_heritage_item_view_update_delete(generics.RetrieveUpdateDestroyAPIView):
    serializer_class = cultural_heritage_serializer
    lookup_field = 'id'
    def get_queryset(self):
        return Cultural_Heritage.objects.filter()
