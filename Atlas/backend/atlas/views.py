from .models import User,Cultural_Heritage
from rest_framework import generics
from django.http import HttpResponse, JsonResponse
from django.core import serializers
from .serializers import cultural_heritage_serializer



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