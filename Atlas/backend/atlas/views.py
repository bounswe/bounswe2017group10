from .models import User,Cultural_Heritage,comment,tag,favorite_items,image_media_item as image_item
from rest_framework import generics,mixins
from django.http import HttpResponse, JsonResponse,HttpRequest
from django.core import serializers
from .serializers import cultural_heritage_serializer,image_media_item_serializer,tag_serializer,comment_serializer,favorite_item_serializer
from django.shortcuts import get_object_or_404
from rest_framework.response import Response
from rest_framework import status
from rest_framework.pagination import LimitOffsetPagination
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from jwt_auth.compat import json
from django.contrib.postgres.search import SearchVector




def users(request):
    users_list= serializers.serialize('json',User.objects.order_by('-age')[:5])
    return HttpResponse(users_list,content_type='application/json')

class cultural_heritage_item(generics.ListCreateAPIView):

    queryset = Cultural_Heritage.objects.get_queryset().order_by('id')
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    pagination_class = LimitOffsetPagination

    def list(self, request, *args, **kwargs):
        queryset = self.filter_queryset(self.get_queryset())

        page = self.paginate_queryset(queryset)
        if page is not None:
            serializer = self.get_serializer(page, many=True)
            data = self.add_is_favorite_field(serializer, request.user)
            return self.get_paginated_response(data)

        serializer = self.get_serializer(queryset, many=True)
        data = self.add_is_favorite_field(serializer,request.user)
        return Response(data)
    def add_is_favorite_field(self,serializer,user):
        newData = serializer.data
        for i in range (0,len(newData)):
            item_id = newData[i]['id']
            if user and favorite_items.objects.filter(item=item_id, user=user.pk).count() > 0:
                newData[i]['is_favorite'] = True
        return newData
    def perform_create(self,serializer):
        serializer.save()
    def get_queryset(self):
        return Cultural_Heritage.objects.get_queryset().order_by('id')
    def create(self, request, *args, **kwargs):
        #Get the user from the request so that we can add it to cultural heritage item model.
        #Pk is the id of the user.
        request.data['user'] = request.user.pk
        images = []
        if 'images' in request.data:
            images = request.data['images']


        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)
        if len(images) > 0 :
            id = serializer.data['id']
            for image in images:
                image['cultural_heritage_item'] = id
                image_serializer = image_media_item_serializer(data=image)
                image_serializer.is_valid(raise_exception=True)
                image_serializer.save()
        headers = self.get_success_headers(serializer.data)
        return Response(serializer.data, status=status.HTTP_201_CREATED, headers=headers)


class HeritageIdInterceptorMixin(object):
    @property
    def current_heritage_item(self):
        cultural_heritage_id = self.kwargs.get('heritage_id')
        return get_object_or_404(Cultural_Heritage,pk=cultural_heritage_id)

class cultural_heritage_item_comment(HeritageIdInterceptorMixin, generics.CreateAPIView):
    queryset = comment.objects.all()
    serializer_class = comment_serializer
    def create(self, request, *args, **kwargs):
        try:
            comment_data = request.data['comment']
            request.data['cultural_heritage_item'] = self.current_heritage_item.pk
            for k, v in comment_data.items():
                request.data[k] = v
            request.data['user'] =request.user.pk
            result =super(cultural_heritage_item_comment, self).create(request, *args, **kwargs)
            return result
        except BaseException as e:
            return Response(json.dumps(str(e)), status=status.HTTP_400_BAD_REQUEST)
class user_favorite_item(HeritageIdInterceptorMixin,generics.CreateAPIView,mixins.DestroyModelMixin):
    queryset = favorite_items.objects.all()
    serializer_class =  favorite_item_serializer
    def create(self, request, *args, **kwargs):
        try:
            item= Cultural_Heritage.objects.get(id=self.current_heritage_item.pk)
            user =request.user
            request.data['item'] =item.pk
            request.data['user'] =user.pk
            if favorite_items.objects.filter(item=item.pk,user=user.pk).count()>0:
                return Response(status=status.HTTP_400_BAD_REQUEST)
            result =super(user_favorite_item, self).create(request,*args,**kwargs)
            item.favorited_amount=favorite_items.objects.filter(item=item.pk).count()
            item.save()
            return result
        except BaseException as e:
            return Response(json.dumps(str(e)), status=status.HTTP_400_BAD_REQUEST)

    def delete(self, request, *args, **kwargs):
        item = Cultural_Heritage.objects.get(id=self.current_heritage_item.pk)
        user = request.user
        request.data['item'] = item.pk
        request.data['user'] = user.pk
        result =self.destroy(request, *args, **kwargs)
        item.favorited_amount = favorite_items.objects.filter(item=item.pk).count()
        item.save()
        return result
    def destroy(self, request, *args, **kwargs):
        instance = get_object_or_404(self.queryset,item=request.data['item'],user=request.data['user'])
        self.perform_destroy(instance)

        return Response(status=status.HTTP_204_NO_CONTENT)
class get_user_favorite_items(HeritageIdInterceptorMixin,generics.ListAPIView):
    queryset = favorite_items.objects.all()
    serializer_class =  favorite_item_serializer
    def get_queryset(self):
        return favorite_items.objects.filter(user=self.request.user.pk)


class image_media_item(HeritageIdInterceptorMixin, generics.CreateAPIView):
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
        except BaseException as e:
            return Response(json.dumps(str(e)), status=status.HTTP_400_BAD_REQUEST)

class cultural_heritage_item_view_update_delete(generics.RetrieveUpdateDestroyAPIView):
    serializer_class = cultural_heritage_serializer
    lookup_field = 'id'
    permission_classes = [IsAuthenticatedOrReadOnly]
    def retrieve(self, request, *args, **kwargs):
        instance = self.get_object()
        serializer = self.get_serializer(instance)
        item_id = self.kwargs.get('id')
        if request.user and favorite_items.objects.filter(item=item_id,user=request.user.pk).count() > 0 :
         new_dict = serializer.data
         new_dict['is_favorite'] = True
         return Response(new_dict)
        return Response(serializer.data)
    def get_queryset(self):
        return Cultural_Heritage.objects.filter()

class tags(generics.ListAPIView):
    serializer_class = tag_serializer
    pagination_class =  None
    def get_queryset(self):
        return tag.objects.get_queryset().order_by('id')


class cultural_heritage_item_list_user_items(generics.ListAPIView):

    serializer_class = cultural_heritage_serializer


    def get_queryset(self):
        user=self.request.user;
        return Cultural_Heritage.objects.filter(user=user)

class cultural_heritage_item_search(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]
    def get_queryset(self):
        query = self.kwargs.get('query')
        return Cultural_Heritage.objects.annotate(
           search=SearchVector('tags__name', 'title','description'),
        ).filter(search=query)

class cultural_heritage_item_search_autocorrect(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]
    def get_queryset(self):
        query = self.kwargs.get('query')
        return Cultural_Heritage.objects.filter(title__icontains=query)

