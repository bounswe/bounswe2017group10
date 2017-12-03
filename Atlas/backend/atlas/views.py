from .models import User,Cultural_Heritage,comment,tag,favorite_items,item_visit,image_media_item as image_item,hidden_tag
from rest_framework import generics,mixins
from django.http import HttpResponse, JsonResponse,HttpRequest
from django.core import serializers
from .serializers import cultural_heritage_serializer,image_media_item_serializer,tag_serializer,comment_serializer,favorite_item_serializer,item_visit_serializer
from django.shortcuts import get_object_or_404
from rest_framework.response import Response
from rest_framework import status
from rest_framework.pagination import LimitOffsetPagination
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from jwt_auth.compat import json
from django.contrib.postgres.search import SearchVector
from .util import hidden_tag_extractor

import geopy.distance




def users(request):
    users_list= serializers.serialize('json',User.objects.order_by('-age')[:5])
    return HttpResponse(users_list,content_type='application/json')

class cultural_heritage_item(generics.ListCreateAPIView):

    queryset = Cultural_Heritage.objects.get_queryset().order_by('id')
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    pagination_class = LimitOffsetPagination

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
    def update(self, request, *args, **kwargs):
        partial = kwargs.pop('partial', False)
        instance = self.get_object()
        serializer = self.get_serializer(instance, data=request.data, partial=partial)
        serializer.is_valid(raise_exception=True)
        data = request.data
        if 'description' in data:
            instance.hidden_tags = []
            description = data['description']
            hidden_tags = hidden_tag_extractor.extract_keywords(hidden_tag_extractor, text=description)
            for tag in hidden_tags:
                new_tag, created = hidden_tag.objects.get_or_create(name=tag)
                instance.hidden_tags.add(new_tag)
            instance.save()

        self.perform_update(serializer)
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
        #self.keywords = hidden_tag_extractor.extract_keywords(hidden_tag_extractor,query)
        self.keywords = query.split()
        objects = Cultural_Heritage.objects.all()
        objects_with_score = []
        for object in objects:
            objects_with_score.append((self.cmp(object),object))
        objects_with_score =sorted(objects_with_score,key=lambda x : x[0])
        objects_with_positive_score = []
        for i in range(0,len(objects_with_score)):
            score,_ = objects_with_score[i]
            if score>0:
                objects_with_positive_score = objects_with_score[i:]
                break
        objects = []
        for object in objects_with_positive_score:
            _,item  = object
            objects.append(item)
        return objects
    def cmp(self,item):
        keywords = self.keywords
        hidden_tags =item.hidden_tags.all()
        common_hidden_tag_amount = 0
        common_words_in_title_amount = 0
        common_tag_amount = 0
        for keyword in keywords:
            for hidden_tag in hidden_tags:
                if keyword.lower() == hidden_tag.name.lower():
                    common_hidden_tag_amount += 1
            for word in item.__dict__['title'].split():
                if keyword.lower() == word.lower():
                    print(keyword," ",word)
                    common_words_in_title_amount += 1
            for tag in item.tags.all():
                if keyword.lower() == tag.name.lower():
                    common_tag_amount += 1
        return common_tag_amount+common_hidden_tag_amount+common_words_in_title_amount


class cultural_heritage_item_search_autocorrect(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]
    def get_queryset(self):
        query = self.kwargs.get('query')
        return Cultural_Heritage.objects.filter(title__icontains=query)

class item_visit_update(generics.UpdateAPIView):
    serializer_class =  item_visit_serializer
    queryset = item_visit.objects.all()
    def update(self, request, *args, **kwargs):
        request.data['user'] =request.user.pk
        item = get_object_or_404(Cultural_Heritage,pk=request.data['cultural_heritage_item'])
        previous_duration = 0
        if item_visit.objects.filter(user=request.user,cultural_heritage_item=item,).count()>0:
            instance = item_visit.objects.get(user=request.user,cultural_heritage_item=item)
            previous_duration = instance.__dict__['duration']
        request.data['duration'] = request.data['duration']+previous_duration
        serializer = self.get_serializer(data=request.data,partial=True)
        serializer.is_valid(raise_exception=True)
        self.perform_update(serializer)
        return Response(serializer.data)

class nearby_search(generics.ListAPIView):
    serializer_class =  cultural_heritage_serializer
    def get_queryset(self):
        longitude = float(self.request.GET['longitude'])
        latitude = float(self.request.GET['latitude'])
        self.baseCoor = (longitude,latitude)
        objects = Cultural_Heritage.objects.all().exclude(longitude=None).exclude(latitude=None)
        return sorted(objects,key= self.dist)


    def dist(self,item):
        coord = (item.longitude,item.latitude )
        return geopy.distance.vincenty(self.baseCoor,coord).km

