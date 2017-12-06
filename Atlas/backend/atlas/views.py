import geopy.distance
from django.shortcuts import get_object_or_404
from jwt_auth.compat import json
from rest_framework import generics, mixins
from rest_framework import status
from rest_framework.pagination import LimitOffsetPagination
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from rest_framework.response import Response
from .constants import *
from .models import Cultural_Heritage, comment, tag, favorite_items, image_media_item as image_item, \
    hidden_tag
from .popularity import *
from .serializers import cultural_heritage_serializer, image_media_item_serializer, tag_serializer, comment_serializer, \
    favorite_item_serializer, item_visit_serializer
from .util import hidden_tag_extractor


class cultural_heritage_item(generics.ListCreateAPIView):
    queryset = Cultural_Heritage.objects.get_queryset().order_by('id')
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    pagination_class = LimitOffsetPagination

    def perform_create(self, serializer):
        serializer.save()

    def get_queryset(self):
        return Cultural_Heritage.objects.get_queryset().order_by('id')

    def create(self, request, *args, **kwargs):
        # Get the user from the request so that we can add it to cultural heritage item model.
        # Pk is the id of the user.
        request.data['user'] = request.user.pk
        images = []
        if 'images' in request.data:
            images = request.data['images']

        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)
        if len(images) > 0:
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
        return get_object_or_404(Cultural_Heritage, pk=cultural_heritage_id)


class cultural_heritage_item_comment(HeritageIdInterceptorMixin, generics.CreateAPIView):
    queryset = comment.objects.all()
    serializer_class = comment_serializer

    def create(self, request, *args, **kwargs):
        try:
            comment_data = request.data['comment']
            request.data['cultural_heritage_item'] = self.current_heritage_item.pk
            for k, v in comment_data.items():
                request.data[k] = v
            request.data['user'] = request.user.pk
            result = super(cultural_heritage_item_comment, self).create(request, *args, **kwargs)
            return result
        except BaseException as e:
            return Response(json.dumps(str(e)), status=status.HTTP_400_BAD_REQUEST)


class user_favorite_item(HeritageIdInterceptorMixin, generics.CreateAPIView, mixins.DestroyModelMixin):
    queryset = favorite_items.objects.all()
    serializer_class = favorite_item_serializer

    def create(self, request, *args, **kwargs):
        try:
            item = Cultural_Heritage.objects.get(id=self.current_heritage_item.pk)
            user = request.user
            request.data['item'] = item.pk
            request.data['user'] = user.pk
            if favorite_items.objects.filter(item=item.pk, user=user.pk).count() > 0:
                return Response(status=status.HTTP_400_BAD_REQUEST)
            result = super(user_favorite_item, self).create(request, *args, **kwargs)
            item.favorited_amount = favorite_items.objects.filter(item=item.pk).count()
            item.save()
            return result
        except BaseException as e:
            return Response(json.dumps(str(e)), status=status.HTTP_400_BAD_REQUEST)

    def delete(self, request, *args, **kwargs):
        item = Cultural_Heritage.objects.get(id=self.current_heritage_item.pk)
        user = request.user
        request.data['item'] = item.pk
        request.data['user'] = user.pk
        result = self.destroy(request, *args, **kwargs)
        item.favorited_amount = favorite_items.objects.filter(item=item.pk).count()
        item.save()
        return result

    def destroy(self, request, *args, **kwargs):
        instance = get_object_or_404(self.queryset, item=request.data['item'], user=request.data['user'])
        self.perform_destroy(instance)

        return Response(status=status.HTTP_204_NO_CONTENT)


class get_user_favorite_items(HeritageIdInterceptorMixin, generics.ListAPIView):
    queryset = favorite_items.objects.all()
    serializer_class = cultural_heritage_serializer

    def get_queryset(self):
        items = favorite_items.objects.filter(user=self.request.user.pk)
        return [Cultural_Heritage.objects.get(pk=cur.item.pk) for cur in items]


class image_media_item(HeritageIdInterceptorMixin, generics.CreateAPIView):
    queryset = image_item.objects.all()
    serializer_class = image_media_item_serializer

    def create(self, request, *args, **kwargs):
        try:
            for image_item_data in request.data['images']:
                request.data['cultural_heritage_item'] = self.current_heritage_item.pk
                for k, v in image_item_data.items():
                    request.data[k] = v
                result = super(image_media_item, self).create(request, *args, **kwargs)
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
            extractor = hidden_tag_extractor()
            hidden_tags = extractor.extract_keywords(text=description)
            for tag in hidden_tags:
                if len(tag) > MAX_HIDDEN_TAG_SIZE:
                    continue
                new_tag, created = hidden_tag.objects.get_or_create(name=tag)
                instance.hidden_tags.add(new_tag)
            instance.save()

        self.perform_update(serializer)
        return Response(serializer.data)

    def get_queryset(self):
        return Cultural_Heritage.objects.filter()


class tags(generics.ListAPIView):
    serializer_class = tag_serializer
    pagination_class = None

    def get_queryset(self):
        return tag.objects.get_queryset().order_by('id')


class cultural_heritage_item_list_user_items(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer

    def get_queryset(self):
        user = self.request.user;
        return Cultural_Heritage.objects.filter(user=user)


class cultural_heritage_item_featured(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer

    def get_queryset(self):
        all_items = Cultural_Heritage.objects.all()
        items_with_score = [(item, popularity_score(item)) for item in all_items]
        sorted_items_with_score = sorted(items_with_score, key=lambda x: x[1], reverse=True)
        return [pair[0] for pair in sorted_items_with_score]


class cultural_heritage_item_search(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    def get_queryset(self):
        query = self.kwargs.get('query')
        self.keywords = query.split()
        objects = Cultural_Heritage.objects.all()
        self.location = None
        for object in objects:
            for keyword in self.keywords:
                if getattr(object, 'place_name') and keyword.lower() == getattr(object, 'place_name').lower():
                    if getattr(object, 'latitude') != None:
                        self.location = (getattr(object, 'longitude'), getattr(object, 'latitude'))
                        break
            if self.location != None:
                break

        objects_with_score = [(self.cmp(obj), obj) for obj in objects]
        objects_with_score = sorted(objects_with_score, key=lambda x: x[0], reverse=True)
        objects_with_positive_score = filter(lambda x: x[0] > 0, objects_with_score)
        objects = [pair[1] for pair in objects_with_positive_score]
        return objects

    def cmp(self, item):
        keywords = self.keywords
        hidden_tags = item.hidden_tags.all()
        common_hidden_tag_amount = 0
        common_words_in_title_amount = 0
        common_tag_amount = 0
        location_score = 0
        for keyword in keywords:
            matching_hidden_tags = (hidden_tag for hidden_tag in hidden_tags if
                                    keyword.lower() == hidden_tag.name.lower())
            common_hidden_tag_amount += sum(1 for _ in matching_hidden_tags)
            matching_title_words = (word for word in getattr(item, 'title').split() if keyword.lower() == word.lower())
            common_words_in_title_amount += sum(1 for _ in matching_title_words)
            matching_tags = (tag for tag in item.tags.all() if keyword.lower() == tag.name.lower())
            common_tag_amount += sum(1 for _ in matching_tags)
        if self.location != None and getattr(item, 'latitude') != None:
            coord = (item.longitude, item.latitude)
            location_distance_in_km = geopy.distance.vincenty(self.location, coord).km
            location_score = 1 if location_distance_in_km == 0 else 1 / location_distance_in_km
        search_score = COEFF_COMMON_TAG_AMOUNT * common_tag_amount + COEFF_COMMON_HIDDEN_TAG_AMOUNT * common_hidden_tag_amount + \
                       COEFF_COMMON_WORDS_IN_TITLE_AMOUNT * common_words_in_title_amount + COEFF_LOCATION_SCORE * location_score
        if search_score >= SEARCH_THRESHOLD:
            search_score += COEFF_ADMIRATION_SCORE_FOR_SEARCH * admiration_score(item) + \
                            COEFF_COMPLETENESS_SCORE_FOR_SEARCH * completeness_score(item)
        return search_score


class cultural_heritage_item_search_autocorrect(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer
    permission_classes = [IsAuthenticatedOrReadOnly]

    def get_queryset(self):
        query = self.kwargs.get('query')
        return Cultural_Heritage.objects.filter(title__icontains=query)


class item_visit_update(generics.UpdateAPIView):
    serializer_class = item_visit_serializer
    queryset = item_visit.objects.all()

    def update(self, request, *args, **kwargs):
        request.data['user'] = request.user.pk
        serializer = self.get_serializer(data=request.data, partial=True)
        serializer.is_valid(raise_exception=True)
        if 'cultural_heritage_item' not in request.data:
            return Response({'error': 'cultural heritage item is required'}, status=status.HTTP_400_BAD_REQUEST)
        item = get_object_or_404(Cultural_Heritage, pk=request.data['cultural_heritage_item'])
        previous_duration = 0
        if item_visit.objects.filter(user=request.user, cultural_heritage_item=item, ).count() > 0:
            instance = item_visit.objects.get(user=request.user, cultural_heritage_item=item)
            previous_duration = instance.__dict__['duration']
        request.data['duration'] = request.data['duration'] + previous_duration
        serializer = self.get_serializer(data=request.data, partial=True)
        serializer.is_valid(raise_exception=True)
        self.perform_update(serializer)
        return Response(serializer.data)


class nearby_search(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer

    def get_queryset(self):
        longitude = float(self.request.GET['longitude'])
        latitude = float(self.request.GET['latitude'])
        self.baseCoor = (longitude, latitude)
        objects = Cultural_Heritage.objects.all().exclude(longitude=None).exclude(latitude=None)
        return sorted(objects, key=self.dist)

    def dist(self, item):
        coord = (item.longitude, item.latitude)
        return geopy.distance.vincenty(self.baseCoor, coord).km


class recommendation(generics.ListAPIView):
    serializer_class = cultural_heritage_serializer

    def get_queryset(self):
        item_id = self.request.GET['item_id']
        self.base_item = get_object_or_404(Cultural_Heritage, pk=item_id)
        objects = Cultural_Heritage.objects.exclude(pk=item_id)
        objects_with_score = [(self.cmp(obj), obj) for obj in objects]
        objects_with_score = sorted(objects_with_score, key=lambda x: x[0], reverse=True)
        objects = [pair[1] for pair in objects_with_score]
        return objects

    def cmp(self, item):
        hidden_tags = item.hidden_tags.all()
        common_hidden_tag_amount = 0
        common_words_in_title_amount = 0
        common_tag_amount = 0
        location_score = 0
        time_score = 0
        time_overlap_perc = 0
        for hidden_tag_1 in self.base_item.hidden_tags.all():
            matching_hidden_tags = (hidden_tag for hidden_tag in hidden_tags if
                                    hidden_tag_1.name.lower() == hidden_tag.name.lower())
            common_hidden_tag_amount += sum(1 for _ in matching_hidden_tags)
        for word_1 in self.base_item.title.split():
            matching_title_words = (word for word in item.title.split() if
                                    word_1.lower() == word.lower())
            common_words_in_title_amount += sum(1 for _ in matching_title_words)
        for tag_1 in self.base_item.tags.all():
            matching_tags = (tag for tag in item.tags.all() if tag_1.name.lower() == tag.name.lower())
            common_tag_amount += sum(1 for _ in matching_tags)

        if self.base_item.latitude != None:
            coord = (item.longitude, item.latitude)
            location = (self.base_item.longitude, self.base_item.latitude)
            location_distance_in_km = geopy.distance.vincenty(location, coord).km
            location_score = 1 if location_distance_in_km == 0 else 1 / location_distance_in_km
        if self.base_item.start_year != None and item.start_year != None:
            avg_time_1 = (self.base_item.start_year + self.base_item.end_year) / 2
            avg_time_2 = (item.start_year + item.end_year) / 2

            time_score = 1 / (abs(avg_time_1 - avg_time_2)) if avg_time_1 != avg_time_1 else 1

            # Calculate overlapping percentage
            left_boundary_of_overlapped = max(self.base_item.start_year, item.start_year)
            left_boundary_of_union = min(self.base_item.start_year, item.start_year)
            right_boundary_of_overlapped = min(self.base_item.end_year, item.end_year)
            right_boundary_of_union = max(self.base_item.end_year, item.end_year)
            time_overlap_perc = 1 * (right_boundary_of_overlapped - left_boundary_of_overlapped) / (
                right_boundary_of_union - left_boundary_of_union) if right_boundary_of_union != left_boundary_of_union else 1
        recommendation_score = COEFF_COMMON_TAG_AMOUNT * common_tag_amount + COEFF_COMMON_HIDDEN_TAG_AMOUNT * common_hidden_tag_amount + \
                               COEFF_COMMON_WORDS_IN_TITLE_AMOUNT * common_words_in_title_amount + \
                               COEFF_LOCATION_SCORE * location_score + COEFF_TIME_SCORE * time_score + COEFF_TIME_OVERLAPPED * time_overlap_perc
        if recommendation_score >= RECOMMENDATION_THRESHOLD:
            recommendation_score += COEFF_ADMIRATION_SCORE_FOR_RECOMMENDATION * admiration_score(item) + \
                                    COEFF_COMPLETENESS_SCORE_FOR_RECOMMENDATION * completeness_score(item)
        return recommendation_score
