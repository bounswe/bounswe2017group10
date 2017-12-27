from authentication.serializers import AccountSerializer

from rest_framework import serializers
from rest_framework.utils import model_meta
from .constants import *
from .models import Cultural_Heritage, comment as comment_model, image_media_item, item_visit, favorite_items, \
    tag as tag_model, hidden_tag, tag_user_score, hidden_tag_user_score
from .util import hidden_tag_extractor


class image_media_item_serializer(serializers.ModelSerializer):
    class Meta:
        model = image_media_item
        fields = '__all__'


class tag_serializer(serializers.ModelSerializer):
    class Meta:
        model = tag_model
        fields = '__all__'


class hidden_tag_serializer(serializers.ModelSerializer):
    class Meta:
        model = hidden_tag
        fields = '__all__'


class comment_serializer(serializers.ModelSerializer):
    text = serializers.CharField(required=True)
    user_info = serializers.ReadOnlyField()

    class Meta:
        model = comment_model
        fields = ('id', 'user', 'text', 'created_time', 'updated_time', 'cultural_heritage_item', 'user_info')


class cultural_heritage_serializer(serializers.ModelSerializer):
    country = serializers.CharField(required=False)
    city = serializers.CharField(required=False)
    continent = serializers.CharField(required=False)
    user = AccountSerializer
    images = image_media_item_serializer(source='image_media_item_set', read_only=True, many=True)
    comments = comment_serializer(source='comment_set', read_only=True, many=True)
    tags = tag_serializer(many=True, required=False)
    is_favorite = serializers.SerializerMethodField()
    user_info = serializers.ReadOnlyField()

    class Meta:
        model = Cultural_Heritage
        fields = ['user', 'title', 'description', 'continent', 'country', 'city', 'public_accessibility',
                  'created_time',
                  'updated_time', 'tags', 'favorited_amount', 'longitude', 'latitude', 'start_year', 'end_year',
                  'place_name',
                  'is_favorite', 'images', 'comments', 'id', 'user_info']

    def get_is_favorite(self, obj):
        user = None
        request = self.context.get("request")
        if request and hasattr(request, "user"):
            user = request.user
        if user:
            return favorite_items.objects.filter(item=obj, user=user).count() > 0
        return False

    def create(self, validated_data):

        tags = []
        if 'tags' in validated_data.keys():
            tags = validated_data.pop('tags')
        heritage_item = Cultural_Heritage.objects.create(**validated_data)
        if 'description' in validated_data:
            description = validated_data['description']
            extractor = hidden_tag_extractor()
            hidden_tags = extractor.extract_keywords(text=description)
            for tag in hidden_tags:
                if len(tag) > MAX_HIDDEN_TAG_SIZE:
                    continue
                new_tag, created = hidden_tag.objects.get_or_create(name=tag)
                if created:
                    heritage_item.hidden_tags.add(new_tag)

        if len(tags) > 0:
            for tag in tags:
                new_tag, created = tag_model.objects.get_or_create(name=tag["name"])
                heritage_item.tags.add(new_tag)
        return heritage_item

    def update(self, instance, validated_data):
        info = model_meta.get_field_info(instance)
        if validated_data.get('tags'):
            tags = validated_data.pop('tags')
            instance.tags = []
            if len(tags) > 0:
                for tag in tags:
                    new_tag, created = tag_model.objects.get_or_create(name=tag["name"])
                    instance.tags.add(new_tag)
                instance.save()
        # We shouldn't use this endpoint to update images.
        # This popping is just to make sure we dont get any internal errors
        # when clients sends images in update.
        if validated_data.get('images'):
            validated_data.pop('images')
        # We shouldn't use this endpoint to update comments.
        # This popping is just to make sure we dont get any internal errors
        # when clients sends comments in update.
        if validated_data.get('comments'):
            validated_data.pop('comments')
        # Simply set each attribute on the instance, and then save it.
        # Note that unlike `.create()` we don't need to treat many-to-many
        # relationships as being a special case. During updates we already
        # have an instance pk for the relationships to be associated with.
        for attr, value in validated_data.items():
            if attr in info.relations and info.relations[attr].to_many:
                field = getattr(instance, attr)
                field.set(value)
            else:
                setattr(instance, attr, value)
        instance.save()

        return instance


class favorite_item_list_serializer(serializers.ModelSerializer):
    cultural_heritage_item = cultural_heritage_serializer(source='item')

    class Meta:
        model = favorite_items
        fields = ['cultural_heritage_item', 'user', 'id']


class favorite_item_serializer(serializers.ModelSerializer):
    class Meta:
        model = favorite_items
        fields = ['item', 'user', 'id']


class item_visit_serializer(serializers.ModelSerializer):
    class Meta:
        model = item_visit
        fields = '__all__'

class tag_user_score_serializer(serializers.ModelSerializer):
    user = serializers.IntegerField(required=True)
    class Meta:
        model = tag_user_score
        fields = '__all__'

class hidden_tag_user_score_serializer(serializers.ModelSerializer):
    user = serializers.IntegerField(required=True)
    class Meta:
        model = hidden_tag_user_score
        fields = '__all__'