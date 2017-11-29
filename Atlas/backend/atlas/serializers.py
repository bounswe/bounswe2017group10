from rest_framework import serializers
from .models import Cultural_Heritage,comment as comment_model,image_media_item,favorite_items,tag as tag_model
from authentication.serializers import AccountSerializer



class image_media_item_serializer(serializers.ModelSerializer):

    class Meta:
        model = image_media_item
        fields = '__all__'
class tag_serializer(serializers.ModelSerializer):
    class Meta:
        model = tag_model
        fields = '__all__'

class comment_serializer(serializers.ModelSerializer):
    text = serializers.CharField(required=True)
    class Meta:
        model = comment_model
        fields = '__all__'

class cultural_heritage_serializer(serializers.ModelSerializer):
    country = serializers.CharField(required=False)
    city= serializers.CharField(required=False)
    continent= serializers.CharField(required=False)
    user = AccountSerializer
    images = image_media_item_serializer(source='image_media_item_set',read_only=True,many=True)
    comments = comment_serializer(source='comment_set', read_only=True, many=True)
    tags  = tag_serializer(many=True,required=False)
    class Meta:
        model = Cultural_Heritage
        fields = '__all__'
    def create(self, validated_data):
         if 'tags' in validated_data.keys():
            tags = validated_data.pop('tags')
            heritage_item = Cultural_Heritage.objects.create(**validated_data)
            if len(tags) > 0:
                for tag in tags:
                    new_tag,created = tag_model.objects.get_or_create(name=tag["name"])
                    heritage_item.tags.add(new_tag)
                heritage_item.save()
            return heritage_item
         return super(cultural_heritage_serializer, self).create(validated_data)

class favorite_item_serializer(serializers.ModelSerializer):
    class Meta:
        model = favorite_items
        fields = '__all__'