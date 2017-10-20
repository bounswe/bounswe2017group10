from rest_framework import serializers
from .models import Cultural_Heritage,image_media_item
from authentication.serializers import AccountSerializer



class image_media_item_serializer(serializers.ModelSerializer):

    class Meta:
        model = image_media_item
        fields = '__all__'


class cultural_heritage_serializer(serializers.ModelSerializer):
    country = serializers.CharField(required=False)
    city= serializers.CharField(required=False)
    continent= serializers.CharField(required=False)
    user = AccountSerializer
    images = image_media_item_serializer(source='image_media_item_set',read_only=True,many=True)

    class Meta:
        model = Cultural_Heritage
        fields = '__all__'