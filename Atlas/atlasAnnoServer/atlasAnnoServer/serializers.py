from rest_framework import serializers
from rest_framework.utils import model_meta
from .models import annotation, target, body

class body_serializer(serializers.ModelSerializer):
    class Meta:
        model = target
        field = '__all__'

class target_serializer(serializers.ModelSerializer):
    class Meta:
        model = target
        #selector = serializers.ReadOnlyField()
        #fields = ['type', 'IRI', 'selector']
        fields = '__all__'

class target_return_serializer(serializers.ModelSerializer):
    class Meta:
        model = target
        selector = serializers.ReadOnlyField()
        fields = ['type', 'IRI', 'selector']

class annotation_serializer(serializers.ModelSerializer):
    #images = image_media_item_serializer(source='image_media_item_set', read_only=True, many=True)
    context = serializers.CharField(required=False)
    IRI = serializers.URLField(required=False)
    motivation = serializers.CharField(required=False)

    creator = serializers.URLField(required=False)
    target = target_return_serializer(source= 'target_set',read_only=True, many=True)

    class Meta:
        model = annotation
        fields = ['context', 'motivation', 'creator', 'motivation', 'IRI','target']

    def create(self, validated_data):

        anno = annotation.objects.create(**validated_data)
        return anno

