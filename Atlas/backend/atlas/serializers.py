from rest_framework import serializers
from .models import Cultural_Heritage
from authentication.serializers import AccountSerializer

class cultural_heritage_serializer(serializers.ModelSerializer):
    country = serializers.CharField(required=False)
    city= serializers.CharField(required=False)
    continent= serializers.CharField(required=False)
    user = AccountSerializer
    class Meta:
        model = Cultural_Heritage
