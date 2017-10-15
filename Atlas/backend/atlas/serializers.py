from rest_framework import serializers
from .models import Cultural_Heritage
from authentication.serializers import AccountSerializer

class cultural_heritage_serializer(serializers.ModelSerializer):
    #user = serializers.PrimaryKeyRelatedField(required=False,read_only=True)
    #user = AccountSerializer(many=False,read_only=True)
    class Meta:
        model = Cultural_Heritage
        fields = ('title','public_accessibility')

    def validate(self, data):
        print ("ASDDASDASASADAS")
        title = data['title']
        # Ensure title exists
        if not title:
            raise serializers.ValidationError('Cultural heritage item must have a title.')

        return data