from .models import User,Item
import json
from django.http import HttpResponse, JsonResponse
from django.core import serializers

def users(request):
    users_list= serializers.serialize('json',User.objects.order_by('-age')[:5])
    return HttpResponse(users_list,content_type='application/json')