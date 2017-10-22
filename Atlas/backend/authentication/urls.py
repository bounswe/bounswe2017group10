from django.conf.urls import include, url
from .views import AuthRegister,user
from rest_framework_jwt.views import  refresh_jwt_token, verify_jwt_token,ObtainJSONWebToken
from .serializers import CustomJWTSerializer

urlpatterns = [
    url(r'^login/?$', ObtainJSONWebToken.as_view(serializer_class=CustomJWTSerializer)),
    url(r'^token-refresh/', refresh_jwt_token),
    url(r'^token-verify/', verify_jwt_token),
    url(r'^signup/?$', AuthRegister.as_view()),
    url(r'^me/?$', user),
]
