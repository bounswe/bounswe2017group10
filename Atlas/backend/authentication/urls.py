from django.conf.urls import url
from rest_framework_jwt.views import refresh_jwt_token, verify_jwt_token, ObtainJSONWebToken
from .serializers import CustomJWTSerializer
from .views import AuthRegister, user, AccountUpdate

urlpatterns = [
    url(r'^login/?$', ObtainJSONWebToken.as_view(serializer_class=CustomJWTSerializer)),
    url(r'^token-refresh/', refresh_jwt_token),
    url(r'^token-verify/', verify_jwt_token),
    url(r'^signup/?$', AuthRegister.as_view()),
    url(r'^me/?$', user),
    url(r'^me/(?P<username>\w+)/?$', AccountUpdate.as_view()),
]
