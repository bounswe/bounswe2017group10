"""atlas URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/1.11/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  url(r'^$', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  url(r'^$', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.conf.urls import url, include
    2. Add a URL to urlpatterns:  url(r'^blog/', include('blog.urls'))
"""
from django.conf.urls import url,include
from django.contrib import admin
from rest_framework_swagger.views import get_swagger_view
from django.contrib.staticfiles.urls import staticfiles_urlpatterns

from. import views

schema_view = get_swagger_view(title='ATLAS API DOCUMENTS')

urlpatterns = [

    url(r'^swagger/', schema_view),
    url(r'^admin/', admin.site.urls),
    url(r'^users/', views.users, name='users'),
    url(r'^api/auth/', include('authentication.urls')),
    url(r'^cultural_heritage_item/?$', views.cultural_heritage_item.as_view()),
    url(r'^cultural_heritage_item/(?P<heritage_id>[0-9]+)/image/?$', views.image_media_item.as_view()),
    url(r'^cultural_heritage_item/(?P<id>\d+)/?$', views.cultural_heritage_item_view_update_delete.as_view()),
    url(r'^cultural_heritage_item/myitems', views.cultural_heritage_item_list_user_items.as_view()),
    url(r'^cultural_heritage_item/(?P<heritage_id>[0-9]+)/comment/?$', views.cultural_heritage_item_comment.as_view()),

]

urlpatterns += staticfiles_urlpatterns()
