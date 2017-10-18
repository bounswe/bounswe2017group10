import pytest
from django.test import TestCase
from django.test.client import Client

from jwt_auth import utils
from jwt_auth.compat import json, smart_text
from authentication.models import Account
from rest_framework.test import APIClient


#response_content = json.loads(smart_text(response.content))
#print(response_content)
@pytest.mark.django_db
class cultural_heritage_item(TestCase):
    def setUp(self):
        self.email = 'talhatest1@gmail.com'
        self.username = 'heisenberg12'
        self.password = 'passworD1ss'
        self.login_url = '/api/auth/login/'
        self.sigun_url = '/api/auth/signup/'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.data = {
            'username': self.username,
            'email': self.email,
            'password': self.password,
            'confirm_password': self.password,
        }
        self.client = APIClient()
        self.client.login(username=self.username,password=self.password)
    def test_create_cultural_heritage_item(self):

        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format ='json',

        )
        self.assertEqual(response.status_code, 201)


    def test_create_cultural_heritage_item_with_missing_title(self):
        item_data = {
           'country' : 'America'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        #Since title is missing it should return 400
        self.assertEqual(response.status_code, 400)

    def test_create_cultural_heritage_item_with_all_fields(self):
        item_data = {
            'country': 'America',
            'title' : 'Seattle Discovery Park',
            'description' : 'This park is really awesome. You can play frisbee with your dog on very large fields located in the center of the park',
            'continent' : 'NA',
            'city' :'Seattle',
            'public_accessibility' :True
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_create_cultural_heritage_item_endpoint_regex(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            '/cultural_heritage_item',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_create_cultural_heritage_item_with_guest_user(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        self.client.logout()
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        #Ensure guest user cannot create a cultural heritage item.
        self.assertEqual(response.status_code, 403)

    def test_create_cultural_heritage_item_with_empty_title(self):
        self.client.login(username= self.username,password= self.password)
        item_data = {
            "title": "",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 400)