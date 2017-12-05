import pytest
from django.test import TestCase

from jwt_auth.compat import json, smart_text
from authentication.models import Account
from rest_framework.test import APIClient



@pytest.mark.django_db
class cultural_heritage_item(TestCase):
    def setUp(self):
        self.email = 'talhatest1@gmail.com'
        self.username = 'heisenberg12'
        self.password = 'passworD1ss'
        self.login_url = '/api/auth/login/'
        self.sigun_url = '/api/auth/signup/'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'
        self.tags_url = '/tags/'
        self.my_items_url= '/cultural_heritage_item/myitems'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.email2 = 'emrantest1@gmail.com'
        self.username2 = 'heisenberg13'
        self.password2 = 'passworD1ss'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'
        self.user2 = Account.objects.create_user(email="threshDraven@tgmail.com", username="Thresh12",
                                                 password="123123AA")
        self.data = {
            'username': self.username,
            'email': self.email,
            'password': self.password,
            'confirm_password': self.password,
        }
        self.client = APIClient()
        self.client.login(username=self.username,password=self.password)

        self.client2 = APIClient()
        self.client2.login(username="Thresh12", password="123123AA")

    def test_recommendation_response_not_contains_the_same_item(self):
        item_data = {
            "title": "Draven support",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        item_data = {
            "item_id": id,
        }
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 0);

    def test_recommendation(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'}
            ],
            'longitude': '23.523',
            'latitude' : '43.232',
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item_data = {
            "title": "zoe support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'}
            ],
            'longitude': '40.523',
            'latitude': '52.232',
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "istanbul galata tower",
            'longitude': '25.523',
            'latitude': '48.232',
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "item_id": id,
        }
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'],'zoe support' );

    def test_recommendation_with_time(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'}
            ],
            'longitude': '23.523',
            'latitude': '43.232',
            'start_year' : 1500,
            'end_year' : 1700,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item_data = {
            "title": "zoe support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'}
            ],
            'longitude': '40.523',
            'latitude': '52.232',
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "istanbul galata tower",
            'longitude': '25.523',
            'latitude': '48.232',
            'start_year' : 1500,
            'end_year' : 1700,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "item_id": id,
        }
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'istanbul galata tower');

    def test_recommendation_with_same_time(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'}
            ],
            'longitude': '23.523',
            'latitude': '43.232',
            'start_year': 1500,
            'end_year': 1500,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item_data = {
            "title": "istanbul galata tower",
            'longitude': '25.523',
            'latitude': '48.232',
            'start_year': 1500,
            'end_year': 1500,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "item_id": id,
        }
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'istanbul galata tower');