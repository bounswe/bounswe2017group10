from authentication.models import Account

import pytest
from django.test import TestCase
from jwt_auth.compat import json, smart_text
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
        self.my_items_url = '/cultural_heritage_item/myitems'
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
        self.client.login(username=self.username, password=self.password)

        self.client2 = APIClient()
        self.client2.login(username="Thresh12", password="123123AA")

    def test_item_visit_time(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
            "tags": [
                {
                    "name": "talha",
                    "name": "thresh",
                }
            ]
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
            'cultural_heritage_item': id,
            'duration': 5

        }
        response = self.client.put(
            '/user/visit_time',
            item_data,
            format='json',
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 200)

        self.assertEqual(response_content['duration'], 5)

        response = self.client.put(
            '/user/visit_time',
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['duration'], 10)

        response = self.client2.put(
            '/user/visit_time',
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['duration'], 5)

    def test_item_visit_time_with_nonexisistent_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
            "tags": [
                {
                    "name": "talha",
                    "name": "thresh",
                }
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            'cultural_heritage_item': 15556,
            'duration': 5

        }
        response = self.client.put(
            '/user/visit_time',
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 400)

    def test_item_visit_time_with_bad_data(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
            "tags": [
                {
                    "name": "talha",
                    "name": "thresh",
                }
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            'cultural_heritage': 15556,
            'duration': 5

        }
        response = self.client.put(
            '/user/visit_time',
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 400)
