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

    def test_personalized_recommendation_when_favorited_item(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'},
                {'name': 'fun'},

            ],
            'longitude': '23.523',
            'latitude': '43.232',
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
                {'name': 'support'},

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
            "title": "seattle park",
            'longitude': '0.523',
            'latitude': '0.232',
            'tags': [
                {'name': 'seattle'},
                {'name': 'fun'},
            ],
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        item_id = response_content['id']

        item_data = {
            "title": "istanbul galata tower",
            'longitude': '25.523',
            'latitude': '48.232',
            'tags': [
                {'name': 'istanbul'},
                {'name': 'fun'},
            ],
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
        self.assertEqual(response_content['results'][0]['title'], 'zoe support');

        # Favorite an item and make sure items with similar items get higher scores
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(item_id) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'istanbul galata tower')

        # Unfavorite the same item and make sure the result is the same as the previous one

        response = self.client.delete(
            '/user/cultural_heritage_item/' + str(item_id) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 204)
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'zoe support')

    def test_personalized_recommendation_when_commented_item(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'},
                {'name': 'fun'},

            ],
            'longitude': '23.523',
            'latitude': '43.232',
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
                {'name': 'support'},

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
            "title": "seattle park",
            'longitude': '0.523',
            'latitude': '0.232',
            'tags': [
                {'name': 'seattle'},
                {'name': 'fun'},
            ],
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        item_id = response_content['id']

        item_data = {
            "title": "istanbul galata tower",
            'longitude': '25.523',
            'latitude': '48.232',
            'tags': [
                {'name': 'istanbul'},
                {'name': 'fun'},
            ],
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
        self.assertEqual(response_content['results'][0]['title'], 'zoe support');

        # comment an item and make sure items with similar items get higher scores
        text = 'That is a nice heritage item'
        item_data_1 = {
            'comment':
                {'text': text,}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(item_id) + '/comment',
            item_data_1,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            '/cultural_heritage_item/recommendation',
            item_data,
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'istanbul galata tower')
    def test_user_feed_when_favorited(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support'},
                {'name': 'fun'},

            ],
            'longitude': '23.523',
            'latitude': '43.232',
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)


        item_data = {
            "title": "zoe support",
            'tags': [
                {'name': 'lol'},
                {'name': 'support1'},

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
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        response = self.client.get(
            self.cultural_heritage_item_url + 'featured/',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'Draven support');

        response = self.client.post(
            '/user/cultural_heritage_item/' + str(id) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)

        response = self.client.get(
            self.cultural_heritage_item_url + 'featured/',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'zoe support');