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

    def test_cultural_heritage_favorite_item(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )

        self.assertEqual(response.status_code, 201)

    def test_get_favorite_items(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 201)
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Urgot top montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            '/user/cultural_heritage_item/favorite/',
            format='json',
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(response_content['results']), 2)
        self.assertEqual(response_content['results'][0]['item']['title'], 'Ahri mid montage')

    def test_get_favorited_cultural_heritage_item(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['is_favorite'], True)

        response = self.client.get(
            self.cultural_heritage_item_url,
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertAlmostEqual(response_content['results'][0]['is_favorite'], True)

    def test_unfavorite_cultural_heritage_item(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.delete(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 204)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['is_favorite'], False)

    def test_cultural_heritage_item_favorited_amount(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)

        response = self.client2.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['favorited_amount'], 2)

    def test_favorite_cultural_heritage_item_twice(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        # Here we ensure that the same user cannot favorite the same item more than once.
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        self.assertEqual(response.status_code, 400)

    def test_cultural_heritage_item_favorited_amount_field_not_editable(self):
        item_data = {
            "title": "Ahri mid montage",
            "favorited_amonut": 10,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['favorited_amount'], 0)

    def test_get_favorited_cultural_heritage_item_favorited_by_another_user(self):
        item_data = {
            "title": "Ahri mid montage",
        }
        response = self.client2.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        response = self.client2.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['is_favorite'], False)

        response = self.client.get(
            self.cultural_heritage_item_url,
            format='json',

        )

        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 200)
        self.assertAlmostEqual(response_content['results'][0]['is_favorite'], False)
