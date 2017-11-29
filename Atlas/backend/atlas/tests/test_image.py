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
        self.my_items_url = '/cultural_heritage_item/myitems'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.email2 = 'emrantest1@gmail.com'
        self.username2 = 'heisenberg13'
        self.password2 = 'passworD1ss'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'
        self.user2 = Account.objects.create_user(
            email=self.email2, password=self.password2, username=self.username2)

        self.data = {
            'username': self.username,
            'email': self.email,
            'password': self.password,
            'confirm_password': self.password,
        }
        self.client = APIClient()
        self.client.login(username=self.username, password=self.password)

    def test_create_cultural_heritage_item_with_image_media_item(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        item_data = {
            'images': [
                {'url': 'http://i.imgur.com/3OLTFVq.jpg',}
            ]

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_create_cultural_heritage_item_with_invalid_image_media_item(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        item_data = {
            'images': [
                {'url': 'asd',}
            ]

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 400)

    def test_get_cultural_heritage_item_by_id_with_image_media_item(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.assertEqual(response.status_code, 201)
        # First image item
        image_item_data = {
            'images': [{'url': 'http://i.imgur.com/3OLTFVq.jpg',}]
        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            image_item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        # Second image item
        image_item_data = {
            'images': [{'url': 'http://i.imgur.com/3OL28374TFVq.jpg',}]
        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            image_item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        id = response_content['id']
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )

        self.assertEqual(response.status_code, 200)

        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['images']), 2)

    def test_create_cultural_heritage_item_with_many_images_at_once(self):
        item_data = {
            "title": "Space needle",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 201)

        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        image_item_data = {
            'images': [{'url': 'http://i.imgur.com/1113OLTFsdfVq.jpg',},
                       {'url': 'http://i.imgur.com/111r3OLTF21Vq.jpg',},
                       {'url': 'http://i.imgur.com/111e3OLT3213FVq.jpg',},
                       {'url': 'http://i.imgur.com/1113wO12LTFVq.jpg',},
                       {'url': 'http://i.imgur.com/1113weOLTFVq.jpg',},
                       ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            image_item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['images']), 5)

    def test_get_cultural_heritage_item_by_id_with_image_media_items(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        # First image item
        image_item_data1 = {
            'images': [{
                'url': 'http://i.imgur.com/3OLTFVq.jpg',
                'main': True
            },
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            image_item_data1,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        # Second image item
        image_item_data2 = {
            'images': [{'url': 'http://i.imgur.com/3OL28374TFVq.jpg',}
                       ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/image',
            image_item_data2,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )

        self.assertEqual(response.status_code, 200)

        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['images']), 2)



