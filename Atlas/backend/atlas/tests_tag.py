import pytest
from django.test import TestCase
from django.test.client import Client

from jwt_auth import utils
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
        self.user2 = Account.objects.create_user(
            email=self.email2, password=self.password2, username=self.username2)

        self.data = {
            'username': self.username,
            'email': self.email,
            'password': self.password,
            'confirm_password': self.password,
        }
        self.client = APIClient()
        self.client.login(username=self.username,password=self.password)
    def test_create_cultural_heritage_item_with_tags(self):
        item_data = {
            "title": "Space needle",
            'tags': [
                {'name':'place',},
                {'name':'Seattle'},
                {'name':'space'},
                {'name':'Needle'},
                {'name':'downtown'}
            ]
        }
        response = self.client.post(
        self.cultural_heritage_item_url,
        item_data,
        format='json',
        )
        response_content = json.loads(smart_text(response.content))

        self.assertEqual(response.status_code, 201)
        id = response_content['id']
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(response_content['tags']),5)

    def test_create_cultural_heritage_item_with_same_tags_from_different_items(self):
        item_data = {
            "title": "Space needle",
            'tags': [
                {'name': 'place',},
                {'name': 'Seattle'},
                {'name': 'space'},
                {'name': 'Needle'},
                {'name': 'downtown'}
            ]
        }
        item_data2 = {
            "title": "Discovery park",
            'tags': [
                {'name': 'place',},
                {'name': 'Seattle'},
                {'name': 'space'},
                {'name': 'Needle'},
                {'name': 'downtown'}
            ]
        }
        response = self.client.post(
             self.cultural_heritage_item_url,
             item_data,
             format='json',
         )

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data2,
            format='json',
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 201)
        id = response_content['id']
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(response_content['tags']), 5)

    def test_create_cultural_heritage_item_with_empty_tag(self):
        item_data = {
            "title": "Space needle is life",
            'tags': [

            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',
        )
        response_content = json.loads(smart_text(response.content))

        self.assertEqual(response.status_code, 201)
        id = response_content['id']
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        self.assertEqual(len(response_content['tags']),0)
    def test_get_tags(self):
        item_data = {
            "title": "Space needle",
            'tags': [
                {'name': 'place1',},
                {'name': 'Seattle1'},
                {'name': 'space1'},
                {'name': 'Needle1'},
                {'name': 'downtown1'}
            ]
        }
        item_data2 = {
            "title": "Discovery park",
            'tags': [
                {'name': 'place',},
                {'name': 'Seattle'},
                {'name': 'space'},
                {'name': 'Needle'},
                {'name': 'downtown'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 201)

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data2,
            format='json',
        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.tags_url,
            format='json',
        )
        #test endpoint flexibility
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content),10)
        response = self.client.get(
            '/tags',
            format='json',
        )

        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content), 10)