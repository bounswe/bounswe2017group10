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

    def test_create_cultural_heritage_item_with_comment(self):
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
        text = 'That is a nice heritage item'
        item_data = {
            'comment':
                {'text': text,}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['comments']), 1)
        self.assertEqual(response_content['comments'][0]['text'], text)
        self.assertEqual(response_content['id'], id)

    def test_create_cultural_heritage_item_with_empty_comment(self):
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
            'comment':
                {}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 400)

    def test_create_cultural_heritage_item_with_multiple_comments(self):
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
        text = 'That is a nice heritage item'
        item_data = {
            'comment':
                {'text': text,}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            'comment':
                {'text': 'thresh hook ',}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

        response = self.client.get(
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['comments']), 2)

    def test_create_cultural_heritage_item_with_comment_and_user(self):
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
        text = 'That is a nice heritage item'
        item_data = {
            'comment':
                {'text': text,}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        text = 'That is a nice heritage item2'
        item_data = {
            'comment':
                {'text': text,}

        }
        response = self.client.post(
            self.cultural_heritage_item_url + str(id) + '/comment',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url,
            format='json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['comments'][1]['text'], text)
        self.assertEqual(response_content['results'][0]['comments'][1]['user_info']['username'], self.username)
