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
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.data = {
            'username': self.username,
            'email': self.email,
            'password': self.password,
            'confirm_password': self.password,
        }
        self.item_data = {
            'title': 'Very emotional thresh hook',
            'public_accessibility':True,
        }
        self.client = APIClient()
        self.client.login(username=self.username,password=self.password)
    def test_create_cultural_heritage_item(self):
        response = self.client.post(
            '/cultural/',
            json.dumps(self.item_data),
            format ='json',

        )
        print(response.content)
        response_content = json.loads(smart_text(response.content))
        print (response_content)

        self.assertEqual(response.status_code, 200)
