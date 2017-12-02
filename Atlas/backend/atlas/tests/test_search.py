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

    def test_cultural_heritage_search_contains(self):
        item_data = {
            "title": "Oulder Hill",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Shoulder Hill",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Titan's Boulder",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Nothing like the other word",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + 'search_autocorrect/' + 'oulder'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 3);

    def test_cultural_heritage_search(self):
        item_data = {
            "title": "Vayne Jungle meta",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Draven mid meta",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Thresh smite",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'draven'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1);
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'meta'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 2);

    def test_cultural_heritage_search_with_tags(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'adc',},
                {'name': 'meta'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Tresh hook",
            'tags': [
                {'name': 'Meta'},
                {'name': 'support'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'adc'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1);
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'meta'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 2);

    def test_cultural_heritage_search_with_guest_user(self):
        item_data = {
            "title": "Draven support",
            'tags': [
                {'name': 'adc',},
                {'name': 'meta'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Tresh hook",
            'tags': [
                {'name': 'Meta'},
                {'name': 'support'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        self.client.logout()
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'adc'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1);
        response = self.client.get(
            self.cultural_heritage_item_url + 'search/' + 'meta'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 2);

    def test_cultural_heritage_search_contains_with_guest_user(self):
        item_data = {
            "title": "Oulder Hill",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Shoulder Hill",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Titan's Boulder",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Nothing like the other word",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        self.client.logout()
        response = self.client.get(
            self.cultural_heritage_item_url + 'search_autocorrect/' + 'oulder'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 3);

    def test_nearby_search(self):
        item_data = {
            "title": "Draven montage",
            "longitude": "23.123523",
            "latitude": "21.123523",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

        item_data = {
            "title": "Elo"
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

        item_data = {
            "title": "thresh montage",
            "longitude": "50.123523",
            "latitude": "23.123523",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Ahri montage",
            "longitude": "-87.123523",
            "latitude": "-85.123523",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        location_data = {
            'longitude': "12.51231",
            'latitude': "11.51231",

        }
        response = self.client.get(
            '/nearby_items',
            location_data,
            format ='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'],'Draven montage')
        self.assertEqual(response_content['results'][1]['title'],'thresh montage')
        self.assertEqual(response_content['results'][2]['title'],'Ahri montage')


