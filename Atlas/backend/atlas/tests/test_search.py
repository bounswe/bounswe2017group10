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
            self.cultural_heritage_item_url + 'search/' + 'Draven'
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
            ],
            'place_name': 'meta',
            'latitude': '22.12',
            'longitude': '23.14'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Zoe support",

            'latitude': '26.12',
            'longitude': '27.14'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Tresh hook meta",
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
        self.assertEqual(len(response_content['results']), 3);

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
            format='json'

        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], 'Draven montage')
        self.assertEqual(response_content['results'][1]['title'], 'thresh montage')
        self.assertEqual(response_content['results'][2]['title'], 'Ahri montage')

    def test_cultural_heritage_search_with_tags_and_complete_items(self):
        item_data = {
            "title": "Ahri one shot",
            'tags': [
                {'name': 'adc'},
                {'name': 'support'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Draven support",
            'description': "Chinese ceramics show a continuous development since pre-dynastic times "
                           "and are one of the most significant forms of Chinese art and ceramics globally. "
                           "The first pottery was made during the Palaeolithic era. Chinese ceramics range from "
                           "construction materials such as bricks and tiles, to hand-built pottery vessels fired "
                           "in bonfires or kilns, to the sophisticated Chinese porcelain wares made for the imperial "
                           "court and for export. Porcelain is so identified with China that it is still called \"china\" "
                           "in everyday English usage. Most later Chinese ceramics, even of the finest quality, "
                           "were made on an industrial scale, thus few names of individual potters were recorded. "
                           "Many of the most important kiln workshops were owned by or reserved for the Emperor, and "
                           "large quantities of ceramics were exported as diplomatic gifts or for trade from an early "
                           "date, initially to East Asia and the Islamic world, and then from around the 16th century "
                           "to Europe. Chinese ceramics have had an enormous influence on other ceramic traditions in "
                           "these areas. Increasingly over their long history, Chinese ceramics can be classified "
                           "between those made for the imperial court, either to use or distribute, those made "
                           "for a discriminating Chinese market, and those for popular Chinese markets or for "
                           "export. Some types of wares were also made only or mainly for special uses such as "
                           "burial in tombs, or for use on altars.",
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
                {'name': 'adc'},
                {'name': 'support'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Tresh hook montage",
            'tags': [
                {'name': 'adc'},
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
        self.assertEqual(len(response_content['results']), 4);
        self.assertEqual(response_content['results'][0]['title'], 'Draven support')

    def test_cultural_heritage_search_with_multiple_words(self):

        item_data = {
            "title": "gas works",
            'tags': [
                {'name': 'adc'},
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
            self.cultural_heritage_item_url + 'search/' + 'gas works'
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1);
