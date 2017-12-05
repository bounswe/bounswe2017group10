import datetime
from math import log

from atlas.models import Cultural_Heritage
from atlas.popularity import popularity_score, trending_score
from atlas.popularity_constants import *
from authentication.models import Account

import copy
import pytest
from django.test import TestCase
from jwt_auth.compat import json, smart_text
from rest_framework.test import APIClient


@pytest.mark.django_db
class popularity_score_tester(TestCase):
    def setUp(self):
        self.login_url = '/api/auth/login/'
        self.sigun_url = '/api/auth/signup/'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'
        self.item_visit_url = '/user/visit_time'

        self.email = 'esref@gmail.com'
        self.username = 'esref12'
        self.password = 'passworD1ss'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.client = APIClient()
        self.client.login(username=self.username, password=self.password)

    def test_item_popularity_visit_time(self):
        response = self.client.post(
            self.cultural_heritage_item_url,
            {
                'title': 'Lee Sin'
            },
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))

        item = Cultural_Heritage.objects.all()[0]
        self.assertEqual(popularity_score(item), 0)

        visit_time = 10
        response = self.client.put(
            self.item_visit_url,
            {
                'cultural_heritage_item': response_content['id'],
                'duration': visit_time
            },
            format='json'
        )
        self.assertEqual(response.status_code, 200)
        item = Cultural_Heritage.objects.all()[0]

        expected_popularity = log(1 + visit_time * COEFF_VIEW_SEC)
        self.assertAlmostEqual(popularity_score(item), expected_popularity)

    def test_item_popularity_num_comments(self):

        response = self.client.post(
            self.cultural_heritage_item_url,
            {
                'title': 'Lee Sin'
            },
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.all()[0]
        self.assertEqual(popularity_score(item), 0)

        num_comments = 6
        for i in range(num_comments):
            response = self.client.post(
                self.cultural_heritage_item_url + str(id) + '/comment',
                {
                    'comment': {
                        'text': 'Comment {}'.format(i)
                    }
                },
                format='json'
            )
            self.assertEqual(response.status_code, 201)

        item = Cultural_Heritage.objects.all()[0]
        expected_popularity = log(1 + num_comments * COEFF_NUM_COMMENTS)
        self.assertAlmostEqual(popularity_score(item), expected_popularity)

    def test_item_popularity_num_favorites(self):
        response = self.client.post(
            self.cultural_heritage_item_url,
            {
                'title': 'Lee Sin'
            },
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))

        response = self.client.post(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',
        )
        self.assertEqual(response.status_code, 201)

        item = Cultural_Heritage.objects.all()[0]
        expected_popularity = log(1 + COEFF_NUM_FAVORITES)
        self.assertAlmostEqual(popularity_score(item), expected_popularity)

        response = self.client.delete(
            '/user/cultural_heritage_item/' + str(response_content['id']) + '/favorite/',
            format='json',
        )
        self.assertEqual(response.status_code, 204)

        item = Cultural_Heritage.objects.all()[0]
        self.assertEqual(popularity_score(item), 0)

    def test_item_popularity_too_short_description(self):
        too_short_description = 'This is a Turkish coffee.'
        response = self.client.post(
            self.cultural_heritage_item_url,
            {
                'title': 'Turkish Coffee',
                'description': too_short_description
            },
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), 0)

    def test_item_popularity_long_description(self):
        long_description = '''
Sadistic and cunning, Thresh is an ambitious and restless spirit of the Shadow
Isles. Once the custodian of countless arcane secrets, he sought a power
greater than life or death, and now sustains himself by tormenting and breaking
others with slow, excruciating inventiveness. His victims suffer far beyond their
brief mortal coil as Thresh wreaks agony upon their souls, imprisoning them in
his unholy lantern to torture for all eternity.
'''
        response = self.client.post(
            self.cultural_heritage_item_url,
            {
                'title': 'Thresh',
                'description': long_description
            },
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertAlmostEqual(popularity_score(item), COEFF_COMPLETENESS)

    def test_item_popularity_too_few_tags(self):
        item_data = {
            'title': 'Lee Sin'
        }
        item_data['tags'] = []
        for i in range(COMPLETE_TAG_NUM - 1):
            item_data['tags'].append({'name': 'tag {}'.format(i)})

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), 0)

    def test_item_popularity_complete_tags(self):
        item_data = {
            'title': 'Lee Sin'
        }
        item_data['tags'] = []
        for i in range(COMPLETE_TAG_NUM):
            item_data['tags'].append({'name': 'tag {}'.format(i)})

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), COEFF_COMPLETENESS)

    def test_item_popularity_too_few_images(self):
        item_data = {
            'title': 'Lee Sin'
        }
        item_data['images'] = []
        for i in range(COMPLETE_IMG_NUM - 1):
            item_data['tags'].append({'url': 'url {}'.format(i)})

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), 0)

    def test_item_popularity_complete_images(self):
        item_data = {
            'title': 'Lee Sin'
        }
        item_data['images'] = []
        for i in range(COMPLETE_IMG_NUM):
            item_data['images'].append({'url': 'http://imgur.com/{}'.format(i)})

        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), COEFF_COMPLETENESS)

    def test_item_popularity_complete_location(self):
        item_data = {
            'title': 'Lee Sin',
            'latitude': '37.460044',
            'longitude': '48.247236'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']

        item = Cultural_Heritage.objects.filter(id=id)[0]
        self.assertEqual(popularity_score(item), COEFF_COMPLETENESS)


@pytest.mark.django_db
class trending_score_tester(TestCase):
    def setUp(self):
        self.login_url = '/api/auth/login/'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'

        self.email = 'esref@gmail.com'
        self.username = 'esref12'
        self.password = 'passworD1ss'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.client = APIClient()
        self.client.login(username=self.username, password=self.password)

    def test_newer_items_have_higher_score(self):
        item_data = {
            'title': 'Lee Sin'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)

        new_item = Cultural_Heritage.objects.all()[0]
        old_item = copy.deepcopy(new_item)
        new_date = new_item.created_time
        old_item.created_time = datetime.date(new_date.year, new_date.month, new_date.day - 1)

        old_trending_score = trending_score(old_item)
        new_trending_score = trending_score(new_item)
        self.assertTrue(new_trending_score > old_trending_score)


@pytest.mark.django_db
class featured_endpoint_tester(TestCase):
    def setUp(self):
        self.login_url = '/api/auth/login/'
        self.cultural_heritage_item_url = '/cultural_heritage_item/'

        self.email = 'esref@gmail.com'
        self.username = 'esref12'
        self.password = 'passworD1ss'
        self.user = Account.objects.create_user(
            email=self.email, password=self.password, username=self.username)

        self.client = APIClient()
        self.client.login(username=self.username, password=self.password)

    def test_featured_endpoint_item_ordering(self):
        item_data = {
            'title': 'Lee Sin'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        worst_item = Cultural_Heritage.objects.filter(id=id)[0]

        long_description = '''
Sadistic and cunning, Thresh is an ambitious and restless spirit of the Shadow
Isles. Once the custodian of countless arcane secrets, he sought a power
greater than life or death, and now sustains himself by tormenting and breaking
others with slow, excruciating inventiveness. His victims suffer far beyond their
brief mortal coil as Thresh wreaks agony upon their souls, imprisoning them in
his unholy lantern to torture for all eternity.
'''
        item_data = {
            'title': 'Lee Sin',
            'description': long_description,
            'tags': [
                {'name': 'tag-1'},
                {'name': 'tag-2'}
            ],
            'images': [
                {'url': 'http://imgur.com/1'}
            ]
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        avg_item = Cultural_Heritage.objects.filter(id=id)[0]

        item_data = {
            'title': 'Lee Sin',
            'description': long_description,
            'tags': [
                {'name': 'tag-1'},
                {'name': 'tag-2'}
            ],
            'images': [
                {'url': 'http://imgur.com/1'}
            ],
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json'
        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        response = self.client.post(
            '/user/cultural_heritage_item/' + str(id) + '/favorite/',
            format='json',
        )
        self.assertEqual(response.status_code, 201)
        best_item = Cultural_Heritage.objects.filter(id=id)[0]

        response = self.client.get(
            self.cultural_heritage_item_url + 'featured/',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))

        response_item_list = response_content['results']
        self.assertEqual(response_item_list[0]['id'], best_item.id)
        self.assertEqual(response_item_list[1]['id'], avg_item.id)
        self.assertEqual(response_item_list[2]['id'], worst_item.id)
