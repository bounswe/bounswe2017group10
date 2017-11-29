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

    def test_create_cultural_heritage_item(self):

        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format ='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_list_user_items(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.my_items_url,
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'], title)

    def test_list_user_items_with_another_user(self):
            title = 'Very emotional thresh hook'
            item_data = {
                "title": title,
            }
            response = self.client.post(
                self.cultural_heritage_item_url,
                item_data,
                format='json',

            )
            self.assertEqual(response.status_code, 201)
            response = self.client.get(
                self.my_items_url,
                format='json',

            )
            self.assertEqual(response.status_code, 200)
            response_content = json.loads(smart_text(response.content))
            self.assertEqual(response_content['results'][0]['title'], title)
            self.client.logout()
            self.client.login(username=self.username2, password=self.password2)

            response = self.client.get(
                self.my_items_url,
                format='json',

            )
            self.assertEqual(response.status_code, 200)
            response_content = json.loads(smart_text(response.content))
            self.assertEqual(len(response_content['results']), 0)

    def test_create_cultural_heritage_item_with_missing_title(self):
        item_data = {
           'country' : 'America'
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        #Since title is missing it should return 400
        self.assertEqual(response.status_code, 400)

    def test_create_cultural_heritage_item_with_all_fields(self):
        item_data = {
            'country': 'America',
            'title' : 'Seattle Discovery Park',
            'description' : 'This park is really awesome. You can play frisbee with your dog on very large fields located in the center of the park',
            'continent' : 'NA',
            'city' :'Seattle',
            'public_accessibility' :True
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_create_cultural_heritage_item_endpoint_regex(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        response = self.client.post(
            '/cultural_heritage_item',
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)

    def test_create_cultural_heritage_item_with_guest_user(self):
        item_data = {
            "title": "Very emotional thresh hook",
        }
        self.client.logout()
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        #Ensure guest user cannot create a cultural heritage item.
        self.assertEqual(response.status_code, 403)

    def test_create_cultural_heritage_item_with_empty_title(self):
        self.client.login(username= self.username,password= self.password)
        item_data = {
            "title": "",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 400)

    def test_get_cultural_heritage_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code,201)
        response = self.client.get(
            self.cultural_heritage_item_url,
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['results'][0]['title'] ,title)


    def test_get_cultural_heritage_item_by_id(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id =response_content['id']
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        self.assertEqual(response.status_code,200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['title'],title)
        self.assertEqual(response_content['id'],id)

    def test_get_cultural_heritage_item_by_id_with_guest_user(self):
        title = 'Very publicly available thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        self.client.logout()
        response = self.client.get(
            self.cultural_heritage_item_url + str(id),
            format='json',
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response_content['title'],'Very publicly available thresh hook')
        self.assertEqual(response_content['id'],id)

    def test_list_all_cultural_heritage_items_anon(self):
        title = 'Very spammable pudge hook'
        for x in range(0, 3):
            item_data = {
                "title": title,
            }
            response = self.client.post(
                self.cultural_heritage_item_url,
                item_data,
                format='json',

            )
            self.assertEqual(response.status_code, 201)
        self.client.logout()
        response = self.client.get(
            self.cultural_heritage_item_url,
            format='json',
        )
        self.assertEqual(response.status_code, 200)

        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['count'], 3)

    def test_get_cultural_heritage_item_by_id_regex(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
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
            self.cultural_heritage_item_url + str(id)+'/',
            format='json',
        )
        self.assertEqual(response.status_code, 200 )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['title'],title)
        self.assertEqual(response_content['id'],id)

    def test_delete_cultural_heritage_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        response = self.client.delete(
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )
        self.assertEqual(response.status_code, 204)

        response = self.client.get(
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )

        self.assertEqual(response.status_code, 404)

    def test_update_cultural_heritage_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        new_title = 'Akali sup montage'
        item_data = {
            "title": new_title,
        }
        response_content = json.loads(smart_text(response.content))
        id = response_content['id']
        response = self.client.patch(
            self.cultural_heritage_item_url + str(id) + '/',
            item_data,
            format='json',
        )
        self.assertEqual(response.status_code, 200)

        response = self.client.get(
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )

        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response_content['title'],new_title)

    def test_get_cultural_heritage_item_by_invalid_id(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response_content = json.loads(smart_text(response.content))
        response = self.client.get(
            self.cultural_heritage_item_url + '12321321',
            format='json',
        )
        self.assertEqual(response.status_code, 404)

    def test_get_cultural_heritage_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        item_data = {
            "title": "Draven montage",
        }
        response = self.client.post(
            self.cultural_heritage_item_url,
            item_data,
            format='json',

        )
        self.assertEqual(response.status_code, 201)
        response = self.client.get(
            self.cultural_heritage_item_url + "?limit=1",
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1)
        response = self.client.get(
            self.cultural_heritage_item_url + "?limit=1&offset=1",
            format='json',

        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(len(response_content['results']), 1)
