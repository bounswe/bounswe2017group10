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
            'description' : "Chinese ceramics show a continuous development since pre-dynastic times "
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
                            "burial in tombs, or for use on altars."
                            ,
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
            'place_name':'Seattle'
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
        self.assertEqual(response_content['place_name'],'Seattle')
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
                           "burial in tombs, or for use on altars."
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
                        'description' : "Chinese ceramics show a continuous development since pre-dynastic times "
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
                            "burial in tombs, or for use on altars."
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

    def test_create_cultural_heritage_item_with_location(self):
        item_data = {
            "title": "Very emotional thresh hook",
            'longitude': 10.52122,
            'latitude': 20.12312
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
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )

        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        self.assertAlmostEqual(float(response_content['longitude']),10.52122 )
        self.assertAlmostEqual(float(response_content['latitude']),20.12312 )

    def test_update_cultural_heritage_item(self):
        title = 'Very emotional thresh hook'
        item_data = {
            "title": title,
            "tags" : [
                {
                    "name":"talha",
                    "name":"thresh",
                }
            ]
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
            "comments": [],
            "description": "new description",
            "id": id,
            "images": [],
            "public_accessibility": True,
            "tags": [
                {
                    "name": "tag"
                }
            ],
            "title": "Title 7jaaa",
            # "user": 2 Since we wont change the user
        }
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
        self.assertEqual(response_content['title'], 'Title 7jaaa')
        self.assertEqual(response_content['tags'][0]['name'],'tag')
    def test_create_cultural_heritage_item_with_time(self):
        item_data = {
            "title": "Very emotional thresh hook",
            'start_year': 1512,
            'end_year': 1571
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
            self.cultural_heritage_item_url + str(id) + '/',
            format='json',
        )

        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))


        self.assertAlmostEqual(float(response_content['start_year']), 1512)
        self.assertAlmostEqual(float(response_content['end_year']), 1571)
