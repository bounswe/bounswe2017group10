import pytest
from django.test import TestCase
from django.test.client import Client

from jwt_auth import utils
from jwt_auth.compat import json, smart_text
from authentication.models import Account
from rest_framework.test import APIClient
from django.conf import settings


@pytest.mark.django_db
class JSONWebTokenAuthTestCase(TestCase):
    def setUp(self):
        self.email = 'talha12@gmail.com'
        self.username = 'heisenberg1'
        self.password = 'passworD1'
        self.login_url = '/api/auth/login/'
        self.sigun_url = '/api/auth/signup/'
        self.user = Account.objects.create_user(
            email = self.email , password =self.password, username =self.username)

        self.data = {
            'username': self.username,
            'email'   : self.email,
            'password': self.password,
            'confirm_password' :self.password,
        }
        self.login_data_with_username =  {
            'username_or_email' : self.username,
            'password'          : self.password,
        }


        self.client = Client()

    def test_jwt_login_json_with_username(self):
        """
        Ensure JWT login view using JSON POST works with only username and password.
        """
        response = self.client.post(
            self.login_url,
            json.dumps(self.login_data_with_username),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))

        decoded_payload = utils.jwt_decode_handler(response_content['token'])

        self.assertEqual(decoded_payload['username'], self.username)

    def test_jwt_login_json_with_email(self):
        """
        Ensure JWT login view using JSON POST works with only email and password.
        """
        data = {
            'username_or_email'    : self.email,
            'password' : self.password,
        }
        response = self.client.post(
            self.login_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))

        decoded_payload = utils.jwt_decode_handler(response_content['token'])

        self.assertEqual(decoded_payload['username'], self.username)


    def test_jwt_login_json_bad_creds(self):
        """
        Ensure JWT login view using JSON POST fails
        if bad credentials are used.
        """
        data = {
            'username': self.username,
            'password': 'wrongPassword'
        }
        response = self.client.post(
            self.login_url,
            json.dumps(self.data),
            content_type='application/json'
        )

        self.assertEqual(response.status_code, 400)


    def test_jwt_login_json_missing_fields(self):
        """
        Ensure JWT login view using JSON POST fails if missing fields.
        """
        response = self.client.post(
            self.login_url,
            json.dumps({'username': self.username}),
            content_type='application/json'
        )

        self.assertEqual(response.status_code, 400)

    def test_jwt_login_with_expired_token(self):
        """
        Ensure JWT login view works even if expired token is provided
        """
        payload = utils.jwt_payload_handler(self.user)
        payload['exp'] = 1
        token = utils.jwt_encode_handler(payload)

        auth = 'Bearer {0}'.format(token)
        response = self.client.post(
            self.login_url,
            json.dumps(self.login_data_with_username),
            content_type='application/json',
            HTTP_AUTHORIZATION=auth
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))

        decoded_payload = utils.jwt_decode_handler(response_content['token'])

        self.assertEqual(decoded_payload['username'], self.username)

    def test_signup(self):
        """
        Ensure JWT signup works using JSON POST .
        """
        data = {
            'email': "testing@gmail.com",
            'username': "Shannon1",
            'password': self.password,
            'confirm_password': self.password,
        }
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 201)

    def test_signup_default_profile_picture(self):
        """
        Ensure JWT signup works using JSON POST .
        """
        data = {
            'email': "Suleiman@gmail.com",
            'username': "Suleiman",
            'password': self.password,
            'confirm_password': self.password,
        }
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 201)
        self.login_data_with_username['username_or_email']= "Suleiman"
        response = self.client.post(
            self.login_url,
            json.dumps(self.login_data_with_username),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        token = response_content['token']
        response = APIClient().get(
            '/api/auth/me/',
            HTTP_AUTHORIZATION='JWT ' + token
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response_content['username'], "Suleiman")
        self.assertEqual(response_content['email'], "Suleiman@gmail.com")
        self.assertTrue(response_content['profile_picture'] in settings.PROFILE_PICTURES)

    def test_signupWithProfilePicture(self):
        """
        Ensure JWT signup works using JSON POST .
        """
        data = {
            'email': "testing@gmail.com",
            'username': "Shannon1",
            'password': self.password,
            'confirm_password': self.password,
            'profile_picture': 'http://i.imgur.com/3OLTFVq.jpg',
        }
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 201)

    def test_signupWithSameEmail(self):
        """
        Ensure JWT signup fails using JSON POST when the email is already taken .
        """
        data = {
            'email': "testing@gmail.com",
            'username': "Shannon1",
            'password': self.password,
            'confirm_password': self.password,
        }
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 400)

    def test_signupWithInvalidPassword(self):
        """
        Ensure JWT signup fails with password less than 7 chars
        """
        data = {
            'email': "testing@gmail.com",
            'username': "Shannon1",
            'password': "asd",
            'confirm_password': "asd",
        }
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code,400)

        #Ensure JWT fails with password that does not have any capital letter
        data['password'] = "askljqwlkeqj1312"
        data['confirm_password'] = "askljqwlkeqj1312"
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code,400)

        data['password'] = "AA123"
        data['confirm_password'] = "AA123"
        response = self.client.post(
            self.sigun_url,
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code,400)

    def test_login_regex(self):
        response = self.client.post(
            '/api/auth/login',
            json.dumps(self.login_data_with_username),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 200)

    def test_signup_regex(self):
        data = {
            'email': "testingaa@gmail.com",
            'username': "Shannon2",
            'password': self.password,
            'confirm_password': self.password,
        }
        response = self.client.post(
            '/api/auth/signup',
            json.dumps(data),
            content_type='application/json'
        )
        self.assertEqual(response.status_code, 201)

    def test_me_endpoint_with_loggedin_user(self):
        response = self.client.post(
            self.login_url,
            json.dumps(self.login_data_with_username),
            content_type ='application/json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        token = response_content['token']
        response = APIClient().get(
            '/api/auth/me/',
            HTTP_AUTHORIZATION='JWT ' + token
        )
        response_content = json.loads(smart_text(response.content))
        self.assertEqual(response.status_code,200)
        self.assertEqual(response_content['username'], self.username)
        self.assertEqual(response_content['email'], self.email)

    def test_me_endpoint_with_guest_user(self):
        token = "asdasd21312321j12jk312"
        response = APIClient().get(
            '/api/auth/me/',
            HTTP_AUTHORIZATION='JWT ' + token
        )
        self.assertEqual(response.status_code,403)
    def test_me_regex(self):
        response = self.client.post(
            self.login_url,
            json.dumps(self.login_data_with_username),
            content_type='application/json',
        )
        self.assertEqual(response.status_code, 200)
        response_content = json.loads(smart_text(response.content))
        token = response_content['token']
        response = APIClient().get(
            '/api/auth/me',
            HTTP_AUTHORIZATION='JWT ' + token
        )
        self.assertEqual(response.status_code, 200)



