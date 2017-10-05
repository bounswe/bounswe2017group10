import pytest
from django.test import TestCase
from django.test.client import Client

from jwt_auth import utils
from jwt_auth.compat import json, smart_text
from authentication.models import Account


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
        
        self.client = Client()

    def test_jwt_login_json(self):
        """
        Ensure JWT login view using JSON POST works.
        """
        response = self.client.post(
            self.login_url,
            json.dumps(self.data),
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
        self.data['password'] = 'wrong'

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
            json.dumps(self.data),
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
        Ensure JWT signup works using JSON POST .
        """
        data = {
            'email': "testing@gmail.com",
            'username': "Shannon1",
            'password': "asd",
            'confirm_password': "asd",
        }
        try :
            response = self.client.post(
                self.sigun_url,
                json.dumps(data),
                content_type='application/json'
            )
            self.assertFail()
        except ValueError :
            pass

        data['password'] = "askljqwlkeqj1312"
        data['confirm_password'] = "askljqwlkeqj1312"
        try :
            response = self.client.post(
                self.sigun_url,
                json.dumps(data),
                content_type='application/json'
            )
            self.assertFail()
        except ValueError :
            pass
        data['password'] = "AA123"
        data['confirm_password'] = "AA123"
        try:
            response = self.client.post(
                self.sigun_url,
                json.dumps(data),
                content_type='application/json'
            )
            self.assertFail()
        except ValueError:
            pass

