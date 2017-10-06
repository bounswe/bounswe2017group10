from django.contrib.auth.models import AbstractBaseUser, BaseUserManager
from django.db import models
from django.http import HttpResponseBadRequest


class AccountManager(BaseUserManager):
    def create_user(self, email, password=None, **kwargs):
        account = self.model(
            email=self.normalize_email(email),
            username=kwargs.get('username'),
            firstname=kwargs.get('firstname', None),
            lastname=kwargs.get('lastname', None),
        )

        account.set_password(password)
        account.save()

        return account

    def create_superuser(self, email, password=None, **kwargs):
        account = self.create_user(email, password, **kwargs)

        account.is_admin = True
        account.save()

        return account


class Account(AbstractBaseUser):
    username = models.CharField(unique=True, max_length=50)
    email = models.EmailField(unique=True)

    firstname = models.CharField(max_length=100, blank=True,null =True)
    lastname = models.CharField(max_length=100, blank=True,null =True)

    date_created = models.DateTimeField(auto_now_add=True,null=True)
    date_modified = models.DateTimeField(auto_now=True,null=True)

    is_admin = models.BooleanField(default=False)

    objects = AccountManager()

    USERNAME_FIELD = 'username'
    REQUIRED_FIELDS = ['email']

    def get_full_name(self):
        return ' '.join(self.firstname, self.last_login)
