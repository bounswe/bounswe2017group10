from django.contrib.auth.models import AbstractBaseUser, BaseUserManager
from django.db import models


class AccountManager(BaseUserManager):
    def create_user(self, email, password=None, **kwargs):
        # Ensure that an email address is set
        if not email:
            raise ValueError('Users must have a valid e-mail address')

        # Ensure that a username is set
        username = kwargs.get('username')
        if not username:
            raise ValueError('Users must have a valid username')
        if not password:
            raise ValueError('Users must have a password')
        if not (any(x.isupper() for x in password) and any(x.isdigit() for x in password) and len(password) >= 7):
            raise ValueError('Password should contain at least one capital letter and digit and 7 chars.')
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
        account = self.create_user(email, password, kwargs)

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
