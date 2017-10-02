from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User
from django import forms
class RegistrationForm(UserCreationForm):
    def clean_password1(self):
        password =self.cleaned_data['password1']
        if len(password) < 7:
            raise forms.ValidationError("password must contain at least 7 chars.")
        if not (any(c.isupper() for c in password)):
            raise forms.ValidationError("password must contain at least one capital letter.")
        if not (any(c.isdigit() for c in password)):
            raise forms.ValidationError("password must contain at least one number.")
        return password
    def clean_password2(self):
        password = self.cleaned_data['password2']
        return password

    def clean_email(self):
        email = self.cleaned_data.get('email')
        username = self.cleaned_data.get('username')
        if email and User.objects.filter(email=email).exclude(username=username).exists():
            raise forms.ValidationError(u'Email addresses must be unique.')
        return email
    class Meta:
        model = User
        fields = ('username', 'email', 'password1', 'password2',)