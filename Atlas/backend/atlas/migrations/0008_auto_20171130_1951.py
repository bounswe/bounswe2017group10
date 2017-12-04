# -*- coding: utf-8 -*-
# Generated by Django 1.11.5 on 2017-11-30 19:51
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):
    dependencies = [
        ('atlas', '0007_comment'),
    ]

    operations = [
        migrations.AddField(
            model_name='cultural_heritage',
            name='latitude',
            field=models.DecimalField(decimal_places=6, max_digits=9, null=True),
        ),
        migrations.AddField(
            model_name='cultural_heritage',
            name='longitude',
            field=models.DecimalField(decimal_places=6, max_digits=9, null=True),
        ),
    ]
