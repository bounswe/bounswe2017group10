# Atlas Models

Check out the models.py in atlas folder. I have added 2 sample models

```
python manage.py makemigrations atlas
```
This will create migrations from atlas
```
python manage.py migrate
```
For migrating the tables to the DB.
After this:
```
python manage.py shell
>>> from atlas.models import User, Item
>>> u1=User(username='emir',age=23)
>>> u1.save()
>>> i1 = Item(user=u1,item_name='Galata')
>>> i1.save()
```
This will add the entities to DB.  
I will continue this tutorial
