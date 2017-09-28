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
After adding some more users with shell check out the code in
atlas.views and atlas.urls. 
If you run the server in localhost:8080 and type 
```
localhost:8080/users
```
 in your browser Django will serve the view that has been parsed in the urls.py  
The users view in views just simply queries for the youngest 5 users and serializes it
It should return something like:

```
[{"model": "atlas.user", "pk": 3, "fields": {"username": "Ramo", "age": 25}}, {"model": "atlas.user", "pk": 2, "fields": {"username": "emir", "age": 23}}]
``` 