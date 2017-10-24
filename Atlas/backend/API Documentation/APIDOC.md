FORMAT: 1A
HOST: localhost:8000/

# Atlas


CmpE 352/451 Group 10: Atlas API Documentation
You can see this document more clearly in [here](http://docs.bounswe2017group10.apiary.io).

## Login [/api/auth/login/]


### Login with username or e-mail and password [POST]


+ Request (application/json)

        {
            'username_or_email' : 'kutay@atlasmail.com',
            'password'          : 'hjk123',
        }

+ Response 200 (application/json)

            {

                'token': 'adsqwev34121f!^%!^sd1edc131231fj1',
            }

## Sign-up [/api/auth/signup/]


### Signup with username, e-mail and password [POST]


+ Request (application/json)

        {
            'email': 'kutay@atlasmail.com',
            'username': 'kutay',
            'password': 'hjk123.',
            'confirm_password': 'hjk123.',
        }

+ Response 201 (application/json)


##Information about user [/api/auth/me]


### Get firstname, lastname, username, e-mail information [GET]


+ Response 200 (application/json)


        {
            'firstname': 'Kutay',
            'lastname': 'CANDAN',
            'email': 'kutay@atlasmail.com',
            'username': 'kutay',
        }




## Cultural heritage with no parameter [/cultural_heritage_item]


### Create cultural heritage item [POST]

+ Request (application/json)

        {
            'country': 'America',
            'title' : 'Seattle Discovery Park',(MUST)
            'description' : 'This park is really awesome. You can play frisbee with your dog on very large fields located in the center of the park',
            'continent' : 'NA',
            'city' :'Seattle',
            'public_accessibility' :True
        }


+ Response 201 (application/json)


### Get all cultural heritage item [GET]


+ Response 200 (application/json)

        [
            {
                'country': 'America',
                'title' : 'Seattle Discovery Park',(MUST)
                'description' : 'This park is really awesome. You can play frisbee with your dog on very large fields located in the center of the park',
                'continent' : 'NA',
                'city' :'Seattle',
                'public_accessibility' :True
            }
        ]


## Cultural heritage with id parameter [/cultural_heritage_item/<id>]


### Get cultural heritage item with id[GET]


+ Response 200 (application/json)


        {
            'country': 'America',
            'title' : 'Seattle Discovery Park',(MUST)
            'description' : 'This park is really awesome. You can play frisbee with your dog on very large fields located in the center of the park',
            'continent' : 'NA',
            'city' :'Seattle',
            'public_accessibility' :True
        }


## Cultural heritage with id and image parameter [/cultural_heritage_item/<id>/image]


### Adding media items to cultural heritage item[POST]


+ Request (application/json)

        {
           'images': [
                {'url': 'http://i.imgur.com/3OLTFVq.jpg',}
            ]
        }


+ Response 201 (application/json)









