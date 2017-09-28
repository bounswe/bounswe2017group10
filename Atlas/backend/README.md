# Atlas Backend
Project can be visited here:
http://174.129.64.109/

# Setting up the development environment

## OS X

### Install pip3 and python3
```bash
brew install python3
curl https://bootstrap.pypa.io/get-pip.py | python3
python3
```

### Make sure you add pip3 package bindir to your path (required for running virtualenv later on)
```bash
export PATH=$PATH/Library/Frameworks/Python.framework/Versions/3.6/bin
```

### Go to backend directory
```bash
cd Atlas/backend
```

### Run the following:
```bash
pip3 install virtualenv
```

### Create the virtual environment
```bash
virtualenv atlasenv
```

### Get inside virtual environment
```bash
source atlasenv/bin/activate
```

### Install Django
```bash
pip install -r requirements.txt # We always use pip inside virtualenv, not pip3
```

### Get postgres database
Download from [here](https://postgresapp.com/). Make sure `psql` from commandline works afterwards

### Perform initial database setup
Enter postgresql prompt by typing `psql`

Do the following:
```sql
CREATE DATABASE atlas;
CREATE USER atlas WITH PASSWORD 'atlas';
ALTER ROLE atlas SET client_encoding TO 'utf8';
ALTER ROLE atlas SET default_transaction_isolation TO 'read committed';
ALTER ROLE atlas SET timezone TO 'UTC';
GRANT ALL PRIVILEGES ON DATABASE atlas TO atlas;
\q
```

### Run database migrations
Migrations are like commits for database schemas. If you want to make a change to database schema,
you don't do it by writing SQL, instead, you prepare migrations and run them so that everyone working on
the project has the same database schemas.
```bash
./manage.py migrate
```

### Finally, run the development server
```bash
./manage.py runserver 0.0.0.0:8000
```

## LINUX

### Refresh your local package index before starting
```bash
sudo apt-get update
```

### Install Python3

[https://www.python.org/downloads/](https://www.python.org/downloads/)

or alternatively

```bash
sudo add-apt-repository ppa:deadsnakes/ppa
sudo apt-get update
sudo apt-get install python3.x
```
### Install pip by typing
```bash
sudo apt-get install python3-pip
```

### Once pip is installed, you can use it to install the virtualenv package.
```bash
sudo pip3 install virtualenv
```

### Go to 'backend' directory
```bash
cd Atlas/backend
```

### Create the virtual environment with python3
```bash
virtualenv --python=/usr/bin/python3.x atlasenv
```

### Get inside the virtual environment
```bash
source atlasenv/bin/activate
```

### Install Django
```bash
pip3 install -r requirements.txt
```

### Get postgres database
```bash
sudo apt-get install postgresql postgresql-contrib
sudo su -postgres
```

### Perform initial database setup
Enter postgresql prompt by typing `psql`

Do the following:
```sql
CREATE DATABASE atlas;
CREATE USER atlas WITH PASSWORD 'atlas';
ALTER ROLE atlas SET client_encoding TO 'utf8';
ALTER ROLE atlas SET default_transaction_isolation TO 'read committed';
ALTER ROLE atlas SET timezone TO 'UTC';
GRANT ALL PRIVILEGES ON DATABASE atlas TO atlas;
\q
```
### Run database migrations
Migrations are like commits for database schemas. If you want to make a change to database schema,
you don't do it by writing SQL, instead, you prepare migrations and run them so that everyone working on
the project has the same database schemas.
```bash
./manage.py migrate
```

### Finally, run the development server
```bash
./manage.py runserver 0.0.0.0:8000
```
