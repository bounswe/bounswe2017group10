### Go to backend directory
```bash
cd Atlas/atlasAnnoServer
```

### Run the following:
```bash
pip3 install virtualenv
```

### Create the virtual environment
```bash
virtualenv annotationenv
```

### Get inside virtual environment
```bash
source annotationenv/bin/activate
```

### Install Django
```bash
pip install -r requirements.txt # We always use pip inside virtualenv, not pip3
```

### Perform initial database setup
Enter postgresql prompt by typing `psql`

Do the following:
```sql
CREATE DATABASE annotator;
CREATE USER atlas WITH PASSWORD 'annotator';
ALTER ROLE annotator SET client_encoding TO 'utf8';
ALTER ROLE annotator SET default_transaction_isolation TO 'read committed';
ALTER ROLE annotator SET timezone TO 'UTC';
GRANT ALL PRIVILEGES ON DATABASE annotator TO annotator;
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