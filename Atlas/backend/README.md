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

### In project directory, run the following:
```bash
pip3 install -r requirements.txt
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
pip install django # We always use pip inside virtualenv, not pip3
```

### Get postgres database
Download from [here](https://postgresapp.com/)
Make sure `psql` from commandline works

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
