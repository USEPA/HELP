# local_settings.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov

"""
Local Django settings for HELP project.

Available functions:
NOTE: All development uses 'localhost' not private or EPA servers
- Email settings
- PostgreSQL database settings
"""

SITE_NAME = 'localhost'

# Settings when installing HELP on the EPA RedHat email Server.
DEFAULT_FROM_EMAIL = 'dyoung11@engineering4sustainability.com'
EMAIL_HOST = '127.0.0.1'
EMAIL_HOST_USER = 'dyoung11'
EMAIL_HOST_PASSWORD = 'password'
EMAIL_PORT = 25
EMAIL_FILE_PATH = '/var/www/html/gemm/uploads'

EMAIL_SUPPORT = 'young.daniel@epa.gov'

USER_CONFIRMATION_EMAILS = [
    'young.daniel@epa.gov',
    'jacob@gqc.com',
    'raghuraman.venkatapathy@ptsied.com',
]

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = 'z&&=(=sm60$x+8asdkcgfehrgc5k^5^_6q@=-uychcf$j1a-&d53u!('

# SECURITY WARNING: do not run with debug turned on in production!
DEBUG = True

if DEBUG is True:
    ALLOWED_HOSTS = ['127.0.0.1', 'localhost', 'testserver']
else:
    ALLOWED_HOSTS = ['127.0.0.1', 'localhost', '.engineering4sustainability.com', 'testserver']

# This sets URL to run as localhost for development on VS 2019
BASE_URL = 'http://127.0.0.1'
DJANGO_SETTINGS_MODULE = 'HELP.settings'

ROOT_URLCONF = 'HELP.urls'

# To run on localhost password needs to be set.
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql_psycopg2',
        'NAME': 'help',
        'USER': 'postgres',
        'PASSWORD': 'Evelynj1!',
        'HOST': 'localhost',
        'PORT': '5432'
    }
}

# From GEMM local_settings, trying to get Admin to work properly.
STATIC_ROOT = 'HELP/static/'
STATIC_URL = '/static/'
