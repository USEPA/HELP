# settings.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: skip-file
"""
Django settings for HELP project.

Based on 'django-admin startproject' using Django 2.1.2.

For more information on this file, see
https://docs.djangoproject.com/en/2.1/topics/settings/

For the full list of settings and their values, see
https://docs.djangoproject.com/en/2.1/ref/settings/
"""

import os
import posixpath

DEFAULT_FROM_EMAIL = 'dyoung11@engineering4sustainability.com'
EMAIL_HOST = '127.0.0.1'
EMAIL_HOST_USER = 'dyoung11'
EMAIL_HOST_PASSWORD = 'OVERWRITE_IN_LOCAL_SETTINGS.PY'
EMAIL_PORT = 25
EMAIL_FILE_PATH = '/var/www/html/help/uploads'

EMAIL_SUPPORT = 'young.daniel@epa.gov'

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/2.1/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = '6dcc2b90-89f1-4fcb-878a-abeeb676e5b4'

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = True

ALLOWED_HOSTS = []

# Application references
# https://docs.djangoproject.com/en/2.1/ref/settings/#std:setting-INSTALLED_APPS
INSTALLED_APPS = [
    # Add your apps here to enable them
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.humanize',
    'accounts',
    'support',
    'HELP',
]

# Middleware framework
# https://docs.djangoproject.com/en/2.1/topics/http/middleware/
MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
]

ROOT_URLCONF = 'HELP.urls'

# Template configuration
# https://docs.djangoproject.com/en/2.1/topics/templates/
TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'HELP.wsgi.application'
# Database - OVERRIDE THIS IN local_setting.py!!!
# Connection string is overridden
# https://docs.djangoproject.com/en/1.11/ref/settings/#databases

# Password validation
# https://docs.djangoproject.com/en/2.1/ref/settings/#auth-password-validators
AUTH_PASSWORD_VALIDATORS = [
    {
        'NAME':
        'django.contrib.auth.password_validation.UserAttributeSimilarityValidator',
    },
    {
        'NAME':
        'django.contrib.auth.password_validation.MinimumLengthValidator',
    },
    {
        'NAME':
        'django.contrib.auth.password_validation.CommonPasswordValidator',
    },
    {
        'NAME':
        'django.contrib.auth.password_validation.NumericPasswordValidator',
    },
]

# Internationalization
# https://docs.djangoproject.com/en/2.1/topics/i18n/
LANGUAGE_CODE = 'en-us'
TIME_ZONE = 'UTC'
USE_I18N = True
USE_L10N = True
USE_TZ = True

# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/2.1/howto/static-files/
STATIC_URL = '/static/'
STATIC_ROOT = posixpath.join(*(BASE_DIR.split(os.path.sep) + ['static']))

DOWNLOADS_DIR = os.path.join("..", "documents")
MANUAL_NAME = 'user_manual.pdf'

APP_NAME = 'HELP'
APP_NAME_SHORT = 'Hydrologic Evaluation of Landfill Performance'

APP_NAME_LONG = 'Hydrologic Evaluation of Landfill Performance (HELP) Model Software web-based application'

APP_DESCRIPTION = 'HELP is developed and maintained by National Risk Management Research Laboratory Office \
                   of Research and Development U.S. Environmental Protection Agency, Cincinnati, OH 45268.'

APP_VERSION = '0.0.01'

APP_DISCLAIMER = 'Copyright (c) 2019. The United States Environmental Protection Agency (USEPA), through its Office of Research and Development, \
                  contributed to the development of the Hydrologic Evaluation of Landfill Performance (HELP) Model Software web-based application. \
                  The simulations have not been subjected to full Agency review and no official endorsement of their use should be inferred. \
                  Futhermore, the simulations were developed for demonstrative purposes only and have not been fully vetted according to the standards \
                  of Sanitary Landfill Design and Operations and/or Toxic Substances Control Act of 1976 (TSCA) Landfill Inspection. The user assumes \
                  responsibility for any application of these inventories in TSCA activities and USEPA, or its collaborators in the underlying research, \
                  can not be held liable for any decision outcomes involving the data. The United States Environmental Protection Agency (EPA) GitHub \
                  project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the \
                  information and no longer has the responsibility to protect theintegrity, confidentiality, or availability of the information. Any \
                  reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not \
                  constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to \
                  imply endorsement of any commercial product or activity by EPA or the United States Government. \
                  https://www.epa.gov/webguide/github-guidance#open.'

APP_ENDORSEMENT = 'Disclaimer of Endorsement. Reference on this website to any specific commercial products, process, or service by trade name, trademark, \
                   manufacturer, or otherwise, does not necessarily constitute or imply its endorsement, recommendation, or favoring by the United States \
                   Government. The presence of external hyperlinks does not constitute endorsement by the EPA, its employees, or contractors, of such external \
                   websites or the information contained therein. Furthermore, the views and opinions of authors expressed herein do not necessarily state \
                   or reflect those of the United States Government, and shall not be used for advertising or product endorsement purposes.'

try:
    from .local_settings import *
except ImportError:
    pass
