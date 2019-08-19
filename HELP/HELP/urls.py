# urls.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: disable=invalid-name
# We disable the invalid name because urlpatterns is the Django default


"""Definition of urls for HELP."""

from datetime import datetime
from django.urls import include, path
from django.contrib import admin
from django.contrib.auth.views import LoginView, LogoutView
from HELP import forms, views


urlpatterns = [
    path('', views.home, name='home'),
    path('contact/', views.contact, name='contact'),
    path('about/', views.about, name='about'),
    path('dashboard/', views.dashboard, name='dashboard'),
    path('help/', views.help, name='help'),
    path('admin/', admin.site.urls),
    path('accounts/', include('accounts.urls')),
    path('support/', include('support.urls')),
    path(r'suggestions/', views.ticket, name='ticket'),
    path(r'usermanual/', views.usermanual, name='usermanual'),
]
