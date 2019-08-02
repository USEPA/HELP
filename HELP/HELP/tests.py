# tests.py (app)
# !/usr/bin/env python3
# coding=utf-8
# pylint: skip-file
# We skip this file because it wasn't written by/for EPA.

"""
This file demonstrates writing tests using the unittest module.

These will pass when you run "manage.py test".
"""

import django
from django.test import TestCase

# TODO: Configure your database in settings.py and sync before running tests.


class ViewTest(TestCase):
    """Tests for the application views."""

    if django.VERSION[:2] >= (1, 7):
        # Django 1.7 requires an explicit setup() when running tests in PTVS
        @classmethod
        def setUpClass(cls):
            """Add docstring."""  # TODO add docstring.
            super(ViewTest, cls).setUpClass()
            django.setup()

    def test_home(self):
        """Tests the home page."""
        response = self.client.get('/')
        self.assertContains(response, 'Home Page', 1, 200)

    def test_contact(self):
        """Tests the contact page."""
        response = self.client.get('/contact')
        self.assertContains(response, 'Contact', 3, 200)

    def test_about(self):
        """Tests the about page."""
        response = self.client.get('/about')
        self.assertContains(response, 'About', 3, 200)

    def test_dashboard(self):
        """Tests the dashboard page."""
        response = self.client.get('/dashboard')
        self.assertContains(response, 'Dashboard', 3, 200)

    def test_support(self):
        """Tests the support page."""
        response = self.client.get('/support')
        self.assertContains(response, 'Support', 3, 200)
