# views.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov


"""Definition of views."""

from datetime import datetime
from django.shortcuts import render
from django.http import HttpRequest


def home(request):
    """Renders the home page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'index.html',
        {
            'title': 'Home Page',
            'year': datetime.now().year,
        }
    )


def contact(request):
    """Renders the contact page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'contact.html',
        {
            'title': 'US Environmental Protection Agency',
            'message': 'Office of Research & Development.',
            'year': datetime.now().year,
        }
    )


def about(request):
    """Renders the about page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'about.html',
        {
            'title': 'HELP',
            'message': 'Hydrologic Evaluation of Landfill Performance (HELP).',
            'year': datetime.now().year,
        }
    )

def dashboard(request):
    """Renders the dashboard page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'dashboard.html',
        {
            'title': 'HELP',
            'message': 'Hydrologic Evaluation of Landfill Performance (HELP).',
            'year': datetime.now().year,
        }
    )

def support(request):
    """Renders the support page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'support.html',
        {
            'title': 'HELP',
            'message': 'Hydrologic Evaluation of Landfill Performance (HELP).',
            'year': datetime.now().year,
        }
    )

def FortranC(request):
    """Renders the software page."""
    assert isinstance(request, HttpRequest)
    return render(
        request,
        'FortranC.html',
        {
            'title': 'HELP',
            'message': 'Software Launch.',
            'year': datetime.now().year,
        }
    )
