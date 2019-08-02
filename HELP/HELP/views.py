# views.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov


"""Definition of views."""

from datetime import datetime
from django.contrib.auth.decorators import login_required
from django.core.mail import EmailMessage
from django.shortcuts import render
from django.http import HttpRequest, HttpResponse
from os.path import join
from HELP.forms import TicketForm, Ticket
from HELP import settings


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
        'main/about.html',
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


def submit_ticket(p_ticket):
    """Take data from the Ticket form and submits it."""
    print('sending emails to ')
    print(p_ticket.email)
    print(' from ')
    print(settings.DEFAULT_FROM_EMAIL)
    subject = p_ticket.type + ": " + p_ticket.subject
    body = "New GEMM support message from " + p_ticket.email + "\n\n"
    body += p_ticket.description
    email = EmailMessage(
        subject,
        body,
        settings.DEFAULT_FROM_EMAIL,
        [settings.EMAIL_SUPPORT]
    )
    result = email.send()
    return result


def ticket(request):
    """
    Receives POST and GET requests.
    On GET, method returns a blank ticket form.
    On POST, method calls the submit_ticket method and returns ticket form page again.
    """
    if not request.user.is_authenticated:
        return render(request, 'main/base.html', {})
    if request.method == 'POST':
        form = TicketForm(request.POST)
        if form.is_valid():
            temp_ticket = Ticket()
            temp_ticket.issue = form.cleaned_data['issue']
            temp_ticket.subject = form.cleaned_data['subject']
            temp_ticket.type = form.cleaned_data['type']
            temp_ticket.description = form.cleaned_data['description']
            temp_ticket.email = form.cleaned_data['email']
            results = submit_ticket(temp_ticket)
            # Once the ticket is submitted either all FIELDS should be cleared
            # or USER should be redirected to the WHAT IS GEMM? Page/tab.
            # Check that result is OK before redirecting to about. If not okay,
            # return back to ticket page.
            if results > 0:
                messages.info(request, 'Your ticket has been submitted.')
                return render(request, 'main/about.html', locals())

            messages.info(request,
                          'Something went wrong with your ticket, please \
                           try again.')
            return render(request, 'support/ticket.html',
                          {'results': results, 'form': form})

    else:
        form = TicketForm()
        return render(request, 'support/ticket.html', {'form': form})

    messages.info(request,
                  'Something went wrong with your ticket, please try again.')
    return render(request, 'support/ticket.html', {'form': form})


def download_file(response, name):
    """Receives the path, name, and extension of a file to be returned to the user."""
    name_split = name.split('.')
    ext = name_split[len(name_split) - 1]
    file = join(settings.DOWNLOADS_DIR, name)

    if ext == 'pdf':
        with open(file, 'rb') as pdf:
            response = HttpResponse(pdf)
            response['Content-Disposition'] = 'attachment; filename="' + name + '"'
            return response

    elif ext == 'docx':
        with open(file, 'rb') as doc:
            response = HttpResponse(doc)
            response['Content-Disposition'] = 'attachment; filename="' + name + '"'
            return response

    elif 'xls' in ext:
        with open(file, 'rb') as xls:
            response = HttpResponse(
                xls, content_type="application/vnd.vnd.openxmlformats-officedocument.spreadsheetml.sheet")
            response['Content-Disposition'] = 'attachment; filename="' + name + '"'
            return response

    response.status_code = 404
    return render(response, 'main/404.html', {})


@login_required
def usermanual(request):
    """Receives GET requests and will start download of GEMM user manual stored on the server."""
    return download_file(request, settings.MANUAL_NAME)
