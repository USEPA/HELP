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
from os import path
import tempfile
from HELP.forms import TicketForm, Ticket, SimulationForm
from HELP import settings
from HELP.pyhelp_interface import run_help
from pyhelpinterface.managers import HelpManager


def help(request):
    """Renders the software page."""
    if not request.user.is_authenticated:
        return render(request, 'main/base.html', {})

    assert isinstance(request, HttpRequest)

    ctx = {'title': 'HELP',
           'message': 'Run a Simulation',
           'year': datetime.now().year,}

    if request.method != 'POST':
        ctx['form'] = SimulationForm()
        return render(request, 'help.html', ctx)

    form = SimulationForm(request.POST, request.FILES)
    if(form.is_valid()):
        # Set up a temporary directory:
        with tempfile.TemporaryDirectory() as workdir:
            # Save all the input files as tempfiles into the tempdir.
            # This is the quickest way to get started with PyHELP, but a more complete and
            # more efficient solution would be to modify the PyHELP functions to accept the files
            # themselves, instead of accepting a directory.
            with open(join(workdir, 'airtemp_input_data.csv')) as dest:
                for chunk in request.FILES['airtemp_input_data']:
                    dest.write(chunk)
            with open(join(workdir, 'precip_input_data.csv')) as dest:
                for chunk in request.FILES['precip_input_data']:
                    dest.write(chunk)
            with open(join(workdir, 'solrad_input_data.csv')) as dest:
                for chunk in request.FILES['solrad_input_data']:
                    dest.write(chunk)
            with open(join(workdir, 'input_grid.csv')) as dest:
                for chunk in request.FILES['input_grid']:
                    dest.write(chunk)

            # Instantiate HelpManager and calculate  monthly water budget for 2000-2010 period.
            helpm = HelpManager(tempdir, year_range=(2000, 2010))
            output = run_help(helpm)

            # Export and save the data to an ESRI shapefile.
            help_output_shp = join(workdir, 'help_example.shp')
            output.save_to_shp(help_output_shp)

            # Plot some results.
            output.plot_area_monthly_avg()
            output.plot_area_yearly_avg()
            output.plot_area_yearly_series()

            # Calculate the yearly water budget for surface water cells.
            evp_surf = 650
            surf_output_hdf5 = join(workdir, 'surf_example.out')
            output_surf = helpm.calc_surf_water_cells(evp_surf, surf_output_hdf5)

    return render(request, 'help.html', ctx)


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


def submit_ticket(p_ticket):
    """Take data from the Ticket form and submits it."""
    print('sending emails to ')
    print(p_ticket.email)
    print(' from ')
    print(settings.DEFAULT_FROM_EMAIL)
    subject = p_ticket.type + ": " + p_ticket.subject
    body = "New HELP support message from " + p_ticket.email + "\n\n"
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
            # or USER should be redirected to the WHAT IS HELP? Page/tab.
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
    """Receives GET requests and will start download of HELP user manual stored on the server."""
    return download_file(request, settings.MANUAL_NAME)
