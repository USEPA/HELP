# views.py (support)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: skip-file

"""
Views for managing user support functions.

Available functions:
- Create support ticket.
- Save changes to support form.
"""

import datetime
from datetime import datetime, timedelta
from math import *
from constants.models import *
from constants.utils import *
from django.conf import settings
from django.contrib.auth.decorators import login_required
from django.db.models import Q
from django.http import HttpResponseRedirect
from django.shortcuts import get_object_or_404
from django.shortcuts import render
from django.utils.decorators import method_decorator
from django.views.decorators.cache import never_cache
from django.views.generic import FormView
from .forms import *
from .models import *

getcontext().prec = 9
# testing


@login_required
def index(request):
    """User login support."""
    user = request.user
    title = "Support Main Page"

    return render(request, 'main/support.html', locals())


# Start Support


class SuggestionCreateView(FormView):
    """View to create a new support ticket."""

    form_class = SupportForm
    template = 'main/create_support.html'

    @method_decorator(login_required)
    def get(self, request, support_type_name):
        """Display the project create form."""
        if "suggestion" == support_type_name.lower():
            title = "Make a suggestion to improve GWSC"
            instructions = "Describe your suggestion for GWSC below.  You will " \
                           "have the option to add attachments after saving the suggestion."
        else:
            title = "Request help with GWSC"
            instructions = "Describe the problem you encountered with GWSC below.  You will " \
                           "have the option to add attachments after saving the request."
        form = self.form_class(initial={'weblink': request.user.email})
        return render(request, self.template, locals())

    @method_decorator(login_required)
    def post(self, request, support_type_name, *args, **kwargs):
        """Save the changes to the support form."""
        user = request.user
        form = self.form_class(data=request.POST, files=request.FILES)

        if form.is_valid():

            support = form.save(commit=False)
            support.user = user
            support.created_by = user
            support.last_modified_by = user
            # lookup ticket type
            support.support_type = SupportType.objects.filter(the_name__iexact=support_type_name).first()
            support.save()

            url = reverse('support:edit_support',
                          kwargs={'support_type_name': support_type_name, 'obj_id': support.id}) + "?new=1"
            return HttpResponseRedirect(url)
        else:
            return render(request, self.template, locals())


class SuggestionEditView(FormView):
    """View to create a new support ticket."""

    form_class = SupportForm
    admin_form_class = SupportAdminForm
    template = 'edit/edit_support.html'
    no_edit_template = 'main/no_edit.html'

    @method_decorator(login_required)
    def get(self, request, support_type_name, obj_id):
        """Add docstring."""  # TODO add docstring.
        user = request.user
        form_class = SupportForm if not user.is_staff else SupportAdminForm
        support = get_object_or_404(Support, id=obj_id)
        support_attachments = SupportAttachment.objects.filter(support=support)

        title = "Suggestion" if support_type_name == 'suggestion' else "Help"
        instructions = "Edit your request or add attachments below."
        if request.GET.get("new") is not None:
            warn = "You're almost done. Update your suggestion as needed, add attachments, " \
                   "and then click Submit to complete your request"

        # make sure the user has permission to edit this support object
        if not user.is_staff and support.user != user:
            return render(request, self.no_edit_template, locals())

        form = form_class(instance=support)
        return render(request, self.template, locals())

    @method_decorator(login_required)
    def post(self, request, support_type_name, obj_id, *args, **kwargs):
        """Save changes to user form."""
        user = request.user
        title = "Suggestion" if support_type_name == 'suggestion' else "Help"
        instructions = "Edit your request or add attachments below."
        form_class = SupportForm if not user.is_staff else SupportAdminForm
        support = get_object_or_404(Support, id=obj_id)
        support_attachments = SupportAttachment.objects.filter(support=support)

        # make sure the user has permission to edit this support object
        if not user.is_staff and support.user != user:
            return render(request, self.no_edit_template, locals())

        form = form_class(data=request.POST, files=request.FILES)
        if form.is_valid():

            support.modified = request.user
            support.subject = form.cleaned_data["subject"]
            support.the_description = form.cleaned_data["the_description"]
            support.weblink = form.cleaned_data["weblink"]
            if user.is_staff:
                support.date_resolved = form.cleaned_data["date_resolved"]
                support.review_notes = form.cleaned_data["review_notes"]

            support.save()

            # reload the attachment list
            support_attachments = SupportAttachment.objects.filter(support=support)

            # email the GWSC admins and cc the user
            # get type of ticket
            if support.support_type is not None:
                support_type_desc = support.support_type.the_name
            else:
                support_type_desc = support_type_name

            support_email = settings.SUPPORT_GSC_EMAIL
            submitter_email = support.weblink

            # Use this when testing so ordqatrack@epa.gov doesn't get test emails.
            # email_to = [submitter_email]
            email_to = [support_email, submitter_email]

            # detect if this is a new request versus editing
            # if it's an update, we add language to the subject and body to indicate that to the recipient.
            referer_url = request.META['HTTP_REFERER']
            if referer_url.endswith("?new=1"):
                email_subject = 'GWSC ' + str(support_type_desc) + ' ' + str(obj_id) + ': ' + str(support.subject)
                # email_body = support.the_description + '\n\nSubmitted by ' + support.weblink
                email_body = support.the_description
            else:
                email_subject = 'UPDATED: GWSC ' + str(support_type_desc) + ' ' + str(obj_id) + ': ' + str(
                    support.subject)
                email_body = "This automated email is to notify you that your support request has been updated.\r\n\r\nDescription:\r\n" + str(
                    support.the_description) + "\r\n\r\nDate Resolved: " + str(
                    support.date_resolved) + "\r\n\r\nReview Notes:\r\n" + str(support.review_notes)

            the_email = create_qt_email_message(email_subject, email_body, support_email, email_to)

            for att in support_attachments:
                the_email.attach_file(settings.MEDIA_ROOT + "/" + str(att.attachment))

            the_email.send(fail_silently=False)

            # Go to show detail page next.
            if support_type_name == 'suggestion':
                url = '/support/show/suggestion/%s/' % str(obj_id)
            else:
                url = '/support/show/help/%s/' % str(obj_id)

            return HttpResponseRedirect(url)

        else:
            return render(request, self.template, locals())


@login_required
@never_cache
def show_support(request, support_type_name, obj_id):
    """Support request view."""
    # return HttpResponse('<p>In Support Show for {0}</p>'.format(obj_id))

    user = request.user

    obj = get_object_or_404(Support, id=obj_id)
    support = Support.objects.get(id=obj_id)
    title = "Show Support"
    support_attachments = SupportAttachment.objects.filter(support=obj)

    # print("before render")

    return render(request, 'show/show_support.html', locals())


def search_support(request):
    """Search support tickets."""
    title = "Search For Support - With Results Shown"
    return render(request, 'list/list_support_issues.html', locals())


@login_required
def file_upload_support(request, obj_id):
    """Ensure user logged into app."""
    # return HttpResponse('<p>In file_upload_support for {0}</p>'.format('x'))
    user = request.user
    profile = user.userprofile
    support = Support.objects.get(id=obj_id)

    if user.is_staff or profile.can_edit == "Y":
        error_message = "You can add files"
    else:
        error_message = "You are not authorized to add files."
        back_link = "/support/show/"
        return render(request, 'error.html', locals())

    for new_file in request.FILES.getlist('upl'):
        # Create a new entry in our database
        support_attachment, created = SupportAttachment.objects.get_or_create(support=support, attachment=new_file,
                                                                              the_name=new_file.name, user=user,
                                                                              the_size=new_file.size)

    url = reverse('support:edit_support', kwargs={'obj_id': support.id})

    return HttpResponseRedirect(url)


@login_required
def create_help_request(request):
    """Create help in support tab."""
    user = request.user
    title = "Create a New Request"
    supports = Support.objects.all()

    if request.method == "POST":
        form = SupportForm(data=request.POST, files=request.FILES)
        if form.is_valid():
            # if 'files' in request.FILES:
            #     files = request.FILES['files']
            #     files.user = user

            support = form.save(commit=False)

            support.user = user
            support.created_by = user
            support.last_modified_by = user

            # lookup ticket type
            # support.support_type_id = 1;
            support.support_type = SupportType.objects.filter(the_name="Help").first()

            support.save()
            # send_mail('GWSC Support Request', 'A GWSC Support Request Has Been Submitted. Here is the description of the issue: %s' % str(support.the_description), support.weblink , ['qatrack@epa.gov'], fail_silently=False)
            url = reverse('support:edit_support', kwargs={'obj_id': support.id})
            return HttpResponseRedirect(url)
    else:
        form = SupportForm(initial={'weblink': user.email})

    return render(request, 'main/create_help_request.html', locals())


@login_required
def edit_support_admin(request, obj_id):
    """Edit ticket using Django admin."""
    user = request.user
    support = Support.objects.get(id=obj_id)

    # if user.is_superuser:
    if user.is_staff:

        print("edit as non-admin")
        # user created this ticket
        # can edit as non-admin
        if request.method == "POST":
            form = SupportAdminForm(data=request.POST, files=request.FILES, instance=support)
            if form.is_valid():
                support = form.save(commit=False)

                support.last_modified_by = user.username

                support.save()

                url = reverse('support:show_support', kwargs={'obj_id': support.id})
                return HttpResponseRedirect(url)
        else:
            form = SupportAdminForm(instance=support)
        return render(request, 'edit/admin/edit_support_admin.html', locals())
    else:
        print("not allowed to edit")
        # user unauthorized to edit this ticket
        return render(request, 'main/no_edit.html', locals())


@login_required
def delete_support(request, support_type_name, obj_id):
    """Delete ticket."""
    title = "Delete Support Issue"
    user = request.user
    support = get_object_or_404(Support, id=obj_id)

    # if support.user == user or user.is_superuser:
    if user.is_staff:
        support.delete()

    url = reverse('support:list_supports', kwargs={'support_type_name': support_type_name})
    return HttpResponseRedirect(url)


@login_required
def list_supports(request, support_type_name):
    """List tickets."""
    user = request.user
    title = "Support List"
    # supports = Support.objects.all()
    supports = Support.objects.filter(support_type__the_name__iexact=support_type_name)

    return render(request, 'list/list_support_issues.html', locals())


@login_required
def delete_support_attachment(request, obj_id):
    """Delete attachments for support tickets."""
    title = "Delete Support Attachment"
    user = request.user
    support_attachment = get_object_or_404(SupportAttachment, id=obj_id)
    support = support_attachment.support
    support_id = support.id

    profile = user.userprofile
    obj = get_object_or_404(Support, id=support_id)
    title = "Support"

    support_attachments = SupportAttachment.objects.filter(support=obj)

    try:
        if support_attachment.user == user or profile.user_type == "SUPER":
            support_attachment.delete()
        else:
            error = "You are not authorized to delete this attachment."
    except:
        error = "Failed to delete attachment. Please try again."

    return render(request, 'show/show_support.html', locals())


@login_required
def result_search_support(request):
    """Search tickets."""
    query = Q()
    query_show = ''
    x = int(0)
    y = int(0)
    title = "Search Support - With Results Shown"

    if request is None:
        return Support.objects.get(id=1)

    if 'a' in request.GET:
        a = request.GET['a']
        if a:
            query = query & (Q(name__startswith=a))
            query_show = query_show + "User Compound Library Name = " + str(a) + " "

    if 'b' in request.GET:
        b = request.GET['b']
        if b:
            query = query & (Q(cas_number__startswith=b))
            query_show = query_show + "CAS Number = " + str(b) + " "

    if 'c' in request.GET:
        c = request.GET['c']
        if c:
            query = query & (Q(formula__startswith=c))
            query_show = query_show + "Molecular Formula = " + str(c) + " "

    count_per_page = 50

    if 'w' in request.GET:
        w = request.GET['w']
        if 'next' in request.GET:
            next = request.GET['next']
            page = int(page) + 1
            pg = int(page)
        if 'previous' in request.GET:
            previous = request.GET['previous']
            page = int(page) - 1
            if page == 0:
                page = 1
            pg = int(page)
        if 'start' in request.GET:
            start = request.GET['start']
            pg = int(page)

        if pg > 1:
            y = pg * count_per_page

            if y < count_per_page:
                x = 0
            else:
                x = y - count_per_page
        else:
            pg = 1
            y = count_per_page
            x = 0
    else:
        pg = 1
        y = count_per_page
        x = 0

    query = query & (Q(make_public="Y"))

    if query:
        the_count = Support.objects.filter(query).count()
        max_page_number = int(floor(the_count / 50)) + 1
        if max_page_number <= pg:
            if the_count > count_per_page - 1:
                y = int(the_count)
                x = int(y - count_per_page)
            else:
                y = int(the_count)
                x = 0

        objs = Support.objects.filter(query)[x:y]
    else:
        objs = ""

    return render(request, 'main/support.html', locals())


@login_required
def search_support_for_last_30(request):
    """Search new tickets last 30-days."""
    user = request.user
    query = Q()
    query_show = 'Support Requests Received For Last 30 Days'

    title = "Support Requests Received Last 30 Days - With Results Shown"
    d = datetime.today() - timedelta(days=30)
    if user.is_staff:
        query = Q(created__gte=d)
    else:
        query = Q(created__gte=d) & Q(user=user)

    if query:
        the_count = Support.objects.filter(query).count()
        objs = Support.objects.filter(query)
    else:
        objs = ""

    return render(request, 'list/list_support_issues.html', locals())


@login_required
def search_support_for_last_60(request):
    """Search new tickets last 60-days.."""
    user = request.user
    query = Q()
    query_show = 'Support Requests Received For Last 60 Days'

    title = "Support Requests Received Last 60 Days - With Results Shown"
    d = datetime.today() - timedelta(days=60)
    if user.is_staff:
        query = Q(created__gte=d)
    else:
        query = Q(created__gte=d) & Q(user=user)

    if query:
        the_count = Support.objects.filter(query).count()
        objs = Support.objects.filter(query)
    else:
        objs = ""

    return render(request, 'list/list_support_issues.html', locals())


@login_required
def search_support_for_last_90(request):
    """Search new tickets last 90-days."""
    user = request.user
    query = Q()
    query_show = 'Support Requests Received For Last 90 Days'

    title = "Support Requests Received Last 90 Days - With Results Shown"
    d = datetime.today() - timedelta(days=90)
    if user.is_staff:
        query = Q(created__gte=d)
    else:
        query = Q(created__gte=d) & Q(user=user)

    if query:
        the_count = Support.objects.filter(query).count()
        objs = Support.objects.filter(query)
    else:
        objs = ""

    return render(request, 'list/list_support_issues.html', locals())


@login_required
def search_support_for_last_180(request):
    """Search new tickets last 180-days."""
    user = request.user
    query = Q()
    query_show = 'Support Requests Received For Last 180 Days'

    title = "Support Requests Received Last 180 Days - With Results Shown"
    d = datetime.today() - timedelta(days=180)
    if user.is_staff:
        query = Q(created__gte=d)
    else:
        query = Q(created__gte=d) & Q(user=user)

    if query:
        the_count = Support.objects.filter(query).count()
        objs = Support.objects.filter(query)
    else:
        objs = ""

    return render(request, 'list/list_support_issues.html', locals())


# End Support

# Start SupportType

@login_required
def create_support_type(request):
    """Ticket support type."""
    user = request.user
    title = "Create a New SupportType"
    support_types = SupportType.objects.all()

    if request.method == "POST":
        form = SupportTypeForm(data=request.POST, files=request.FILES)
        if form.is_valid():
            if 'files' in request.FILES:
                files = request.FILES['files']
                files.user = user

            support_type = form.save(commit=False)

            support_type.user = user
            support_type.created_by = user.username
            support_type.last_modified_by = user.username
            support_type.save()

            url = reverse('support:show_support_type', kwargs={'obj_id': support_type.id})
            return HttpResponseRedirect(url)
    else:
        form = SupportTypeForm()
    return render(request, 'create_office.html', locals())


@login_required
def edit_support_type(request, obj_id):
    """Edit support type."""
    user = request.user

    title = "Update SupportType"

    support_type = SupportType.objects.get(id=obj_id)
    support_types = SupportType.objects.all()

    if user.is_staff or user == support_type.user:
        pass
    else:
        return render(request, 'no_edit.html', locals())

    if support_type.user == user:
        if request.method == "POST":
            form = SupportTypeForm(data=request.POST, files=request.FILES, instance=support_type)
            if form.is_valid():
                support_type = form.save(commit=False)

                support_type.last_modified_by = user.username
                support_type.save()

                url = reverse('support:show_support_type', kwargs={'obj_id': support_type.id})
                return HttpResponseRedirect(url)
        else:
            form = SupportTypeForm(instance=support_type)
    else:
        url = '/accounts/not_authorized/'

    return render(request, 'edit_office.html', locals())


@login_required
def delete_support_type(request, obj_id):
    """Add docstring."""  # TODO add docstring.
    title = "Delete SupportType"
    user = request.user
    support_type = get_object_or_404(SupportType, id=obj_id)

    if support_type.user == user:
        support_type.delete()

    url = reverse('support:list_support_types')
    return HttpResponseRedirect(url)


@login_required
def list_support_types(request):
    """Add docstring."""  # TODO add docstring.
    user = request.user
    title = "SupportType List"
    support_types = SupportType.objects.all().order_by('ordering')
    return render(request, 'list/list_support_types.html', locals())


@login_required
@never_cache
def show_support_type(request, obj_id):
    """Add docstring."""  # TODO add docstring.
    user = request.user

    obj = get_object_or_404(SupportType, id=obj_id)
    title = "Show SupportType"

    support_types = SupportType.objects.all().order_by('ordering')
    return render(request, 'show/show.html', locals())


def search_support_type(request):
    """Add docstring."""  # TODO add docstring.
    title = "Search For SupportType - With Results Shown"
    return render(request, 'main/search_support_type.html', locals())


def result_search_support_type(request):
    """Add docstring."""  # TODO add docstring.
    query = Q()
    query_show = ''
    x = int(0)
    y = int(0)
    title = "Search SupportType - With Results Shown"

    if request is None:
        return SupportType.objects.get(id=1)

    if 'a' in request.GET:
        a = request.GET['a']
        if a:
            query = query & (Q(name__startswith=a))
            query_show = query_show + "User Compound Library Name = " + str(a) + " "

    if 'b' in request.GET:
        b = request.GET['b']
        if b:
            query = query & (Q(cas_number__startswith=b))
            query_show = query_show + "CAS Number = " + str(b) + " "

    if 'c' in request.GET:
        c = request.GET['c']
        if c:
            query = query & (Q(formula__startswith=c))
            query_show = query_show + "Molecular Formula = " + str(c) + " "

    count_per_page = 50

    if 'w' in request.GET:
        w = request.GET['w']
        if 'next' in request.GET:
            next = request.GET['next']
            page = int(page) + 1
            pg = int(page)
        if 'previous' in request.GET:
            previous = request.GET['previous']
            page = int(page) - 1
            if page == 0:
                page = 1
            pg = int(page)
        if 'start' in request.GET:
            start = request.GET['start']
            pg = int(page)

        if pg > 1:
            y = pg * count_per_page

            if y < count_per_page:
                x = 0
            else:
                x = y - count_per_page
        else:
            pg = 1
            y = count_per_page
            x = 0
    else:
        pg = 1
        y = count_per_page
        x = 0

    query = query & (Q(make_public="Y"))

    if query:
        the_count = SupportType.objects.filter(query).count()
        max_page_number = int(floor(the_count / 50)) + 1
        if max_page_number <= pg:
            if the_count > count_per_page - 1:
                y = int(the_count)
                x = int(y - count_per_page)
            else:
                y = int(the_count)
                x = 0

        objs = SupportType.objects.filter(query)[x:y]
    else:
        objs = ""

    return render(request, 'main/search_support_type.html', locals())


# End SupportType

# Start Priority

@login_required
def create_priority(request):
    """Add docstring."""  # TODO add docstring.
    user = request.user
    title = "Create a New Priority"
    priorities = Priority.objects.all()

    if request.method == "POST":
        form = PriorityForm(data=request.POST, files=request.FILES)
        if form.is_valid():
            if 'files' in request.FILES:
                files = request.FILES['files']
                files.user = user

            priority = form.save(commit=False)

            priority.user = user
            priority.created_by = user.username
            priority.last_modified_by = user.username
            priority.save()

            url = reverse('support:show_priority', kwargs={'obj_id': priority.id})
            return HttpResponseRedirect(url)
    else:
        form = PriorityForm()
    return render(request, 'create_office.html', locals())


@login_required
def edit_priority(request, obj_id):
    """Add docstring."""  # TODO add docstring.
    user = request.user

    title = "Update Priority"

    priority = Priority.objects.get(id=obj_id)
    priorities = Priority.objects.all()

    if user.is_staff or user == priority.user:
        pass
    else:
        return render(request, 'no_edit.html', locals())

    if priority.user == user:
        if request.method == "POST":
            form = PriorityForm(data=request.POST, files=request.FILES, instance=priority)
            if form.is_valid():
                priority = form.save(commit=False)

                priority.last_modified_by = user.username
                priority.save()

                url = reverse('support:show_priority', kwargs={'obj_id': priority.id})
                return HttpResponseRedirect(url)
        else:
            form = PriorityForm(instance=priority)
    else:
        url = '/accounts/not_authorized/'

    return render(request, 'edit_office.html', locals())


@login_required
def delete_priority(request, obj_id):
    """Add docstring."""  # TODO add docstring.
    title = "Delete Priority"
    user = request.user
    priority = get_object_or_404(Priority, id=obj_id)

    if priority.user == user:
        priority.delete()

    url = reverse('support:list_priorities')
    return HttpResponseRedirect(url)


@login_required
def list_priorities(request):
    """Add docstring."""  # TODO add docstring.
    user = request.user
    title = "Priority List"
    priorities = Priority.objects.all().order_by('ordering')
    return render(request, 'list/list_priorities.html', locals())


@login_required
@never_cache
def show_priority(request, obj_id):
    """Add docstring."""  # TODO add docstring.
    user = request.user

    obj = get_object_or_404(Priority, id=obj_id)
    title = "Show Priority"

    return render(request, 'show/show.html', locals())


def search_priority(request):
    """Add docstring."""  # TODO add docstring.
    title = "Search For Priority - With Results Shown"
    return render(request, 'main/search_priority.html', locals())


def result_search_priority(request):
    """Add docstring."""  # TODO add docstring.
    query = Q()
    query_show = ''
    x = int(0)
    y = int(0)
    title = "Search Priority - With Results Shown"

    if request is None:
        return Priority.objects.get(id=1)

    if 'a' in request.GET:
        a = request.GET['a']
        if a:
            query = query & (Q(name__startswith=a))
            query_show = query_show + "User Compound Library Name = " + str(a) + " "

    if 'b' in request.GET:
        b = request.GET['b']
        if b:
            query = query & (Q(cas_number__startswith=b))
            query_show = query_show + "CAS Number = " + str(b) + " "

    if 'c' in request.GET:
        c = request.GET['c']
        if c:
            query = query & (Q(formula__startswith=c))
            query_show = query_show + "Molecular Formula = " + str(c) + " "

    count_per_page = 50

    if 'w' in request.GET:
        w = request.GET['w']
        if 'next' in request.GET:
            next = request.GET['next']
            page = int(page) + 1
            pg = int(page)
        if 'previous' in request.GET:
            previous = request.GET['previous']
            page = int(page) - 1
            if page == 0:
                page = 1
            pg = int(page)
        if 'start' in request.GET:
            start = request.GET['start']
            pg = int(page)

        if pg > 1:
            y = pg * count_per_page

            if y < count_per_page:
                x = 0
            else:
                x = y - count_per_page
        else:
            pg = 1
            y = count_per_page
            x = 0
    else:
        pg = 1
        y = count_per_page
        x = 0

    query = query & (Q(make_public="Y"))

    if query:
        the_count = Priority.objects.filter(query).count()
        max_page_number = int(floor(the_count / 50)) + 1
        if max_page_number <= pg:
            if the_count > count_per_page - 1:
                y = int(the_count)
                x = int(y - count_per_page)
            else:
                y = int(the_count)
                x = 0

        objs = Priority.objects.filter(query)[x:y]
    else:
        objs = ""

    return render(request, 'main/search_priority.html', locals())

    # End Priority
