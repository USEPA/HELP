# forms.py (support)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: skip-file

"""
Form used to manage support issues.

Available functions:
"""

from django import forms
from django.forms import widgets, ModelForm, ValidationError
from django.forms.models import inlineformset_factory
from django.forms.widgets import SelectDateWidget

from constants.models import *

from support.models import *

from django.contrib.auth.models import User
from django.contrib.auth import authenticate
from django.contrib.auth.tokens import default_token_generator
from django.template import Context, loader
from django.utils.translation import ugettext_lazy as _
from django.utils.http import int_to_base36


class SupportForm(forms.ModelForm):
    """A Form For Creating a Support Issue."""

    def __init__(self, *args, **kwargs):
        """Support form to send to Django Admin."""
        super(SupportForm, self).__init__(*args, **kwargs)

    required_css_class = 'required'

    id = forms.CharField(label=_("Reference Num"),
                         widget=forms.TextInput(attrs={'class': 'form-control', 'readonly': 'readonly'}),
                         required=False)
    subject = forms.CharField(label=_("Subject"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                              required=True)

    the_description = forms.CharField(label=_("Description"), widget=forms.Textarea(attrs={'class': 'form-control'}),
                                      required=True)
    weblink = forms.CharField(label=_("Email Address"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                              required=True)

    class Meta:
        """Support link."""

        model = Support
        fields = ("id", "subject", "the_description", "weblink",)


class SupportAdminForm(forms.ModelForm):
    """A Form For Responding To a Support Issue."""

    def __init__(self, *args, **kwargs):
        """Support Django Admin form."""
        super(SupportAdminForm, self).__init__(*args, **kwargs)

    required_css_class = 'required'

    id = forms.CharField(label=_("Reference Num"),
                         widget=forms.TextInput(attrs={'class': 'form-control', 'readonly': 'readonly'}),
                         required=False)
    subject = forms.CharField(label=_("Subject"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                              required=True)
    date_resolved = forms.DateField(label=_("Date Resolved"),
                                    widget=forms.TextInput(attrs={'class': 'form-control date-control'}),
                                    required=False)
    the_description = forms.CharField(label=_("Description"), widget=forms.Textarea(attrs={'class': 'form-control'}),
                                      required=True)
    weblink = forms.CharField(label=_("Email Address"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                              required=True)
    review_notes = forms.CharField(label=_("Review Notes"), widget=forms.Textarea(attrs={'class': 'form-control'}),
                                   help_text="Notes from review of suggestion", required=False)

    class Meta:
        """All fields to complete support form."""

        model = Support
        fields = ("id", "subject", "the_description", "weblink", "date_resolved", "review_notes",)


class SupportTypeForm(forms.ModelForm):
    """A Form For Creating a Support Issue."""

    def __init__(self, *args, **kwargs):
        """Form type."""
        super(SupportTypeForm, self).__init__(*args, **kwargs)

    required_css_class = 'required'
    the_name = forms.CharField(label=_("Support Type"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                               required=False)

    class Meta:
        """Name form."""

        model = SupportType
        fields = ("the_name",)


class PriorityForm(forms.ModelForm):
    """A Form For Creating a Support Issue."""

    def __init__(self, *args, **kwargs):
        """Form priority."""
        super(PriorityForm, self).__init__(*args, **kwargs)

    the_name = forms.CharField(label=_("Priority"), widget=forms.TextInput(attrs={'class': 'form-control'}),
                               required=False)

    class Meta:
        """Form priority.."""

        model = Priority
        fields = ("the_name",)
