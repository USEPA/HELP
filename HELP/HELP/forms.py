# forms.py (app)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov


"""Definition of forms."""

from django import forms
from django.utils.translation import ugettext_lazy as _
from HELP.util_models import Ticket


class TicketForm(forms.Form):
    """@class TicketForm. Form for submitting ticket issues."""

    INQUIRY_CHOICES = (
        ("Request to Add a New Feature", "Request to Add a New Feature"),
        ("Bug Tracking", "Bug Tracking"))
    issue = forms.CharField(label=_("Issue:"), required=True,
                            widget=forms.TextInput(
                                attrs={'class': 'form-control'}))
    subject = forms.CharField(label=_("Subject:"), required=True,
                              widget=forms.TextInput(
                                  attrs={'class': 'form-control'}))
    type = forms.ChoiceField(label=_("Inquiry Type:"), choices=INQUIRY_CHOICES,
                             widget=forms.Select(
                                 attrs={'class': 'form-control'}),
                             required=False)
    description = forms.CharField(label=_("Problem Description:"),
                                  required=True, widget=forms.Textarea(
                                      attrs={'class': 'form-control', 'rows': 5}))
    email = forms.EmailField(label=_("Your Email:"), required=True,
                             widget=forms.TextInput(
                                 attrs={'class': 'form-control'}))

    class Meta:
        """Ticket fields for support."""

        model = Ticket
        fields = ('issue', 'subject', 'type', 'description', 'email')
