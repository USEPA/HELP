
# util_models.py (HELP)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov

"""
@file util_models.py.

Utility models/classes for the HELP application.
The models in this file do not belong in the database and are kept
separate from the other Django Models.
"""


class Ticket:
    """@class Ticket. Represents an issue ticket submitted by a user."""

    issue = str()
    subject = str()
    type = str()
    description = str()
    email = str()
