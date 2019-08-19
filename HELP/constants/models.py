# models.py (constants)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov

"""
Models related to constants.

This file contains constants to be shared across
other apps in the project.

Available functions:
- None
"""

DEGREE = chr(176)
NAUGHT = u'\u2092'
BETA_CHAR = chr(946)
ETA_CHAR = chr(951)
THREE_SUPER = u'\u00B3'
TWO_SUPER = u'\u00B2'


# this is used only i the equipment app - don't know yet whether that app is
# used in GWSC or not
METROLOGY_DOC_CHOICES = (
    ('Other', 'Other'), ('CP', 'Calibration Procedure'), ('MD', 'Metrology Document'), ('TM', 'Technical Manual'),
    ('Image', 'Image'), ('Thumbnail', 'Thumbnail'), ('Key', 'Key Image - BIG'),)

PUBLIC_CHOICES = (('', ''), ('PUBLIC', 'PUBLIC'), ('PRIVATE', 'PRIVATE'))
RAP_CHOICES = (
    ('', ''), ('ACE', 'ACE'), ('CSS', 'CSS'), ('SSWR', 'SSWR'), ('HHRA', 'HHRA'), ('HSR', 'HSR'), ('SHC', 'SHC'),
    ('Not Applicable', 'Not Applicable'),)
STATUS_CHOICES = (('', ''), ('Active', 'Active'), ('Inactive', 'Inactive'))

USER_TYPE_CHOICES = (
    ('', ''), ('SUPER', 'SUPER USER'), ('ALL', 'ALL LABS'), ('LAB', 'SINGLE LAB'), ('DIVISION', 'DIVISION USER'),
    ('BRANCH', 'BRANCH USER'))

YN_CHOICES = (('', ''), ('Y', 'Yes'), ('N', 'No'))
YNNA_CHOICES = (('', ''), ('Y', 'Y'), ('N', 'N'), ('NA', 'NA'))
BOOLEAN_YES_NO = ((False, 'No'), (True, 'Yes'))

EXPORT_CHOICES = (
    ('CSV', 'CSV'),
    ('XLS', 'Excel'),
    ('JSON', 'JSON'),
    ('Tilde', 'Tilde'))

# This is used to preserve the fields in the string that logs correspondence (audit, SOPs).
# Then we can split the fields back out to populate a table.
CORRESPONDENCE_LOG_SPLITTER = ' | '
