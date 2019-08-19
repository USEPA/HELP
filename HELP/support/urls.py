# urls.py (support)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov
# pylint: skip-file
# We disable the invalid name because urlpatterns is the Django default

"""Add docstring."""  #TODO add docstring.

from django.conf.urls import include, url
from .views import *
from support.views import *

app_name = 'support'

urlpatterns = [
    url(r'^$', index),
    url(r'^index/$', index, name='go_to_support'),

    url(r'^create/(?P<support_type_name>\w+)/$', SuggestionCreateView.as_view(), name='create_support'),
    url(r'^edit/(?P<support_type_name>\w+)/(?P<obj_id>\d+)/$', SuggestionEditView.as_view(), name='edit_support'),
    url(r'^delete/(?P<support_type_name>\w+)/(?P<obj_id>\d+)/$', delete_support, name='delete_support'),
    url(r'^list/(?P<support_type_name>\w+)/$', list_supports, name='list_supports'),
    url(r'^search/$', search_support, name='search_support'),
    url(r'^search/result/$', result_search_support, name='result_search_support'),
    url(r'^show/(?P<support_type_name>\w+)/(?P<obj_id>\d+)/$', show_support, name='show_support'),

    # new
    url(r'^file/upload/(?P<obj_id>\d+)/$', file_upload_support, name='file_upload_support'),
    url(r'^support_attachment/delete/(?P<obj_id>\d+)/$', delete_support_attachment, name='delete_support_attachment'),

    url(r'^type/create/$', create_support_type, name='create_support_type'),
    url(r'^type/edit/(?P<obj_id>\d+)/$', edit_support_type, name='edit_support_type'),
    url(r'^type/delete/(?P<obj_id>\d+)/$', delete_support_type, name='delete_support_type'),
    url(r'^type/list/$', list_support_types, name='list_support_types'),
    url(r'^type/search/$', search_support_type, name='search_support_type'),
    url(r'^type/search/result/$', result_search_support_type, name='result_search_support_type'),
    url(r'^type/show/(?P<obj_id>\d+)/$', show_support_type, name='show_support_type'),

    url(r'^priority/create/$', create_priority, name='create_priority'),
    url(r'^priority/edit/(?P<obj_id>\d+)/$', edit_priority, name='edit_priority'),
    url(r'^priority/delete/(?P<obj_id>\d+)/$', delete_priority, name='delete_priority'),
    url(r'^priority/list/$', list_priorities, name='list_priorities'),
    url(r'^priority/search/$', search_priority, name='search_priority'),
    url(r'^priority/search/result/$', result_search_priority, name='result_search_priority'),
    url(r'^priority/show/(?P<obj_id>\d+)/$', show_priority, name='show_priority'),

    url(r'^support/search/result/thirty/$', search_support_for_last_30, name='search_support_for_last_30'),
    url(r'^support/search/result/sixty/$', search_support_for_last_60, name='search_support_for_last_60'),
    url(r'^support/search/result/ninety/$', search_support_for_last_90, name='search_support_for_last_90'),
    url(r'^support/search/result/one_eighty/$', search_support_for_last_180, name='search_support_for_last_180'),
]
