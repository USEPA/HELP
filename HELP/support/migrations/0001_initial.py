# Generated by Django 2.1.3 on 2018-11-19 18:27
# pylint: skip-file
# SKIP THIS FILE BECAUSE IT IS GENERATED BY DJANGO

from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion
import support.models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='Priority',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created', models.DateTimeField(auto_now_add=True, null=True)),
                ('modified', models.DateTimeField(auto_now=True, null=True)),
                ('created_by', models.CharField(blank=True, max_length=255, null=True)),
                ('last_modified_by', models.CharField(blank=True, max_length=255, null=True)),
                ('the_name', models.CharField(blank=True, max_length=255, null=True)),
                ('the_description', models.TextField(blank=True, null=True)),
                ('weblink', models.CharField(blank=True, max_length=255, null=True)),
                ('ordering', models.DecimalField(blank=True, decimal_places=1, max_digits=10, null=True)),
                ('user', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'ordering': ['ordering'],
            },
        ),
        migrations.CreateModel(
            name='Support',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created', models.DateTimeField(auto_now_add=True, null=True)),
                ('modified', models.DateTimeField(auto_now=True, null=True)),
                ('make_public', models.BooleanField(default=False)),
                ('share_with_user_group', models.BooleanField(default=False)),
                ('attachment', models.FileField(blank=True, null=True, upload_to=support.models.get_support_storage_path)),
                ('the_name', models.CharField(blank=True, max_length=255, null=True)),
                ('subject', models.CharField(blank=True, max_length=255, null=True)),
                ('length_of_reference', models.CharField(blank=True, max_length=255, null=True)),
                ('author', models.CharField(blank=True, max_length=255, null=True)),
                ('is_closed', models.BooleanField(default=False)),
                ('the_description', models.TextField(blank=True, null=True)),
                ('resolution', models.TextField(blank=True, null=True)),
                ('weblink', models.CharField(blank=True, max_length=255, null=True)),
                ('ordering', models.DecimalField(blank=True, decimal_places=1, max_digits=10, null=True)),
                ('date_resolved', models.DateField(blank=True, null=True)),
                ('status', models.CharField(choices=[('To Do', 'To Do'), ('In Progress', 'In Progress'), ('Done', 'Done')], default='To Do', max_length=25)),
                ('review_notes', models.TextField(blank=True, null=True)),
                ('created_by', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, related_name='support_created_by', to=settings.AUTH_USER_MODEL)),
                ('last_modified_by', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, related_name='support_last_modified_by', to=settings.AUTH_USER_MODEL)),
                ('priority', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='support.Priority')),
            ],
            options={
                'ordering': ['ordering'],
            },
        ),
        migrations.CreateModel(
            name='SupportAttachment',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created', models.DateTimeField(auto_now_add=True, null=True)),
                ('modified', models.DateTimeField(auto_now=True, null=True)),
                ('created_by', models.CharField(blank=True, max_length=255, null=True)),
                ('last_modified_by', models.CharField(blank=True, max_length=255, null=True)),
                ('attachment', models.FileField(blank=True, max_length=255, null=True, upload_to=support.models.get_support_attachment_storage_path)),
                ('the_name', models.CharField(blank=True, max_length=255, null=True)),
                ('the_size', models.CharField(blank=True, max_length=255, null=True)),
                ('support', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, related_name='support_attachments', to='support.Support')),
                ('user', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'ordering': ['the_name'],
            },
        ),
        migrations.CreateModel(
            name='SupportType',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created', models.DateTimeField(auto_now_add=True, null=True)),
                ('modified', models.DateTimeField(auto_now=True, null=True)),
                ('created_by', models.CharField(blank=True, max_length=255, null=True)),
                ('last_modified_by', models.CharField(blank=True, max_length=255, null=True)),
                ('the_name', models.CharField(blank=True, max_length=255, null=True)),
                ('the_description', models.TextField(blank=True, null=True)),
                ('weblink', models.CharField(blank=True, max_length=255, null=True)),
                ('ordering', models.DecimalField(blank=True, decimal_places=1, max_digits=10, null=True)),
                ('user', models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'ordering': ['ordering'],
            },
        ),
        migrations.AddField(
            model_name='support',
            name='support_type',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to='support.SupportType'),
        ),
        migrations.AddField(
            model_name='support',
            name='user',
            field=models.ForeignKey(blank=True, null=True, on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL),
        ),
    ]