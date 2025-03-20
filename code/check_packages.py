#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Check for package installs
"""
import pip

# Function to check packages
def check_packages(package_list = ['csv',
                                   'gamsapi',
                                   'ixmp',
                                   'message_ix',
                                   'numpy',
                                   'openpyxl',
                                   'os',
                                   'pandas']):
    print('Checking package installs...')
    for package_name in package_list:
        try:
            __import__(package_name)
        except ImportError:
            pip.main(['install', package_name])
