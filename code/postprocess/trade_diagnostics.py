#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Run trade diagnostics
"""
################
# CHANGE BELOW #
################
repo = '/Users/junshepard/Documents/GitHub/message_trade'

smip_scenarios = {'SSP1_baseline': ['SSP1_v2.3_baseline',
                                    'MsgOutput_SSP_SSP1_v2.3_baseline.gdx']}

import_from_gdx = False # If true, will draw on GDX files saved to a local directory. If False, will connect to ixmp-dev and use Reporting

#######################
# DO NOT CHANGE BELOW #
#######################
# Import required packages
import pandas as pd
import numpy as np
import openpyxl as oxl
from openpyxl.utils.dataframe import dataframe_to_rows
import csv
import os

# Import MESSAGEix packages
from message_ix import Scenario
from message_ix import Reporter
from ixmp import Platform
import ixmp

# Import helper packages
os.chdir(repo + '/code')
from check_packages import *
from dictionaries import *

# Pull in custom packages
os.chdir(repo + '/code/postprocess')
from gdx_import import *
from import_trade_gdx import *

# Directories
cwd = repo + '/data/crosswalks/'
gdxd = repo + '/data/gdx/'
iead = repo + '/data/IEA/WEB/'

# Import MESSAGE exports and imports
exports = dict(); imports = dict()

for scenario in smip_scenarios.keys():
    
    # If no connection to internal ixmp-dev, use GDX files stored locally
    if import_from_gdx == True:
        exports[scenario] = import_trade_gdx(scenario_name=smip_scenarios[scenario][0],
                                             file_name=smip_scenarios[scenario][1],
                                             trade_type='Exports')
        
        imports[scenario] = import_trade_gdx(scenario_name=smip_scenarios[scenario][0],
                                             file_name=smip_scenarios[scenario][1],
                                             trade_type='Imports')
    
    # If there is connection to internal ixmp-dev, use Reporting function
    #if import_from_gdx == False:
        
# Import and streamline IEA World Energy Balances
web = pd.read_csv(iead + '2024.txt', sep='\s+', header=None)
web.columns = ['region', 'fuel', 'year', 'flow', 'unit', 'value']

web = web[web['year'] > 1989] # Keep after 1990 only 
web = web[web['flow'].isin(['EXPORTS', 'IMPORTS'])]
web = web[web['unit'] == 'TJ']

web['value'] = np.where(web['value'].isin(['..', 'x','c']), np.nan, web['value'])
web['value'] = web['value'].astype(float)

webdf = web.copy()

cw_country = pd.read_excel(cwd + 'country_crosswalk.xlsx', sheet_name = 'R12')
cw_country = cw_country[['message_region', 'message_country', 'iea_country']]
webdf = webdf.merge(cw_country, 
                    left_on = ['region'], right_on = 'iea_country', 
                    how = 'inner')

webR12 = webdf.groupby(['message_region', 'fuel', 'flow', 'unit'])['value'].sum().reset_index()

# Historical calibration to IEA-WEB

    
