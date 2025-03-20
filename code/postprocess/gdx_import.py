#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Import GDX Files
"""
import gams.transfer as gt
import pandas as pd

def import_gdx(msg_variable, 
               gdx_file,
               gdx_scenario_name,
               gdx_directory,
               gams_sysdir='/Library/Frameworks/GAMS.framework/Versions/Current/Resources'):
 
    '''
    RETURNS: pd.DataFrame
        Dataframe of GDX parameter, set, or variable with column for scenario and data type
    '''
    
    m = gt.Container(gdx_directory + gdx_file, system_directory=gams_sysdir)

    vardf = m[msg_variable].records
    vardf['SCENARIO'] = gdx_scenario_name
    vardf['DATANAME'] = msg_variable
    vardf['DATADESC'] = dict(list(m))[msg_variable].description
    
    return vardf