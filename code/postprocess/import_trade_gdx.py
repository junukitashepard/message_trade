#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Import GDX Files and pull out activities related to trade
"""
## Function: Import GDX for trade
def import_trade_gdx(scenario_name,
                     file_name,
                     trade_type):
    
    cw_trade = pd.read_excel(cwd + 'trade_activity.xlsx', sheet_name = trade_type)

    activity = import_gdx(msg_variable='ACT',
                          gdx_file=file_name,
                          gdx_scenario_name=scenario_name,
                          gdx_directory=gdxd)

    activity = activity.merge(cw_trade,
                              left_on = ['DATANAME', 'tec'],
                              right_on = ['data_name', 'group_name'],
                              how = 'inner')
    
    if trade_type == 'Exports': nd = 'ORIGIN'
    elif trade_type == 'Imports': nd = 'DESTINATION'
    
    activity[nd] = ''
    for n in R12_nodes.keys():
        activity[nd] = np.where(activity['node'] == 'R12_' + n, n, activity[nd])
    
    activity['YEAR'] = activity['year_all']
    activity = activity[['SCENARIO', 'DATANAME', 'DATADESC', 'DESCRIPTION', 'UNIT', 'YEAR', 'FUEL', 'ORIGIN', 'DESTINATION', 'level', 'marginal']]
    
    if trade_type == 'Exports': gnd = 'DESTINATION'
    elif trade_type == 'Imports': gnd = 'ORIGIN'
    
    return {'total': activity[activity[gnd] == 'Global Pool'],
            'bilateral': activity[activity[gnd] != 'Global Pool']}