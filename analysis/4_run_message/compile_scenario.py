# -*- coding: utf-8 -*-
"""
Created on Mon Jul  1 13:16:10 2019

@author: Jun Shepard
"""

# Update global MESSAGE-TRADE model #
#####################################
# load required packages 
import matplotlib.pyplot as plt
plt.style.use('ggplot')

import pandas as pd
import numpy as np

import ixmp as ixmp
import message_ix as message_ix

import sys
sys.path.insert(1, 'H:/message_trade')
from config import *

# Load and clone model/scenario # 
#################################
def load_scenario(trade_scenario):
    ene_mp = ixmp.Platform() # Connect to central ENE database
    
    global scenario
    
    # Clone model (MESSAGEix_SSP2) and scenario (test)
    base_scenario = message_ix.Scenario(ene_mp, 
                                        model = 'MESSAGEix_SSP2',
                                        scenario = 'test') 
    scenario = base_scenario.clone('MESSAGE_TRADE', trade_scenario)
    
    #scenario = message_ix.Scenario(ene_mp,
    #                               model = 'MESSAGEix_TRADE',
    #                               scenario = 'baseline',
    #                               version = 16)
    
    #scenario = scenario.clone('MESSAGE_TRADE', trade_scenario)

    # Check out scenario so we can make edits #
    if scenario.has_solution():
        scenario.remove_solution()
        
    scenario.check_out()

# Import and append parameters #
################################
def replace_technology(tecname, scenario, rlist = regions_list):
    print("Adding trade technology: " + tecname + "_REGIONS")
    teclist = list(scenario.set('technology'))
    for r in rlist:
        tec = tecname + '_' + r
        if tec not in teclist:
            print('...adding...' + tec)
            scenario.add_set('technology', tec)
    scenario.remove_set('technology', tecname) 
        
def replace_parameter(parname, tecname, scenario, trade_scenario, new_tecname = 'all', remove_existing_par = False): 
    
    df_base = scenario.par(parname)
            
    if (remove_existing_par == True and 'technology' in df_base.columns):
        df_rm = df_base[df_base.technology.isin([tecname])]
        scenario.remove_par(parname, key = df_rm) # Exclude the trade technology
    
    # variable costs are sometimes scenario-dependent
    if ((parname == 'var_cost' and 'imp' in tecname and trade_scenario in scen_var_cost_imp) or
        (parname == 'var_cost' and 'exp' in tecname and trade_scenario in scen_var_cost_exp) or
        (parname == 'inv_cost' and 'shipping' in tecname and trade_scenario in scen_inv_cost)):
        print(parname + ' based on scenario = ' + trade_scenario)
        df_new = pd.read_csv((inpath + 'SCENARIOS/' + trade_scenario + '/' + parname + '/' + tecname + '.csv'))
    else:
        df_new = pd.read_csv((inpath + parname + '/' + tecname + '.csv'))
        
    df_new = df_new[list(df_base.columns)]

    if (new_tecname != 'all'):
        df_new = df_new[df_new.technology.isin([new_tecname])]
        
    # Edit scenario    
    if 'commodity' in list(df_base.columns):
        old_commodity = list(np.unique(df_base['commodity']))
        new_commodity = np.unique(df_new['commodity'])
        for c in new_commodity:
            if c not in old_commodity:
                print("Adding commodity: " + c)
                scenario.add_set('commodity', c) # Add new commodities (e.g. oil_afr)
     
    if 'level' in list(df_base.columns):
        old_level = list(np.unique(df_base['level']))
        new_level = np.unique(df_new['level'])
        for l in new_level:
            if l not in old_level:
                print("Adding level: " + l)
                scenario.add_set('level', l)
                
    print("Adding parameter: " + parname)
    scenario.add_par(parname, df_new) # Add parameter (replace default)

## Run functions: 
def replace_trade_technologies(scenario):
    print('Replace existing export technologies')
    for tec in export_technologies:
        replace_technology(tec, scenario = scenario)
        scenario.add_set('technology', tec) # Add back in for relation-activity
        
        glb_tec = tec + '_glb'
        scenario.add_set('technology', glb_tec)
        
    print('Remove existing import technologies')
    for tec in import_technologies:
        scenario.remove_set('technology', tec) 
        scenario.add_set('technology', tec)
    
    print('Add shipping technologies')
    for tec in shipping_technologies:
        scenario.add_set('technology', tec)
    
def add_new_sets(scenario):
    scenario.add_set('relation', 'lim_liquid_shipping')
    scenario.add_set('relation', 'lim_LNG_shipping')
    scenario.add_set('relation', 'lim_solid_shipping')

def add_new_units(ene_mp):
    ene_mp.add_unit('USD/bton-km-y')
    ene_mp.add_unit('bton-km-y')
    ene_mp.add_unit('kg/bton-km-y')
    ene_mp.add_unit('bton-km')

def add_global_relations(scenario):
    print('Remove global trade technology')
    scenario.remove_set('technology', 'oil_trd')
    scenario.remove_set('technology', 'loil_trd')
    scenario.remove_set('technology', 'foil_trd')
    scenario.remove_set('technology', 'coal_trd')
    scenario.remove_set('technology', 'LNG_trd')
    
    scenario.add_set('technology', 'oil_trd')
    scenario.add_set('technology', 'loil_trd')
    scenario.add_set('technology', 'foil_trd')
    scenario.add_set('technology', 'coal_trd')
    scenario.add_set('technology', 'LNG_trd')

    print('Add relations for global trade')
    scenario.add_set('relation', 'lim_oil_trd')
    scenario.add_set('relation', 'lim_loil_trd')
    scenario.add_set('relation', 'lim_foil_trd')
    scenario.add_set('relation', 'lim_coal_trd')
    scenario.add_set('relation', 'lim_LNG_trd')

def add_export_technologies(scenario, trade_scenario):
    print("EXPORT TECHNOLOGIES")
    for tec in export_technologies:
        rel = 'lim_' + tec
        
        print('Adding relation: lim_' + tec)
        scenario.add_set('relation', rel)
    
        print('Adding parameters for ' + tec)
        for par in parameter_list_export:
            replace_parameter(par, tec, scenario = scenario, trade_scenario = trade_scenario, remove_existing_par = True) 

def add_import_technologies(scenario, trade_scenario):
    print("IMPORT TECHNOLOGIES")
    for tec in import_technologies:
        print('Adding parameters for ' + tec)
        for par in parameter_list_import:
            replace_parameter(par, tec, scenario = scenario, trade_scenario = trade_scenario, remove_existing_par = True) 

def add_shipping_technologies(scenario, trade_scenario):
    print("SHIPPING TECHNOLOGIES")
    for tec in shipping_technologies:
        for par in parameter_list_shipping:
            replace_parameter(par, tec, scenario, trade_scenario)
    
    for rel in ['liquid_shipping', 'LNG_shipping', 'solid_shipping']:
        replace_parameter('relation_activity', rel, scenario = scenario, trade_scenario = trade_scenario)
        replace_parameter('relation_upper', rel, scenario = scenario, trade_scenario = trade_scenario)
        
        historic_tec = rel + '_diesel'
        replace_parameter('historical_activity', historic_tec, scenario = scenario, trade_scenario = trade_scenario)
        replace_parameter('historical_new_capacity', historic_tec, scenario = scenario, trade_scenario = trade_scenario)

    print("Edit bunker technologies (level from exports to shipped-fuel)")
    for fuel in ['foil', 'loil', 'LNG']:  
        print('Update input parameter: ' + fuel)
        tec = fuel + '_tobunker'
        outdf = scenario.par('input', {'technology': tec})
        rmdf = scenario.par('input', {'technology': tec})
        outdf.level = 'shipped-' + fuel
        scenario.remove_par('input', rmdf)
        scenario.add_par('input', outdf)

def adjust_shipping_demand(scenario):
    print("Adjust non-energy shipping demand, set to 50%")
    fixed_demand = scenario.par('demand', {'commodity':'shipping'})
    fixed_demand.value = fixed_demand.value * 0.5
    scenario.add_par('demand', fixed_demand)

def update_levels(scenario):       
    print('Change level names in output, input parameters')
    for par in ['input', 'output']:
        for lev in ['export', 'import']:
            for i in range(5):
                print('...replacing level ' + lev + ' with level ' + level_list[i])
                
                rmdf = scenario.par(par, {'commodity':commodities_list[i], 'level':lev})
                indf = scenario.par(par, {'commodity':commodities_list[i], 'level':lev})
                
                indf.level = level_list[i]
                
                scenario.remove_par(par, rmdf)
                scenario.add_par(par, indf)

# For carbon emission bound, add bound_emission parameter
def add_bound_emission(scenario, trade_scenario, ene_mp):
    ene_mp.add_unit('Gt')
    if trade_scenario is 'CO2_bound':
        print('Adding CO2 bound')
        bound_emission_base = {
            'node': 'World',
            'type_emission': 'TCE',
            'type_tec': 'all',
            'type_year': 'cumulative',
            'value': 1000,
            'unit': 'Gt'
        }
        
        bound_emission = pd.DataFrame(bound_emission_base, index = [0])
        scenario.add_par('bound_emission', bound_emission)
 
# For carbon pricing, add tax_emission parameter
def add_CO2_tax(scenario, trade_scenario):
    if 'CO2_tax' in trade_scenario:
        print('Add CO2 tax')
        indf = pd.read_csv((inpath + 'SCENARIOS/' + trade_scenario + '/tax_emission/tax_emission.csv'))
        scenario.add_par('tax_emission', indf)
    
# Solve model #
###############
def commit_and_export(scenario, commit_message): 
    print('Committing:' + commit_message)
    scenario.commit(commit_message)
    
    print("Scenario version = " + str(scenario.version))
    caseName = scenario.model + '__' + scenario.scenario + '__v' + str(scenario.version)
    scenario.to_gdx(modelPath + '\data','MsgData_' + caseName)


