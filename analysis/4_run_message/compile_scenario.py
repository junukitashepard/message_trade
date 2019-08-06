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

# Set directory for parameters file input
inpath = "H:/data/output/analysis/msg_parameters/"
 
# Parameters to be updated:
bound_activity = ['bound_activity_lo', 'bound_activity_up']
growth_activity = ['growth_activity_lo', 'growth_activity_up']
initial_activity = ['initial_activity_lo', 'initial_activity_up']
level_cost_activity = ['level_cost_activity_soft_lo', 'level_cost_activity_soft_up']
soft_activity = ['soft_activity_lo', 'soft_activity_up']

base_export_parameters = ['capacity_factor', 'emission_factor', 'technical_lifetime',
                         'input', 'output',
                         'relation_activity', 'relation_upper', 'relation_lower',
                         'fix_cost', 'inv_cost', 'var_cost',
                         'historical_activity', 'historical_new_capacity']
base_import_parameters = ['capacity_factor', 'emission_factor', 
                         'input', 'output',
                         'relation_activity',
                         'fix_cost', 'var_cost',
                         'historical_activity']

parameter_list_export = growth_activity + base_export_parameters

parameter_list_import = growth_activity + base_import_parameters

parameter_list_shipping = ['capacity_factor', 'emission_factor', 
                           'fix_cost', 'inv_cost', 'var_cost',
                           'input', 'output', 'technical_lifetime'] # historical_activity only for diesel-powered shipping


export_technologies = ['oil_exp', 'loil_exp', 'foil_exp', 'LNG_exp', 'coal_exp'] 
import_technologies = ['oil_imp', 'loil_imp', 'foil_imp', 'LNG_imp', 'coal_imp']
shipping_technologies = ['liquid_shipping_diesel', 'liquid_shipping_LNG', 'liquid_shipping_elec',
                         'LNG_shipping_diesel', 'LNG_shipping_LNG', 'LNG_shipping_elec',
                         'solid_shipping_diesel', 'solid_shipping_LNG', 'solid_shipping_elec']

regions_list = ['afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 
                'pas', 'rus', 'sas', 'scs', 'ubm', 'weu']

# Load and clone model/scenario # 
#################################
ene_mp = ixmp.Platform() # Connect to central ENE database

# Clone model (MESSAGEix_SSP2) and scenario (test)
base_scenario = message_ix.Scenario(ene_mp, 
                                    model = 'MESSAGEix_SSP2',
                                    scenario = 'test') 
scenario = base_scenario.clone('MESSAGEix_TRADE', 'trade_parameters')

#scenario = message_ix.Scenario(ene_mp,
#                               model = 'MESSAGEix_TRADE',
#                               scenario = 'trade_parameters',
#                               version = 73)

# Check out scenario so we can make edits #
###########################################
if scenario.has_solution():
    scenario.remove_solution()
    
scenario.check_out()

# Import and append parameters #
################################
def replace_technology(tecname, rlist = regions_list):
    print("Adding trade technology: " + tecname + "_REGIONS")
    teclist = list(scenario.set('technology'))
    for r in rlist:
        tec = tecname + '_' + r
        if tec not in teclist:
            print('...adding...' + tec)
            scenario.add_set('technology', tec)
    scenario.remove_set('technology', tecname) 
        
def replace_parameter(parname, tecname, new_tecname = 'all', remove_existing_par = False): 
    
    df_base = scenario.par(parname)
            
    if (remove_existing_par == True and 'technology' in df_base.columns):
        df_rm = df_base[df_base.technology.isin([tecname])]
        scenario.remove_par(parname, key = df_rm) # Exclude the trade technology
    
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
print('Replace existing export technologies')
for tec in export_technologies:
    replace_technology(tec)
    scenario.add_set('technology', tec) # Add back in for relation-activity

print('Remove existing import technologies')
for tec in import_technologies:
    scenario.remove_set('technology', tec) 
    scenario.add_set('technology', tec)

print('Add shipping technologies')
for tec in shipping_technologies:
    scenario.add_set('technology', tec)
    
scenario.add_set('relation', 'lim_liquid_shipping')
scenario.add_set('relation', 'lim_LNG_shipping')
scenario.add_set('relation', 'lim_solid_shipping')

ene_mp.add_unit('USD/bton-km-y')
ene_mp.add_unit('bton-km-y')
ene_mp.add_unit('kg/bton-km-y')
ene_mp.add_unit('bton-km')

print("EXPORT TECHNOLOGIES")
for tec in export_technologies:
    rel = 'lim_' + tec
    print('Adding relation: lim_' + tec)
    scenario.add_set('relation', rel)
    
    print('Adding parameters for ' + tec)
    for par in parameter_list_export:
        replace_parameter(par, tec, remove_existing_par = True) 

print("IMPORT TECHNOLOGIES")
for tec in import_technologies:
    print('Adding parameters for ' + tec)
    for par in parameter_list_import:
        replace_parameter(par, tec, remove_existing_par = True) 
    
print("SHIPPING TECHNOLOGIES")
for tec in shipping_technologies:
    for par in parameter_list_shipping:
        replace_parameter(par, tec)

for rel in ['liquid_shipping', 'LNG_shipping', 'solid_shipping']:
    replace_parameter('relation_activity', rel)
    replace_parameter('relation_upper', rel)
    
    historic_tec = rel + '_diesel'
    replace_parameter('historical_activity', historic_tec)
    replace_parameter('historical_new_capacity', historic_tec)

print("Edit bunker technologies (level from exports to shipped-fuel)")
for fuel in ['foil', 'loil', 'LNG']:  
    print('Update input parameter: ' + fuel)
    tec = fuel + '_tobunker'
    outdf = scenario.par('input', {'technology': tec})
    rmdf = scenario.par('input', {'technology': tec})
    outdf.level = 'shipped-' + fuel
    scenario.remove_par('input', rmdf)
    scenario.add_par('input', outdf)
    
print("Adjust non-energy shipping demand, set to 50%")
fixed_demand = scenario.par('demand', {'commodity':'shipping'})
fixed_demand.value = fixed_demand.value * 0.5
scenario.add_par('demand', fixed_demand)
   
# Solve model #
###############
 #MUST INSERT SCENARIO COMMIT COMMENT   
scenario.commit('Allow shipping inv_cost to go down over time')
    
print("Scenario version = " + str(scenario.version))
modelPath = 'C:\ProgramData\Anaconda3\Lib\site-packages\message_ix\model'
caseName = scenario.model + '__' + scenario.scenario + '__v' + str(scenario.version)
scenario.to_gdx(modelPath + '\data','MsgData_' + caseName)

#import time
#start_time = time.time()
#
#print("Run scenario: " + caseName)
#scenario.commit('Update oil_exp parameters, remove base oil_exp and oil_imp technology')
#scenario.solve(model='MESSAGE', case=caseName)
#
#end_time = time.time()
#time_time = end_time - start_time
#print("MESSAGEix took" +  time_time + "to run")
#

## Remove technology
#df_base = scenario.par('var_cost')
#for tec in export_technologies:
#    for reg in regions_list:
#        tecname = tec + '_' + reg
#        df_rm = df_base[df_base.technology.isin([tecname])]
#        scenario.remove_par('var_cost', key = df_rm) # Exclude the trade technology

