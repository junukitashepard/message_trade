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
                         'fix_cost', 'inv_cost', 'var_cost',
                         'historical_activity', 'historical_new_capacity']
base_import_parameters = ['capacity_factor', 'emission_factor', 
                         'input', 'output',
                         'fix_cost', 'var_cost',
                         'historical_activity']

parameter_list_export = bound_activity + growth_activity + base_export_parameters

parameter_list_import = bound_activity + growth_activity + base_import_parameters


export_technologies = ['oil_exp'] # Add other technologies in a bit
import_technologies = ['oil_imp']

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
            
    if (remove_existing_par == True):
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

## Run functions: 'oil_exp_cpa'
replace_technology('oil_exp')

print('Remove existing oil_imp technology')
scenario.remove_set('technology', 'oil_imp') 
scenario.add_set('technology', 'oil_imp')

print("EXPORT TECHNOLOGIES")
for par in parameter_list_export:
    replace_parameter(par, 'oil_exp', remove_existing_par = True) # Start with oil_exp only

print("IMPORT TECHNOLOGIES")
for par in parameter_list_import:
    replace_parameter(par, 'oil_imp', remove_existing_par = True) # Start with oil_imp only
    
# Solve model #
###############
 #MUST INSERT SCENARIO COMMIT COMMENT   
scenario.commit('Bilateral oil schema without any costs')
    
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



