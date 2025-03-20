# -*- coding: utf-8 -*-
"""
Created on Fri Oct 11 16:51:26 2019

@author: shepard
"""

# load required packages 
import matplotlib.pyplot as plt
plt.style.use('ggplot')

import pandas as pd
import numpy as np
import re

import ixmp as ixmp
import message_ix as message_ix

import sys
sys.path.insert(1, 'H:/message_trade')
from config import *

# Compile all other scenarios #
###############################
ene_mp = ixmp.Platform() # Connect to central ENE database

for trade_scenario in ['baseline', 'tariff_high', 'tariff_low',
                       'CO2_tax_baseline', 'CO2_tax_tariff_high', 'CO2_tax_tariff_low',
                       'low_CO2_tax_baseline', 'high_CO2_tax_baseline',
                       'low_MEA_emission_factor_CO2_tax_baseline']:

    if trade_scenario in ['baseline', 'tariff_high', 'tariff_low']:
        # Clone model (MESSAGEix_SSP2) and scenario (test)
        base_scenario = message_ix.Scenario(ene_mp, 
                                            model = 'MESSAGEix_SSP2',
                                            scenario = 'test') 
        scenario = base_scenario.clone('MESSAGE_TRADE', trade_scenario)
        
        # Check out scenario so we can make edits #
        if scenario.has_solution():
            scenario.remove_solution()
        
        scenario.check_out()
    
        sys.path.insert(1, 'H:/message_trade/analysis/4_run_message')
        from compile_scenario import *
    
        replace_trade_technologies(scenario)
        add_new_sets(scenario)
        add_new_units(ene_mp)
        add_global_relations(scenario)
        add_export_technologies(scenario, trade_scenario)
        add_import_technologies(scenario, trade_scenario)
        add_shipping_technologies(scenario, trade_scenario)
        adjust_shipping_demand(scenario)
        update_levels(scenario)
        commit_and_export(scenario, 'Initial commit')
        
    else:
        scenario = message_ix.Scenario(ene_mp,
                                       model = 'MESSAGE_TRADE',
                                       scenario = re.sub(".*CO2_tax_", "", trade_scenario))
        
        scenario = scenario.clone('MESSAGE_TRADE', trade_scenario)
    
        # Check out scenario so we can make edits #
        if scenario.has_solution():
            scenario.remove_solution()
        
        scenario.check_out()
    
        sys.path.insert(1, 'H:/message_trade/analysis/4_run_message')
        from compile_scenario import *

        add_bound_emission(scenario, trade_scenario, ene_mp)
        add_CO2_tax(scenario, trade_scenario)
        
        if 'low_MEA_emission_factor' in trade_scenario:
                rmdf = scenario.par('emission_factor', {'node_loc':'R14_MEA', 'emission':'CO2'})
                chdf = scenario.par('emission_factor', {'node_loc':'R14_MEA', 'emission':'CO2'})
                
                tecnames_chdf = np.unique(chdf.technology)
                tecnames_chdf = [s for s in tecnames_chdf if 'oil_extr' in s]
                chdf.loc[chdf.technology.isin(tecnames_chdf), 'value'] = 0.50*chdf.loc[chdf.technology.isin(tecnames_chdf), 'value']
                
                scenario.remove_par('emission_factor', rmdf)
                scenario.add_par('emission_factor', chdf)
                
        commit_and_export(scenario, 'Initial commit')
