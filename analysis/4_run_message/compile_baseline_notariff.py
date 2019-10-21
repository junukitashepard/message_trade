# -*- coding: utf-8 -*-
"""
Created on Fri Oct 11 14:57:23 2019

@author: shepard
"""

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

# Compile baseline no tariff #
##############################
trade_scenario = 'baseline_no_tariff'

ene_mp = ixmp.Platform() # Connect to central ENE database

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
add_bound_emission(scenario, trade_scenario, ene_mp)
add_CO2_tax(scenario, trade_scenario)
commit_and_export(scenario, 'Update growth activity lo/hi limits')
