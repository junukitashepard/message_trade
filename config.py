# -*- coding: utf-8 -*-
"""
Configuration for MESSAGE-TRADE

Created on Thu Oct 10 18:30:12 2019

@author: J. Shepard
"""

# Set directory for parameters file input and output #
######################################################
inpath = "H:/data/output/analysis/msg_parameters/"
outpath = "H:/data/output/derived/parameters/"
modelPath = 'C:\ProgramData\Anaconda3\Lib\site-packages\message_ix\model'

# Temporal specification #
##########################
MESSAGE_years = list(range(1990, 2060, 5)) + list(range(2060, 2110, 10))
MESSAGE_model_horizon_start = 2020 # When does the model horizon start?

# Regional specification #
##########################
region_number = 'R14'
regional_specification_csv = 'H:/message_trade/user_inputs/default/regional_specification.csv'
regions_list = ['afr', 'cas', 'cpa', 'eeu', 'lam', 'mea', 'nam', 'pao', 
                'pas', 'rus', 'sas', 'scs', 'ubm', 'weu']
region_list_trade = ['AFR', 'CPA', 'EEU', 'LAM', 'MEA', 'NAM', 'PAO', 'PAS', 'SAS', 'WEU']

# List of technologies #
########################
export_technologies = ['oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp']
import_technologies = ['oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp']

commodities_list = ['fueloil', 'crudeoil', 'lightoil', 'coal', 'LNG']
level_list = ['shipped-foil', 'shipped-oil', 'shipped-loil', 'shipped-coal', 'shipped-LNG']

shipping_technologies = ['liquid_shipping_diesel', 'liquid_shipping_LNG', 'liquid_shipping_elec',
                         'LNG_shipping_diesel', 'LNG_shipping_LNG', 'LNG_shipping_elec',
                         'solid_shipping_diesel', 'solid_shipping_LNG', 'solid_shipping_elec']

# Energy commodities modeled #
##############################
energy_types = ['coal', 'foil', 'loil', 'LNG', 'oil']
energy_types_BACI = ['oil', 'coal', 'foil', 'LNG'] # Energy types found in BACI
energy_types_oil_history = ['loil'] # Energy types for which historical activity can be proxied by oil
energy_types_trade_foil = ['loil'] # Energy types for which trade variable costs behave like fuel oil
energy_types_trade_LNG = [] # Energy types for which trade variable costs behave like LNG

# Technical lifetime or trade #
###############################
MESSAGE_technical_lifetime = 5

# Capacity factor of trade #
############################
MESSAGE_capacity_factor = 1

# Parameters to be compiled in R #
##################################
parameter_list = ['bound_activity_lo', 'bound_activity_up',
                  'capacity_factor', 'fix_cost',
                  'growth_activity_lo', 'growth_activity_up',
                  'historical_activity', 'historical_new_capacity',
                  'initial_activity_lo', 'initial_activity_up',
                  'input', 'inv_cost',
                  'level_cost_activity_soft_lo', 'level_cost_activity_soft_up',
                  'output', 'soft_activity_lo', 'soft_activity_up', 'technical_lifetime']

# Parameters to be updated in compile_scenario.py #
###################################################
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

parameter_list_export = base_export_parameters + growth_activity + initial_activity

parameter_list_import = base_import_parameters

parameter_list_shipping = ['capacity_factor', 'emission_factor', 
                           'fix_cost', 'inv_cost', 'var_cost',
                           'input', 'output', 'technical_lifetime'] # historical_activity only for diesel-powered shipping

# Scenario Selection #
######################
# Scenarios that impact variable cost
scen_var_cost_imp = ['baseline_no_tariff', 
                     'baseline', 'tariff_high', 'tariff_low',
                     'CO2_tax_baseline', 'CO2_tax_tariff_high', 'CO2_tax_tariff_low']
scen_var_cost_exp = ['NAM_CPA_sanction', 'CPA_PAO_sanction', 'NAM_MEA_sanction',
                     'NAM_MEA_sanction_onlydirect']

# Scenarios that impact investment cost
scen_inv_cost = ['shipping_CLT']