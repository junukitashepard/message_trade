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

import ixmp as ixmp
import message_ix as message_ix

# Set directory for parameters file input
inpath = "H:/data/output/analysis/msg_parameters/"

# Load and clone model/scenario # 
#################################
ene_mp = ixmp.Platform() # Connect to central ENE database

# Clone model (MESSAGEix_SSP2) and scenario (test)
base_scenario = message_ix.Scenario(ene_mp, 
                                    model = 'MESSAGEix_SSP2',
                                    scenario = 'test') 
scenario = base_scenario.clone('MESSAGEix_TRADE', 'export_parameters')

# Import and append parameters #
################################

# Solve model #
###############
caseName = scenario.model + '__' + scenario.scenario + '__v' + str(scenario.version)

if scenario.has_solution():
    scenario.remove_solution()
    
scenario.check_out()

scenario.commit('No changes')
scenario.solve(model='MESSAGE', case=caseName)