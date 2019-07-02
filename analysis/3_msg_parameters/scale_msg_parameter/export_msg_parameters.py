# -*- coding: utf-8 -*-
"""
Created on Mon Jul  1 13:16:10 2019

@author: Jun Shepard
"""

# Update global MESSAGE-TRADE model #
#####################################
# load required packages 
import itertools
import pandas as pd
import os
import numpy as np

import matplotlib.pyplot as plt
plt.style.use('ggplot')

import ixmp as ixmp
import message_ix as message_ix

from message_ix.utils import make_df

# Set directory for file output
outpath = "H:/data/output/derived/parameters/"

# Load and clone model/scenario # 
#################################
ene_mp = ixmp.Platform() # Connect to central ENE database

# Clone model (MESSAGEix_SSP2) and scenario (test)
base_scenario = message_ix.Scenario(ene_mp, 
                                    model = 'MESSAGEix_SSP2',
                                    scenario = 'test') 
scenario = base_scenario.clone('MESSAGEix_TRADE', 'export_parameters')

# Export parameters as csv #
############################
def export_parameter(parname, tecname):
    df = scenario.par(parname, {'technology':tecname})
    df.to_csv(path_or_buf = (outpath + parname + '_' + tecname + '.csv'), index = False)

# bound_activity_lo
for t in ['oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp',
          'oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp']:
    export_parameter(parname = 'bound_activity_lo', tecname = t)
    
# bound_activity_up
for t in ['oil_imp', 'coal_imp', 'loil_imp', 'foil_imp', 'LNG_imp',
          'oil_exp', 'coal_exp', 'loil_exp', 'foil_exp', 'LNG_exp']:
    export_parameter(parname = 'bound_activity_up', tecname = t)