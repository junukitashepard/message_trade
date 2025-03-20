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

import sys
sys.path.insert(1, 'H:/message_trade')
from config import *

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
# List of all trade technologies
trade_tech_list = export_technologies + import_technologies

def export_parameter(parname, tecname):
    df = scenario.par(parname, {'technology':tecname})
    df.to_csv(path_or_buf = (outpath + parname + '_' + tecname + '.csv'), index = False)

# bound_activity_lo
for t in trade_tech_list:
    export_parameter(parname = 'bound_activity_lo', tecname = t)
    
# bound_activity_up
for t in trade_tech_list:
    export_parameter(parname = 'bound_activity_up', tecname = t)
   
# initial_activity_lo
for t in trade_tech_list:   
    export_parameter(parname = 'initial_activity_lo', tecname = t)
    
# initial_activity_up
for t in trade_tech_list:   
    export_parameter(parname = 'initial_activity_up', tecname = t)
    
# growth_activity_up
for t in trade_tech_list:   
    export_parameter(parname = 'growth_activity_up', tecname = t)
    
# growth_activity_lo
for t in trade_tech_list:   
    export_parameter(parname = 'growth_activity_lo', tecname = t)
    
# historical_activity
for t in trade_tech_list:
    export_parameter(parname = 'historical_activity', tecname = t)    
    
# relation_activity
for t in trade_tech_list:
    export_parameter(parname = 'relation_activity', tecname = t)    
    
# historical_new_capacity (export technologies only)
for t in export_technologies:
    export_parameter(parname = 'historical_new_capacity', tecname = t) 

# ref_new_capacity
for t in export_technologies:
    export_parameter(parname = 'ref_new_capacity', tecname = t) 

# ref_activity
for t in trade_tech_list:
    export_parameter(parname = 'ref_activity', tecname = t)   





