# Global model working script #
###############################
# load required packages 
import itertools
import pandas as pd
import os

import matplotlib.pyplot as plt
plt.style.use('ggplot')

import ixmp as ixmp
import message_ix as message_ix

from message_ix.utils import make_df

os.chdir('H:\\message_trade\\analysis\\')

# Load and clone model/scenario # 
#################################
ene_mp = ixmp.Platform() # Connect to central ENE database

# Clone model (MESSAGEix_SSP2) and scenario (test)
base_scenario = message_ix.Scenario(ene_mp, 
                                    model = 'MESSAGEix_SSP2',
                                    scenario = 'test') 
scenario = base_scenario.clone('MESSAGEix_TRADE', 'test')

# Investigate #
###############
# Look at parameter format
t = scenario.par('input')

scenario.idx_names('addon_lo')

# Parameters for given trade technology
tec = 'gas_imp'
region = 'R14_WEU'

par_list = [x for x in scenario.par_list() if 'technology' in scenario.idx_sets(x)]

par_dict = {}   # For collecting data and looking into them at the end
for parname in par_list:
    node_cols = [n for n in scenario.idx_names(parname) if 'node' in n] # node index(es)
    if len(node_cols) > 1:
        node_col = 'node_loc'
    elif len(node_cols) == 1:
        node_col = node_cols[0]

    df = scenario.par(parname, {'technology':tec, node_col:region})
    if not df['value'].dropna().empty:
            par_dict[parname] = df
            
#%% 5) Exporting scenario data to GAMS gdx and solve the model
# 5.1) An optional name for the scenario GDX files
caseName = scenario.model + '__' + scenario.scenario + '__v' + str(scenario.version)

# 5.2) Solving
if scenario.has_solution():
    scenario.remove_solution()
scenario.check_out()

scenario.commit('No changes')
scenario.solve(model='MESSAGE', case=caseName)
#scenario.to_gdx('message_gdx\\','MsgData_' + caseName)

from tools import Plots
p = Plots(scenario, 'R14_WEU')
