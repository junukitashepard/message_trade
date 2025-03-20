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

# Load and clone model/scenario # 
#################################
ene_mp = ixmp.Platform() # Connect to central ENE database

# Clone model (MESSAGEix_SSP2) and scenario (test)
base_scenario = message_ix.Scenario(ene_mp, 
                                    model = 'MESSAGEix_SSP2',
                                    scenario = 'test') 
scenario = base_scenario.clone('MESSAGEix_TRADE', 'update_costs')

# Change costs of MEA-WEU gas exports #
#######################################
vc_mea = scenario.par('var_cost', {'technology':'gas_exp_weu', 'node_loc':'R14_MEA'})
vc_mea.value = 100

#vc_weu = scenario.par('var_cost', {'technology':'gas_imp', 'node_loc':'R14_WEU'})
#vc_weu.value = 100

#%% 5) Exporting scenario data to GAMS gdx and solve the model
# 5.1) An optional name for the scenario GDX files
caseName = scenario.model + '__' + scenario.scenario + '__v' + str(scenario.version)

# 5.2) Solving
if scenario.has_solution():
    scenario.remove_solution()
scenario.check_out()

scenario.commit('No changes')
scenario.solve(model='MESSAGE', case=caseName)

from plotting import Plots
p = Plots(scenario, 'R14_MEA')

p.plot_activity(baseyear = False, subset = ['gas_exp_weu'])

