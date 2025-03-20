# -*- coding: utf-8 -*-
"""
This function copies parameters defined for a member of a set (e.g.,
"technology") from a MESSAGEix scenario (sc_ref), applies some changes if
needed, and adds those parameters to another member of the same set either
in the same or in another MESSAGEix scenario (sc_new). See examples belwo.
"""

import ixmp as ix
import message_ix
import pandas as pd


def copy_par(sc_ref, sc_new, node_ref, node_new, set_name, elem_ref, elem_new,
             parameter='all', par_exclude=[], par_remove=[], dict_change={},
             test_run=True):

    ''' Input parameters:
        sc_ref, object: reference message_ix scenario class (to copy from)
        sc_new, object: destination message_ix scenario (to copy to)
        node_ref, string: model region to copy from
        node_new, string: model region to copy to
        set_name, string: message_ix set that copying elements belong to
        elem_ref, string: reference element (for copying its parameters)
        elem_new, string: destination element (for adding parameters for it)
        parameter, string, list, default 'all': list of parameters to be copied
        par_exclude, list: list of parameters excluded from copying
        par_remove, list, default 'all': list of parameters in sc_new whose
            data should be removed before copying new data from sc_ref
        dict_change, python dictionary: dictionary of required modifications
            (see example below)
        test_run, boolean: if True, data won't be added to sc_new but can be
            checked for test purposes

    '''


    if parameter == 'all':
        par_list = [x for x in sc_ref.par_list() if set_name in
                    sc_ref.idx_names(x)]
    elif isinstance(parameter, str):
        par_list = [parameter]
    elif isinstance(parameter, list):
        par_list = parameter

    par_list = [x for x in par_list if 'node' in sc_ref.idx_sets(x)]
    par_dict1 = {}
    par_dict2 = {}
    year_new = sc_new.set('year')

    # First, loading available data from each scenario
    for parname in par_list:
        node_idxs = [x for x in sc_ref.idx_names(parname) if 'node' in x]
        year_idxs = [x for x in sc_ref.idx_names(parname) if 'year' in x]
        if 'node_loc' in node_idxs:
            node_col = 'node_loc'
        else:
            node_col = node_idxs[0]

        par_ref = sc_ref.par(parname, {set_name: elem_ref,
                                       node_col: node_ref})
        for year_col in year_idxs:
            par_ref = par_ref.loc[par_ref[year_col].isin(year_new)].copy()

        par_new = pd.DataFrame()
        if elem_new in sc_new.set(set_name):
            par_new = sc_new.par(parname, {set_name: elem_new,
                                           node_col: node_new})

        if not par_ref.empty:
            par_dict1[parname] = par_ref
        if not par_new.empty:
            par_dict2[parname] = par_new

    if par_remove == 'all':
        par_remove = [x for x in list(set(par_dict2.keys())
                                      ) if x not in par_exclude]
    par_unchanged = list(set(par_dict2.keys())) + par_exclude
    par_diff = [x for x in par_dict1.keys() if x not in par_unchanged]
    par_add = par_diff + par_remove
    print('> These parameters will be copied for technology {} in node {}:'
          ' {}'.format(elem_new, node_new, par_add))

    # Second, copying, removing extra parameters, and changing some if needed
    if not test_run:
        sc_new.check_out()
        for parname in par_add:
            if parname in par_dict1.keys():
                par_copy = par_dict1[parname].copy()
                par_copy = par_copy.replace({node_ref: node_new})
                par_copy[set_name] = elem_new

            if parname in par_remove and parname in par_dict2.keys():
                sc_new.remove_par(parname, par_dict2[parname])
            if parname not in par_exclude:
                sc_new.add_par(parname, par_copy)

            if parname in dict_change.keys():
                df_change = par_copy.copy()
                for column in dict_change[parname][0].keys():
                    df_change = df_change.loc[df_change[column].isin(
                            dict_change[parname][0][column])]

                sc_new.remove_par(parname, df_change)
                for column in dict_change[parname][1].keys():
                    df_change[column] = dict_change[parname][1][column]
                sc_new.add_par(parname, df_change)

        sc_new.commit('')
    return par_dict1, par_dict2


if __name__ == "__main__":
    mp = ix.Platform()
    sc_ref = message_ix.Scenario(mp, 'model', 'baseline')
    tec_ref = 'wind_ppl'
    node_ref = 'Austria'

    sc_new = message_ix.Scenario(mp, 'model', 'policy')
    tec_new = 'wind_ppf'
    node_new = 'Austria'


    # Some examples-----------------------------------------------------------
    # Example 1: changing 'fix_cost' for specific combination of years:
    # dict_change = {'fix_cost': [{'year_vtg': [2010,2015], 'year_act':[2015]},
    #                             {'value': 0.1}]}

    # Example 2: changing 'node_dest' in parameter 'output':
    # dict_change = {'output': [{'node_dest': ['R14_GLB']},
    #                           {'node_dest':'World'}]}

    # Example 3: changing 'commodity' in parameter 'input':
    # dict_change = {'input': [{'commodity': ['gas_eeu']},
    #                          {'commodity': 'gas_scs'}]}

    # Example 4: Exlcuding bounds from copying to new scenario
    # par_exclude = [x for x in sc_new.par_list() if any(
    #        y in x for y in ['bound_'])]
    #-------------------------------------------------------------------------

    d1, d2 = copy_par(sc_ref, sc_new, node_ref, node_new, 'technology',
                      tec_ref, tec_new)


