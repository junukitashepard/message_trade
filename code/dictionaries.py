#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Dictionaries
"""
R12_nodes = {'AFR': 'Africa',
             'EEU': 'Eastern Europe',
             'LAM': 'Latin America',
             'MEA': 'Middle East',
             'NAM': 'North America',
             'SAS': 'South Asia',
             'WEU': 'Western Europe',
             'FSU': 'Former Soviet Union',
             'PAO': 'Pacific OECD',
             'PAS': 'Other Pacific Asia',
             'CHN': 'China',
             'RCPA': 'Rest Centrally Planned Asia'}

R12_aggregation = {'AFR': ['AGO', 'BDI', 'BEN', 'BFA', 'BWA', 'CAF', 'CIV', 'CMR', 'COD', 'COG', 'COM', 'CPV', 'DJI', 'ERI', 
                           'ETH', 'GAB', 'GHA', 'GIN', 'GMB', 'GNB', 'GNQ', 'KEN', 'LBR', 'LSO', 'MDG', 'MLI', 'MOZ', 'MRT', 
                           'MUS', 'MWI', 'MYT', 'NAM', 'NER', 'NGA', 'REU', 'RWA', 'SEN', 'SHN', 'SLE', 'SOM', 'STP', 'SWZ', 
                           'SYC', 'TCD', 'TGO', 'TZA', 'UGA', 'ZAF', 'ZMB', 'ZWE'],
                   'EEU': ['ALB', 'BGR', 'BIH', 'CZE', 'EST', 'HRV', 'HUN', 'LTU', 'LVA', 'MKD', 'MNE', 'POL', 'ROU', 'SCG', 
                           'SRB', 'SVK', 'SVN', 'YUG'],
                   'LAM': ['ABW', 'AIA', 'ANT', 'ARG', 'ATG', 'BES', 'BHS', 'BLZ', 'BMU', 'BOL', 'BRA', 'BRB', 'CHL', 'COL', 
                           'CRI', 'CUB', 'CUW', 'CYM', 'DMA', 'DOM', 'ECU', 'FLK', 'GLP', 'GRD', 'GTM', 'GUF', 'GUY', 'HND', 
                           'HTI', 'JAM', 'KNA', 'LCA', 'MEX', 'MSR', 'MTQ', 'NIC', 'PAN', 'PER', 'PRY', 'SLV', 'SUR', 'SXM', 
                           'TCA', 'TTO', 'URY', 'VCT', 'VEN', 'VGB'],
                   'MEA': ['ARE', 'BHR', 'DZA', 'EGY', 'ESH', 'IRN', 'IRQ', 'ISR', 'JOR', 'KWT', 'LBN', 'LBY', 'MAR', 'OMN', 
                           'PSE', 'QAT', 'SAU', 'SDN', 'SSD', 'SYR', 'TUN', 'YEM'],
                   'NAM': ['CAN', 'GUM', 'PRI', 'SPM', 'USA', 'VIR'],
                   'SAS': ['AFG', 'BGD', 'BTN', 'IND', 'LKA', 'MDV', 'NPL', 'PAK'],
                   'WEU': ['AND', 'AUT', 'BEL', 'CHE', 'CYP', 'DEU', 'DNK', 'ESP', 'FIN', 'FRA', 'FRO', 'GBR', 'GIB', 'GRC', 
                           'GRL', 'IMN', 'IRL', 'ISL', 'ITA', 'LIE', 'LUX', 'MCO', 'MLT', 'NLD', 'NOR', 'PRT', 'SJM', 'SMR', 
                           'SWE', 'TUR', 'VAT'],
                   'FSU': ['ARM', 'AZE', 'BLR', 'GEO', 'KAZ', 'KGZ', 'MDA', 'RUS', 'TJK', 'TKM', 'UKR', 'UZB'],
                   'PAO': ['AUS', 'JPN', 'NZL'],
                   'PAS': ['ASM', 'BRN', 'CCK', 'COK', 'CXR', 'FJI', 'FSM', 'IDN', 'KIR', 'KOR', 'MAC', 'MHL', 'MMR', 'MNP', 
                           'MYS', 'NCL', 'NFK', 'NIU', 'NRU', 'PCI', 'PCN', 'PHL', 'PLW', 'PNG', 'PYF', 'SGP', 'SLB', 'THA', 
                           'TKL', 'TLS', 'TON', 'TUV', 'TWN', 'VUT', 'WLF', 'WSM'],
                   'CHN': ['CHN', 'HKG'],
                   'RCPA': ['KHM', 'LAO', 'MNG', 'PRK', 'VNM']}

