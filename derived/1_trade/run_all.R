############################
# Run all files in 1_trade #
############################
print('######################################')
print('Pre-processing trade data and IEA data')
print('######################################')
print('Running:')
print('...0_iea_ncv.R')
source(paste0(repo, 'derived/1_trade/0_iea_ncv.R'))
print('...1_import_baci.R')
source(paste0(repo, 'derived/1_trade/1_import_baci.R'))
print('...2_import_web.R')
source(paste0(repo, 'derived/1_trade/2_import_baci.R'))
print('...3_data_validation.R')
source(paste0(repo, 'derived/1_trade/3_data_validation.R'))