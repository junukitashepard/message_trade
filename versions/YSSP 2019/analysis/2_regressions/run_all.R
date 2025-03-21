##################################
# Run all files in 2_regressions #
##################################
print('############################')
print('Compile regression dataframe')
print('############################')
print('Running:')
print('...1_prep_data.R')
source(paste0(repo, 'analysis/2_regressions/1_prep_data.R'))
print('...2_add_gravity_terms.R')
source(paste0(repo, 'analysis/2_regressions/2_add_gravity_terms.R'))
print('...3_descriptives.R')
source(paste0(repo, 'analysis/2_regressions/3_descriptives.R'))
print('...4_regress.R')
source(paste0(repo, 'analysis/2_regressions/4_regress.R'))
#print('...5_compile_tables.R')
#source(paste0(repo, 'analysis/2_regressions/5_compile_tables.R'))
print('...6_compile_df.R')
source(paste0(repo, 'analysis/2_regressions/6_compile_df.R'))

