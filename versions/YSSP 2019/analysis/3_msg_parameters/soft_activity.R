#########################################
# Build parameters: soft_activity_lo/up #
#########################################
build_activity('soft_activity_lo', 'lo', value_lo, imports.too = FALSE)
build_activity('soft_activity_up', 'up', value_up, imports.too = FALSE)

clean_up()