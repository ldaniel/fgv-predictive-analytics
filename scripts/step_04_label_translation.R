# Translating relevant labels and domains to english --------------------------

table$column <- plyr::mapvalues(column$type, c('OWNER', 'DISPONENT'), c('Owner', 'User'))
