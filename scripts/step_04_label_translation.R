# Translating relevant labels and domains to english --------------------------

disposition$type <- plyr::mapvalues(disposition$type, c('OWNER', 'DISPONENT'), c('Owner', 'User'))

account$frequency <- plyr::mapvalues(account$frequency,
                                     c('POPLATEK MESICNE', 'POPLATEK TYDNE', 'POPLATEK PO OBRATU'),
                                     c('Monthly', 'Weekly', 'On Transaction'))

permanent_order$k_symbol <- plyr::mapvalues(permanent_order$k_symbol,
                                            c('POJISTNE', 'SIPO', 'LEASING', 'UVER'),
                                            c('insurrance payment', 'household', 'leasing', 'loan payment'))

transaction$type <- plyr::mapvalues(transaction$type,
                                    c('PRIJEM', 'VYDAJ', 'VYBER'),
                                    c('credit', 'withdrawal', 'withdrawal in cash'))

transaction$operation <- plyr::mapvalues(transaction$operation,
                                         c('VYBER KARTOU', 'VKLAD', 'PREVOD Z UCTU', 'VYBER', 'PREVOD NA UCET'),
                                         c('credit card withdrawal', 'credit in cash', 
                                           'collection from another bank', 'withdrawal in cash', 
                                           'remittance to nother bank'))

transaction$k_symbol <- plyr::mapvalues(transaction$k_symbol, 
                                        c('POJISTNE', 'SLUZBY', 'UROK', 'SANKC. UROK', 'SIPO', 'DUCHOD', 'UVER'),
                                        c('insurance payment', 'statement', 'interest credited',
                                          'sanction interest', 'household', 'old age pension', 'loan payment'))
