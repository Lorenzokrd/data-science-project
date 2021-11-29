library(cbsodataR)
cbs_get_meta("81528NED")$Woningkenmerken

# Energie verbruik van Amsterdam
energy_data <- cbs_get_data("81528NED", 
                            Perioden = has_substring("2020JJ00") | has_substring("2019JJ00") |
                              has_substring("2018JJ00"), RegioS = "GM0363",
                            Woningkenmerken = has_substring("T001100") | has_substring("ZW25810") |
                              has_substring("ZW10320") | has_substring("1014850"))
# Vermogen van Amsterdam
cbs_get_meta("85068NED")$KenmerkenHuishoudens

wealth_data <- cbs_get_data("85068NED", RegioS = "GM0363",
                            Perioden = has_substring("2020JJ00") | has_substring("2019JJ00") |
                              has_substring("2018JJ00"))

# Huiselijk geweld van Amsterdam
cbs_get_meta("84851NED")$Perioden

violence_data <- cbs_get_data("84851NED", RegioS = "GM0363",
                              Perioden = has_substring("2019JJ00") | has_substring("2020JJ00") |
                              has_substring("2021HJ01"))

# doodsoorzaken van Amsterdam
cbs_get_meta("80202ned")$Perioden

death_cause_data <- cbs_get_data("80202ned", RegioS = "GM0363",
                                 Perioden = has_substring("2018JJ00") | has_substring("2019JJ00") |
                                 has_substring("2020JJ00"))