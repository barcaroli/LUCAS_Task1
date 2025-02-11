To execute the whole procedure, first you have to download from the link:

https://projects.gopa-group.com/unit/gopa/lucas-statictics-production/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2Funit%2Fgopa%2Flucas%2Dstatictics%2Dproduction%2FShared%20Documents%2FMicro%20data&FolderCTID=0x012000EA473A70070E82439A5569379EFEE84F&View=%7B6242612D%2DC5AF%2D4508%2DB0CE%2DC3C197EBA942%7D

the following datasets:

- LUCAS22_corrected_v4.csv
- sample_LUCAS_2022.csv
- areas_2015_2024.csv
- master_complete.RData
- countries.RData
- EU_population_2009_2023.csv
- EU_structure.csv


and from the link:

https://projects.gopa-group.com/unit/gopa/lucas-statictics-production/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2Funit%2Fgopa%2Flucas%2Dstatictics%2Dproduction%2FShared%20Documents%2FPrevious%20estimates&View=%7B6242612D%2DC5AF%2D4508%2DB0CE%2DC3C197EBA942%7D

the following folders:

- estimates2009
- estimates2012
- estimates2015
- estimates2018

The order of execution is:

1.prepare_LUCAS_input.R
2.Standard_estimates.R
2.all_years.estimates.R
2a.attribute_cal_wgts_standard.R
2a.EU_estimates.R
3.estimates_pro_two_phases.R
4.estimates_twoPhases.R
5.attribute_cal_wgts_twophase.R
6.all_years_estimates.R
7.EU_estimates.R
8a.check_estimates_artificial.R
8b.check_estimates_water.R
9.tables_country.R
10.render_plots.R
11.Compare_estimates_tables.R
11.Compare_estimates_tables_by_NUTS0.R
11.Compare_estimates_tables_by_NUTS1.R
11.Compare_estimates_tables_by_NUTS2.R
12.TwoPhases_NA_counts.R

The whole procedure can be execute in batch with the execute_procedure.bat



