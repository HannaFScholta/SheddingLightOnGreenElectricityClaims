# EXCEL WRITE SCRIPT

install.packages("openxlsx")
library(openxlsx)


###################################################################################################################
###################################################################################################################
################################################### GENERATION ####################################################
###################################################################################################################
###################################################################################################################

############################# ALL RENEWABLES ###############################
############################################################################

# Create new Excel Workbook
wb <- createWorkbook()

# Insert the data frames as sheets in the workbook
addWorksheet(wb, "hourly_generation")
writeData(wb, sheet = "hourly_generation", generation_hourly_AIB)

addWorksheet(wb, "daily_generation")
writeData(wb, sheet = "daily_generation", generation_daily_AIB)

addWorksheet(wb, "weekly_generation")
writeData(wb, sheet = "weekly_generation", generation_weekly_AIB)

addWorksheet(wb, "monthly_generation")
writeData(wb, sheet = "monthly_generation", generation_monthly_AIB)

addWorksheet(wb, "yearly_generation")
writeData(wb, sheet = "yearly_generation", generation_yearly_AIB)

addWorksheet(wb, "ARTIFICIAL")
writeData(wb, sheet = "ARTIFICIAL", generation_filled_artificial)

addWorksheet(wb, "CTY per GenType")
writeData(wb, sheet = "CTY per GenType", gen_prodtypes)

addWorksheet(wb, "GenType per CTY")
writeData(wb, sheet = "GenType per CTY", gen_prodtypes_CTY)

# Save the workbook as excel file
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/3_Outputs")
saveWorkbook(wb, "generation_AIB.xlsx", overwrite = TRUE)

################################# SOLAR ####################################
############################################################################

# Create new Excel Workbook
wb <- createWorkbook()

# Insert the data frames as sheets in the workbook
addWorksheet(wb, "hourly_generation_solar")
writeData(wb, sheet = "hourly_generation_solar", generation_hourly_AIB_solar)

addWorksheet(wb, "daily_generation_solar")
writeData(wb, sheet = "daily_generation_solar", generation_daily_AIB_solar)

addWorksheet(wb, "weekly_generation_solar")
writeData(wb, sheet = "weekly_generation_solar", generation_weekly_AIB_solar)

addWorksheet(wb, "monthly_generation_solar")
writeData(wb, sheet = "monthly_generation_solar", generation_monthly_AIB_solar)

addWorksheet(wb, "yearly_generation_solar")
writeData(wb, sheet = "yearly_generation_solar", generation_yearly_AIB_solar)

addWorksheet(wb, "ARTIFICIAL_solar")
writeData(wb, sheet = "ARTIFICIAL_solar", generation_filled_artificial_solar)

# Save the workbook as excel file
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/3_Outputs")
saveWorkbook(wb, "generation_AIB_solar.xlsx", overwrite = TRUE)

################################## WIND ####################################
############################################################################

# Create new Excel Workbook
wb <- createWorkbook()

# Insert the data frames as sheets in the workbook
addWorksheet(wb, "hourly_generation_wind")
writeData(wb, sheet = "hourly_generation_wind", generation_hourly_AIB_wind)

addWorksheet(wb, "daily_generation_wind")
writeData(wb, sheet = "daily_generation_wind", generation_daily_AIB_wind)

addWorksheet(wb, "weekly_generation_wind")
writeData(wb, sheet = "weekly_generation_wind", generation_weekly_AIB_wind)

addWorksheet(wb, "monthly_generation_wind")
writeData(wb, sheet = "monthly_generation_wind", generation_monthly_AIB_wind)

addWorksheet(wb, "yearly_generation_wind")
writeData(wb, sheet = "yearly_generation_wind", generation_yearly_AIB_wind)

addWorksheet(wb, "ARTIFICIAL_wind")
writeData(wb, sheet = "ARTIFICIAL_wind", generation_filled_artificial_wind)

# Save the workbook as excel file
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/3_Outputs")
saveWorkbook(wb, "generation_AIB_wind.xlsx", overwrite = TRUE)

############################### SOLAR & WIND ###############################
############################################################################

# Create new Excel Workbook
wb <- createWorkbook()

# Insert the data frames as sheets in the workbook
addWorksheet(wb, "hourly_generation_solarwind")
writeData(wb, sheet = "hourly_generation_solarwind", generation_hourly_AIB_solarwind)

addWorksheet(wb, "daily_generation_solarwind")
writeData(wb, sheet = "daily_generation_solarwind", generation_daily_AIB_solarwind)

addWorksheet(wb, "weekly_generation_solarwind")
writeData(wb, sheet = "weekly_generation_solarwind", generation_weekly_AIB_solarwind)

addWorksheet(wb, "monthly_generation_solarwind")
writeData(wb, sheet = "monthly_generation_solarwind", generation_monthly_AIB_solarwind)

addWorksheet(wb, "yearly_generation_solarwind")
writeData(wb, sheet = "yearly_generation_solarwind", generation_yearly_AIB_solarwind)

# Save the workbook as excel file
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/3_Outputs")
saveWorkbook(wb, "generation_AIB_solarwind.xlsx", overwrite = TRUE)


###################################################################################################################
###################################################################################################################
###################################################### LOAD #######################################################
###################################################################################################################
################################################################################################################### 

# Create new Excel Workbook
wb <- createWorkbook()

# Insert the data frames as sheets in the workbook
addWorksheet(wb, "hourly_load")
writeData(wb, sheet = "hourly_load", load_hourly_AIB)

addWorksheet(wb, "daily_load")
writeData(wb, sheet = "daily_load", load_daily_AIB)

addWorksheet(wb, "weekly_load")
writeData(wb, sheet = "weekly_load", load_weekly_AIB)

addWorksheet(wb, "monthly_load")
writeData(wb, sheet = "monthly_load", load_monthly_AIB)

addWorksheet(wb, "yearly_load")
writeData(wb, sheet = "yearly_load", load_yearly_AIB)

addWorksheet(wb, "ARTIFICIAL")
writeData(wb, sheet = "ARTIFICIAL", load_filled_artificial)

# Save the workbook as excel file
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/3_Outputs")
saveWorkbook(wb, "load_AIB.xlsx", overwrite = TRUE)
