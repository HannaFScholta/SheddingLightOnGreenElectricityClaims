###################################################################################################################
###################################################################################################################
####################################### ALL RES - ALL LEVELS OF ANALYSIS ##########################################
###################################################################################################################
###################################################################################################################

#Install necessary packages
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")

#load & execute packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#############################################################################################
############################### Hourly level of analysis ####################################
#############################################################################################

#Definition of input path and file name to import hourly ALL RES Coverage Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_ALL RES_vPub.xlsx")

#Read-in hourly_coverage file
file_path <- file.path(input_path, file_name)
hloa <- read_excel(file_path, sheet = "hourly_coverages")

#Definition of input path and file name to import hourly Solar generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Solar_vPub.xlsx")

#Read-in hourly solar generation file
file_path <- file.path(input_path, file_name)
hloa_generation_solar <- read_excel(file_path, sheet = "hourly_generation_solar")

#Definition of input path and file name to import hourly Wind generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Wind_vPub.xlsx")

#Read-in hourly wind generation file
file_path <- file.path(input_path, file_name)
hloa_generation_wind <- read_excel(file_path, sheet = "hourly_generation_wind")

############################### DATA CLEANING ###############################
#############################################################################

## Coverage File
#Data Clean und Value formatting (as numeric) of hourly coverages hloa
hloa_cleaned <- hloa %>% 
  filter(!is.na(.[,7])) #Remove all rows that show NA value in col 7
hloa <- hloa_cleaned
colnames (hloa)[2:5] <- c("GO Demand", "GO Supply", "Coverage", "Coverage/GO Demand") #Name cols 2-5
hloa <- hloa %>%
  rename(
    "Timestamp" = "Hourly Coverages" #Rename Column "Hourly Coverages" to "Timestamp"
  )
colnames (hloa)[7:8] <- c("Hour Index", "Year Index") #Name col 7 & 8
hloa <- hloa[-1,] #Remove first row
hloa <- hloa %>%
  select(-6,-9,-10) #Remove cols 6,9 & 10
hloa <- hloa %>%
  select(1,7,6,2:5) #Change order of cols
hloa <- hloa %>%
  mutate("Month Index" = substr(Timestamp,6,7)) #Add a new col, extracting the 6th until th 7th character of Timestamp
hloa <- hloa %>%
  select(1,2,8,3:7) #Change order of cols

hloa <- hloa %>%
  mutate_at(vars(5:8), as.numeric) #Ensure that Values in col 5-8 are numeric

## Solar generation file
# Extract the second column of `hloa_generation_solar` as a dataframe
solar_column <- hloa_generation_solar[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(solar_column) <- colnames(hloa_generation_solar)[2]

# Add the column at the end of hloa
hloa <- cbind(hloa, solar_column)

## Wind generation file
# Extract the second column of `hloa_generation_wind` as a dataframe
wind_column <- hloa_generation_wind[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(wind_column) <- colnames(hloa_generation_wind)[2]

# Add the column at the end of hloa
hloa <- cbind(hloa, wind_column)

############################### DATA ANALYSIS ###############################
#############################################################################

# Group by "Year Index" and find the row with the minimum "Coverage" for each year
hloa_min_coverages_df <- hloa %>%
  group_by(`Year Index`) %>%
  slice(which.min(Coverage)) %>%
  ungroup()

# Add a new column to the dataframe for the required percentage increase of solar
hloa_min_coverages_df <- hloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseSolar = ifelse(
      TotalGenerationValue_Solar > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Solar)*100,
      NA  # Set to NA if TotalGenerationValue_Solar is zero or missing
    )
  )

# Add a new column to the dataframe for the required percentage increase of wind
hloa_min_coverages_df <- hloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseWind = ifelse(
      TotalGenerationValue_Wind > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Wind)*100,
      NA  # Set to NA if TotalGenerationValue_Wind is zero or missing
    )
  )


#############################################################################################
################################ DAILY LEVEL OF ANALYSIS ####################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_ALL RES_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
dloa <- read_excel(file_path, sheet = "daily_coverages")

#Definition of input path and file name to import daily Solar generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Solar_vPub.xlsx")

#Read-in daily solar generation file
file_path <- file.path(input_path, file_name)
dloa_generation_solar <- read_excel(file_path, sheet = "daily_generation_solar")

#Definition of input path and file name to import daily Wind generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Wind_vPub.xlsx")

#Read-in daily wind generation file
file_path <- file.path(input_path, file_name)
dloa_generation_wind <- read_excel(file_path, sheet = "daily_generation_wind")

############################### DATA CLEANING ###############################
#############################################################################

#Data clean and value formatting (as numeric) of daily coverages dloa
dloa_cleaned <- dloa %>%
  filter(!is.na(.[,2])) #Remove all rows that show NA value in col 2
dloa <- dloa_cleaned
dloa <- dloa[-1,]
colnames(dloa)[2:5] <- c("GO Demand", "GO Supply", "Coverage", "Coverage/GO Demand") #Name cols 2-5
dloa <- dloa %>%
  rename(
    "Timestamp" = "Daily Coverages" #Rename Column "Daily Coverages" to "Timestamp"
  )
dloa <- dloa %>%
  mutate("Year Index" = substr(Timestamp,1,4))#Add a new col, extracting the 1st until th 4th character of Timestamp
dloa <- dloa %>%
  mutate("Month Index" = substr(Timestamp,6,7)) #Add a new col, extracting the 6th until th 7th character of Timestamp
dloa <- dloa %>%
  select(1,6,7,2:5) #Change order of cols
dloa <- dloa %>%
  mutate_at(vars(4:7), as.numeric) #Ensure that Values in col 5-8 are numeric

## Solar generation file
# Extract the second column of `dloa_generation_solar` as a dataframe
solar_column <- dloa_generation_solar[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(solar_column) <- colnames(dloa_generation_solar)[2]

# Add the column at the end of dloa
dloa <- cbind(dloa, solar_column)

## Wind generation file
# Extract the second column of `dloa_generation_wind` as a dataframe
wind_column <- dloa_generation_wind[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(wind_column) <- colnames(dloa_generation_wind)[2]

# Add the column at the end of dloa
dloa <- cbind(dloa, wind_column)


############################### DATA ANALYSIS ###############################
#############################################################################

# Group by "Year Index" and find the row with the minimum "Coverage" for each year
dloa_min_coverages_df <- dloa %>%
  group_by(`Year Index`) %>%
  slice(which.min(Coverage)) %>%
  ungroup()

# Add a new column to the dataframe for the required percentage increase of solar
dloa_min_coverages_df <- dloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseSolar = ifelse(
      TotalGenerationValue_Solar > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Solar)*100,
      NA  # Set to NA if TotalGenerationValue_Solar is zero or missing
    )
  )

# Add a new column to the dataframe for the required percentage increase of wind
dloa_min_coverages_df <- dloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseWind = ifelse(
      TotalGenerationValue_Wind > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Wind)*100,
      NA  # Set to NA if TotalGenerationValue_Wind is zero or missing
    )
  )

#############################################################################################
################################ WEEKLY LEVEL OF ANALYSIS ###################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_ALL RES_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
wloa <- read_excel(file_path, sheet = "weekly_coverages")

#Definition of input path and file name to import weekly Solar generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Solar_vPub.xlsx")

#Read-in weekly solar generation file
file_path <- file.path(input_path, file_name)
wloa_generation_solar <- read_excel(file_path, sheet = "weekly_generation_solar")

#Definition of input path and file name to import weekly Wind generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Wind_vPub.xlsx")

#Read-in weekly wind generation file
file_path <- file.path(input_path, file_name)
wloa_generation_wind <- read_excel(file_path, sheet = "weekly_generation_wind")

############################### DATA CLEANING ###############################
#############################################################################

#Data clean and value formatting (as numeric) to weekly coverages wloa
wloa_cleaned <- wloa %>%
  filter(!is.na(.[,5])) #Remove all rows that show NA value in col 2
wloa <- wloa_cleaned
wloa <- wloa[-1,]
colnames(wloa)[2:6] <- c("GO Demand", "GO Supply", "Coverage", "Year&Calendar Week","Coverage/GO Demand") #Name cols 2-5
wloa <- wloa %>%
  rename(
    "Timestamp" = "Weekly Coverages" #Rename col "weekly Coverages" in "Timestamp"
  )
wloa <- wloa %>%
  mutate("Year Index" = substr(Timestamp,1,4))#Add a new col, extracting the 1st until th 4th character of Timestamp
wloa <- wloa %>%
  mutate("CW Index" = substr(`Year&Calendar Week`,6,nchar(`Year&Calendar Week`))) #Add a new col, extracting the 6th until th 7th character of Timestamp
wloa <- wloa %>%
  select(1,5,7,8,2:4,6) #Change order of cols
wloa <- wloa %>%
  mutate_at(vars(5:8), as.numeric) #Ensure that Values in col 5-8 are numeric

## Solar generation file
# Extract the second column of `wloa_generation_solar` as a dataframe
solar_column <- wloa_generation_solar[-1, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(solar_column) <- colnames(wloa_generation_solar)[2]

# Add the column at the end of wloa
wloa <- cbind(wloa, solar_column)

## Wind generation file
# Extract the second column of `wloa_generation_wind` as a dataframe
wind_column <- wloa_generation_wind[-1, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(wind_column) <- colnames(wloa_generation_wind)[2]

# Add the column at the end of wloa
wloa <- cbind(wloa, wind_column)

############################### DATA ANALYSIS ###############################
#############################################################################

# Group by "Year Index" and find the row with the minimum "Coverage" for each year
wloa_min_coverages_df <- wloa %>%
  group_by(`Year Index`) %>%
  slice(which.min(Coverage)) %>%
  ungroup()

# Add a new column to the dataframe for the required percentage increase of solar
wloa_min_coverages_df <- wloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseSolar = ifelse(
      TotalGenerationValue_Solar > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Solar)*100,
      NA  # Set to NA if TotalGenerationValue_Solar is zero or missing
    )
  )

# Add a new column to the dataframe for the required percentage increase of wind
wloa_min_coverages_df <- wloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseWind = ifelse(
      TotalGenerationValue_Wind > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Wind)*100,
      NA  # Set to NA if TotalGenerationValue_Wind is zero or missing
    )
  )

#############################################################################################
################################ MONTHLY LEVEL OF ANALYSIS ##################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_ALL RES_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
mloa <- read_excel(file_path, sheet = "monthly_coverages")

#Definition of input path and file name to import monthly Solar generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Solar_vPub.xlsx")

#Read-in monthly solar generation file
file_path <- file.path(input_path, file_name)
mloa_generation_solar <- read_excel(file_path, sheet = "monthly_generation_solar")

#Definition of input path and file name to import monthly Wind generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Wind_vPub.xlsx")

#Read-in monthly wind generation file
file_path <- file.path(input_path, file_name)
mloa_generation_wind <- read_excel(file_path, sheet = "monthly_generation_wind")

############################### DATA CLEANING ###############################
#############################################################################

#Data clean and value formatting (as numeric) von monthly coverages mloa
mloa_cleaned <- mloa %>%
  filter(!is.na(.[,2])) #Remove all rows that show NA value in col 2
mloa <- mloa_cleaned
mloa <- mloa[-1,]
colnames(mloa)[2:5] <- c("GO Demand", "GO Supply", "Coverage", "Coverage/GO Demand") #Name cols 2-5
mloa <- mloa %>%
  rename(
    "Timestamp" = "Monthly Coverages" #Rename col "monthly Coverages" um in "Timestamp"
  )
mloa <- mloa %>%
  mutate("Year Index" = substr(Timestamp,1,4))#Add a new col, extracting the 1st until th 4th character of Timestamp
mloa <- mloa %>%
  mutate("Month Index" = substr(Timestamp,6,7)) #Add a new col, extracting the 6th until th 7th character of Timestamp
mloa <- mloa %>%
  select(1,6,7,2:5) #Change order of cols
mloa <- mloa %>%
  mutate_at(vars(4:7), as.numeric) #Ensure that Values in col 4-7 are numeric

## Solar generation file
# Extract the second column of `mloa_generation_solar` as a dataframe
solar_column <- mloa_generation_solar[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(solar_column) <- colnames(mloa_generation_solar)[2]

# Add the column at the end of mloa
mloa <- cbind(mloa, solar_column)

## Wind generation file
# Extract the second column of `mloa_generation_wind` as a dataframe
wind_column <- mloa_generation_wind[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(wind_column) <- colnames(mloa_generation_wind)[2]

# Add the column at the end of mloa
mloa <- cbind(mloa, wind_column)

############################### DATA ANALYSIS ###############################
#############################################################################

# Group by "Year Index" and find the row with the minimum "Coverage" for each year
mloa_min_coverages_df <- mloa %>%
  group_by(`Year Index`) %>%
  slice(which.min(Coverage)) %>%
  ungroup()

# Add a new column to the dataframe for the required percentage increase of solar
mloa_min_coverages_df <- mloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseSolar = ifelse(
      TotalGenerationValue_Solar > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Solar)*100,
      NA  # Set to NA if TotalGenerationValue_Solar is zero or missing
    )
  )

# Add a new column to the dataframe for the required percentage increase of wind
mloa_min_coverages_df <- mloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseWind = ifelse(
      TotalGenerationValue_Wind > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Wind)*100,
      NA  # Set to NA if TotalGenerationValue_Wind is zero or missing
    )
  )

#############################################################################################
############################### QUARTERLY LEVEL OF ANALYSIS #################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_ALL RES_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
qloa <- read_excel(file_path, sheet = "quarterly_coverages")

#Definition of input path and file name to import quarterly Solar generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Solar_vPub.xlsx")

#Read-in quarterly solar generation file
file_path <- file.path(input_path, file_name)
qloa_generation_solar <- read_excel(file_path, sheet = "quarterly_generation_solar")

#Definition of input path and file name to import quarterly Wind generation Data
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_Wind_vPub.xlsx")

#Read-in quarterly wind generation file
file_path <- file.path(input_path, file_name)
qloa_generation_wind <- read_excel(file_path, sheet = "quarterly_generation_wind")


############################### DATA CLEANING ###############################
#############################################################################

#Data clean and value formatting (as numeric) von quarterly coverages qloa
qloa_cleaned <- qloa %>%
  filter(!is.na(.[,2])) #Remove all rows that show NA value in col 2
qloa <- qloa_cleaned
qloa <- qloa[-1,]
colnames(qloa)[2:5] <- c("GO Demand", "GO Supply", "Coverage", "Coverage/GO Demand") #Name cols 2-5
qloa <- qloa %>%
  rename(
    "Timestamp" = "Quarterly Coverages" #Rename col "quarterly Coverages" to "Timestamp"
  )
qloa <- qloa %>%
  mutate("Year Index" = substr(Timestamp,1,4))#Add a new col, extracting the 1st until th 4th character of Timestamp
qloa <- qloa %>%
  mutate("Quarter Index" = substr(Timestamp,7,7)) #Add a new col, extracting the 7th until th 7th character of Timestamp
qloa <- qloa %>%
  select(1,6,7,2:5) #Change order of cols
qloa <- qloa %>%
  mutate_at(vars(4:7), as.numeric) #Ensure that Values in col 4-7 are numeric

## Solar generation file
# Extract the second column of `qloa_generation_solar` as a dataframe
solar_column <- qloa_generation_solar[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(solar_column) <- colnames(qloa_generation_solar)[2]

# Add the column at the end of qloa
qloa <- cbind(qloa, solar_column)

## Wind generation file
# Extract the second column of `qloa_generation_wind` as a dataframe
wind_column <- qloa_generation_wind[, 2, drop = FALSE]  # Keeps the column name

# Rename the column to match the original heading
colnames(wind_column) <- colnames(qloa_generation_wind)[2]

# Add the column at the end of qloa
qloa <- cbind(qloa, wind_column)

############################### DATA ANALYSIS ###############################
#############################################################################

# Group by "Year Index" and find the row with the minimum "Coverage" for each year
qloa_min_coverages_df <- qloa %>%
  group_by(`Year Index`) %>%
  slice(which.min(Coverage)) %>%
  ungroup()

# Add a new column to the dataframe for the required percentage increase of solar
qloa_min_coverages_df <- qloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseSolar = ifelse(
      TotalGenerationValue_Solar > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Solar)*100,
      NA  # Set to NA if TotalGenerationValue_Solar is zero or missing
    )
  )

# Add a new column to the dataframe for the required percentage increase of wind
qloa_min_coverages_df <- qloa_min_coverages_df %>%
  mutate(
    PercentageIncreaseWind = ifelse(
      TotalGenerationValue_Wind > 0,  # Avoid division by zero
      ((Coverage*(-1)) / TotalGenerationValue_Wind)*100,
      NA  # Set to NA if TotalGenerationValue_Wind is zero or missing
    )
  )



#############################################################################################
############################### COMBINE DATAFRAME & EXPORT ##################################
#############################################################################################


# List of objects to retain
retain_objects <- c(
  "hloa_min_coverages_df", 
  "dloa_min_coverages_df", 
  "wloa_min_coverages_df", 
  "mloa_min_coverages_df", 
  "qloa_min_coverages_df"
)

# Remove all objects except the ones in the list
rm(list = setdiff(ls(), retain_objects))

# Define a function to move "Year Index" to the first column
move_year_index <- function(df) {
  if ("Year Index" %in% colnames(df)) {
    df <- df[, c("Year Index", setdiff(colnames(df), "Year Index"))]
  }
  return(df)
}

# Apply the function to each dataframe
hloa_min_coverages_df <- move_year_index(hloa_min_coverages_df)
dloa_min_coverages_df <- move_year_index(dloa_min_coverages_df)
wloa_min_coverages_df <- move_year_index(wloa_min_coverages_df)
mloa_min_coverages_df <- move_year_index(mloa_min_coverages_df)
qloa_min_coverages_df <- move_year_index(qloa_min_coverages_df)

# Define a function to insert a new column as the second column
insert_second_column <- function(df, df_name) {
  # Extract the first four letters of the dataframe's name
  new_column_value <- substr(df_name, 1, 4)
  
  # Create the new column as a character vector
  new_column <- rep(new_column_value, nrow(df))
  
  # Add the new column as the second column
  df <- cbind(df[, 1, drop = FALSE], LevelOfAnalysis = new_column, df[, -1])
  
  return(df)
}

# Apply the function to each dataframe
hloa_min_coverages_df <- insert_second_column(hloa_min_coverages_df, "hloa_min_coverages_df")
dloa_min_coverages_df <- insert_second_column(dloa_min_coverages_df, "dloa_min_coverages_df")
wloa_min_coverages_df <- insert_second_column(wloa_min_coverages_df, "wloa_min_coverages_df")
mloa_min_coverages_df <- insert_second_column(mloa_min_coverages_df, "mloa_min_coverages_df")
qloa_min_coverages_df <- insert_second_column(qloa_min_coverages_df, "qloa_min_coverages_df")


# Combine the dataframes with all unique columns
combined_df <- bind_rows(
  hloa_min_coverages_df,
  dloa_min_coverages_df,
  wloa_min_coverages_df,
  mloa_min_coverages_df,
  qloa_min_coverages_df
)

# Export combined_df as a CSV file to the specified path
output_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/3_Data Analysis/2_R Results/1_Main Results"
output_file_name <- "ALLRES_Min_Coverages_Scale-Up Solar&Wind.csv"
output_file_path <- file.path(output_path, output_file_name)

# Write the dataframe to the file
write.csv(combined_df, file = output_file_path, row.names = FALSE)

# Confirmation message
cat("The combined dataframe has been successfully exported to:", output_file_path, "\n")
