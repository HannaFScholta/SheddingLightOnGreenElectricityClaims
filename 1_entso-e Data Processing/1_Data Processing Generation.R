#install.packages("dplyr")
#install.packages("lubridate")

library(dplyr)
library(lubridate)

y1 <- 2016
y2 <- 2021
ndays_b <- 60 # Originally set to 4 days; only CY shows gaps larger than 4 days - those were filled via approach B - see method section of paper.


###################################################################################################################
###################################################################################################################
################################################## DATA INTAKE ####################################################
###################################################################################################################
###################################################################################################################

#### WHAT THIS CODE SECTIONS DOES ####
# generation_raw_data: Read in all csv generation files to a dataframe (countrylevel (CTY) data)
# generation_combined_data: Select columns with timestamps, resolution codes (i.e. measurement intervals), country codes, generation values
# generation_combined_data: Filter for selected countries of analysis
# generation_combined_data: Convert resolution codes to numeric (unit: minutes)
# generation_combined_data: Filter data for all green energy types (referred to as production types in the dataset)

#Set working directory to folder containing entso-e data on actual generation per production type from https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/show, accessed 2023-04-04
setwd("INSERTPATH/Generation Dataset")

generation_raw_data <- data.frame()

# loop through all generation csv files
for (year in y1:y2) {
  for (month in 1:12) {
    # use sprintf to force the month to be always two digits long, otherwise it would not find the files with months 01-09
    file_name <- paste0(year, "_", sprintf("%02d", month), "_AggregatedGenerationPerType_16.1.B_C.csv")
    
    if (file.exists(file_name)) {
      # read in the countrylevel data in file and combine it with the existing data
      print(file_name)
      new_data <- read.csv(file_name, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      new_data <- new_data[new_data$AreaTypeCode == 'CTY',]
      generation_raw_data <- rbind(generation_raw_data, new_data)
    } else {
      print(paste(file_name, "was not found"))
    }
  }
}
rm(new_data)

names(generation_raw_data)[names(generation_raw_data) == 'ï..DateTime'] <- 'DateTime'

generation_combined_data <- generation_raw_data[,c("DateTime", "ResolutionCode", "MapCode", "ProductionType", "ActualGenerationOutput")]

# Filter countries according to their relevance for analysis
generation_combined_data <- generation_combined_data[!(generation_combined_data$MapCode %in% c("AL","BA","BG","GB","GE","GR","HU","LT","ME","MD","MK","PL","RO","UA","XK")),]

# Save Resolution Code as numeric
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT60M"] <- 60
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT30M"] <- 30
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT15M"] <- 15
generation_combined_data$ResolutionCode <- as.numeric(generation_combined_data$ResolutionCode)

# Filter for green energy generation types
generation_combined_data <- generation_combined_data[generation_combined_data$ProductionType
                                                     %in% c("Biomass","Geothermal","Hydro Pumped Storage",
                                                            "Hydro Run-of-river and poundage","Hydro Water Reservoir",
                                                            "Marine","Other renewable","Solar","Waste",
                                                            "Wind Offshore","Wind Onshore"),]

print("Head of generation_combined_data:")
head(generation_combined_data)

# Save archive copy for later processing
generation_combined_data_archive <- generation_combined_data


###################################################################################################################
###################################################################################################################
##################### INITIAL DATA SET EXPLORATION (e.g. CHEKCKING FOR MISSING VALUES) ############################
###################################################################################################################
###################################################################################################################

# Display unique values from the ProductionType column
unique_production_types <- unique(generation_raw_data$ProductionType)
##Result: All relevant renwable production types are considered

# Count NA values in the TotalGenerationValue column of generation_combined_data 
na_count_combined_data_actual_generation_output <- sum(is.na(generation_combined_data$ActualGenerationOutput))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_actual_generation_output)
##Result: There are NA values in the data set

# Calculate NA counts by MapCode and ProductionType
na_counts_by_mapcode_type <- generation_combined_data %>%
  group_by(MapCode, ProductionType) %>%
  summarize(NA_Count = sum(is.na(ActualGenerationOutput)), .groups = 'drop')
##RESULT: NA Values mostly in countrys with Hourly Resultion; Exemption: IE -> 30min Resolution

# NA analysis: Deep Dive BE - Filter the data for MapCode 'BE' and ProductionType 'Geothermal'
na_analysis_BE_v2 <- generation_raw_data %>%
  filter(MapCode == "BE" & ProductionType == "Geothermal")
##RESULT: Non existent production types are not part of the data set

# NA analysis: Deep Dive BE - Filter the data for MapCode 'BE' and ProductionType 'Hydro Pumped Storage'
na_analysis_BE <- generation_raw_data %>%
  filter(MapCode == "BE" & ProductionType == "Hydro Pumped Storage")
##RESULT: NAs are blank values throughout the course of day; however, there are also a few hours per day for which values have been reported 
  ## Assumption: Empty values = No production

##CONCLUSION: Set NA Values in data set to zero, as no values have been reported for these hours

# Set the NAs to Zero
generation_combined_data$ActualGenerationOutput[is.na(generation_combined_data$ActualGenerationOutput)] <- 0

#Check
# Count NA values in the TotalGenerationValue column of generation_combined_data 
na_count_combined_data_actual_generation_output <- sum(is.na(generation_combined_data$ActualGenerationOutput))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_actual_generation_output)
##Result: There are no more NA values in the data set

#Check NAs in DateTime

# Count NA values in the DateTime column of generation_combined_data 
na_count_combined_data_date_time <- sum(is.na(generation_combined_data$DateTime))

# Display na_count_combined_data_date_time
print(na_count_combined_data_date_time)
##Result: There are no NA values for DateTime in the data set

#############################################################################################################
#############################################################################################################
################## INITIAL VALUE AGGREGATION TO HARMONIZE DATA TO HOURLY TIMESTAMPs #########################
#############################################################################################################
#############################################################################################################

#### WHAT THIS CODE SECTIONS DOES ####
# As the different green energy types are to be summed up by country and by timestamps, they need to be harmonized to an hourly level based on their resolution code
# generation_hourly_data: Aggregate data in generation_combined_data per hourly timestamps, countries and production types (aggregation function: mean – as different resolution codes exist)

#Convert DateTime column to POSIXct format
generation_combined_data$DateTime <- ymd_hms(generation_combined_data$DateTime)

# Create a new column for the hour component of DateTime
generation_combined_data$Hour <- format(generation_combined_data$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate the data by Hour and MapCode
generation_hourly_data <- aggregate(x = generation_combined_data$ActualGenerationOutput,
                                    by = list(Hour = generation_combined_data$Hour,
                                              MapCode = generation_combined_data$MapCode,
                                              ProductionType = generation_combined_data$ProductionType),
                                    FUN = mean)

# Rename the columns and display result
colnames(generation_hourly_data) <- c("DateTime", "MapCode", "ProductionType", "TotalGenerationValue")

# Save for archive
generation_hourly_data_archive <- generation_hourly_data

#CHECK: No new NAs came up
# Count NA values in the TotalGenerationValue column of generation_hourly_aggregated 
na_count_hourly_data_total_generation_value <- sum(is.na(generation_hourly_data$total_generation_value))

# Display na_count_hourly_aggregated_total_generation_value
print(na_count_hourly_data_total_generation_value)
## RESULT: NO new NAs came up

# Aggregate values over the renewable production types per hourly timestamp & MapCode
generation_hourly_reduced <- generation_hourly_data[,c("DateTime", "MapCode", "TotalGenerationValue")]
generation_hourly_aggregated <- aggregate(x = generation_hourly_reduced$TotalGenerationValue,
                                          by = list(Hour = generation_hourly_reduced$DateTime,
                                                    MapCode = generation_hourly_reduced$MapCode),
                                          FUN = sum)

# Rename the columns and display result
colnames(generation_hourly_aggregated) <- c("DateTime", "MapCode", "TotalGenerationValue")
print("Head of generation_hourly_aggregated:")
head(generation_hourly_aggregated)


###################################################################################################################
###################################################################################################################
################################################ DATA IMPUTATION ##################################################
###################################################################################################################
###################################################################################################################

#############################################################################################
################################# COMPLETE DF CREATION ######################################
#############################################################################################

#### WHAT THIS CODE SECTIONS DOES ####
# generation_complete: Create a dataframe structure for all countries with complete timestamps for their respective period of analysis
# FOR each country do:
    # generation_map_code: Create empty dataframe which is complete with all timestamps within the respective period of analysis
    # Add a column "Artificial" to track artificial value creation
      # Possible values of Artificial: 0: Total Generation is original value; 1: Total Generation must be filled artificially; 
      # 1s will later be replaced depending on used data imputation approach --> A: Approach A; B: Approach B; H: Filled via h-Scaling Factors
      # In this step, only the values 0 and 1 are assigned based on whether the timestamp existed already in the original dataset. 
      # All rows with value 1 are then handled later by artificial value creation.
      # Specifically in this step, this means that all timestamps which were missing in the entso-e data set are marked to be filled artificially.
      # As all countries (with exception of CY (1) and SE(2)) show at least three types of green energy production in the original dataset, 
      # it is assumed that at missing timestamps the accumulated green energy production must be filled artificially.
      # If the accumulated generation value of any timestamp has already a generation value of 0, this value remains and is therefore not filled artificially.
    # Update generation_map_code: Fill in the existing generation data of the country from generation_hourly_aggregated into generation_map_code
    # Add the now complete dataframe of the country to the overall dataframe "generation_complete"
# END FOR

# Post-Processing for Cleanup and Standardization:
    #Remove Duplicates Due to DST Ends (At 1pm UTC clock are set an hour back):
      #Ensure all DateTime entries are in POSIXct format.
      #Re-apply POSIXct to standardize the format.
      #Split the DataFrame by MapCode.
      #Remove duplicates within each group based on DateTime.
      #Reassemble the DataFrame.
      #Reset row names after reassembling to clean up the indices.
    #Complete Cleanup
      #Final DataFrame: Store the cleaned and combined data in generation_complete, which is now devoid of duplicates

# Initial DataFrame construction
generation_combined_data_v2 <- cbind(generation_hourly_aggregated[, "DateTime"],
                                  ResolutionCode = 60,
                                  generation_hourly_aggregated[, 2:ncol(generation_hourly_aggregated)])
colnames(generation_combined_data_v2) <- c("DateTime", "ResolutionCode", "MapCode", "TotalGenerationValue")

  # Check format of DateTime
  class(generation_combined_data_v2$DateTime)

  # Count NA values in the DateTime column of generation_combined_data_v2
  na_count_combined_data_v2_date_time <- sum(is.na(generation_combined_data_v2$DateTime))
  
  # Display na_count_combined_data_v2_date_time
  print(na_count_combined_data_v2_date_time)
  ## RESULT: NO new NAs came up

# Convert DateTime column to POSIXct format 
generation_combined_data_v2$DateTime <- ymd_hms(generation_combined_data_v2$DateTime)

  # Count NA values in the DateTime column of generation_combined_data_v2
  na_count_combined_data_v2_date_time <- sum(is.na(generation_combined_data_v2$DateTime))
  
  # Display na_count_combined_data_v2_date_time
  print(na_count_combined_data_v2_date_time)
  ## RESULT: NO new NAs came up

# Unique combinations of MapCode and ResolutionCode 
generation_resolution_code <- unique(generation_combined_data_v2[, c("MapCode", "ResolutionCode")])

# Initialize the complete dataset 
generation_complete <- data.frame(DateTime = character(),
                                  ResolutionCode = numeric(),
                                  MapCode = character(),
                                  TotalGenerationValue = numeric(),
                                  Artificial = character(),
                                  stringsAsFactors = FALSE)

# Loop over each MapCode and ResolutionCode
for (i in 1:nrow(generation_resolution_code)) {
  map_code <- generation_resolution_code[i, "MapCode"]
  resolution_code <- generation_resolution_code[i, "ResolutionCode"]
  
  # Determining start and end dates // Avoid starting already from prior date 23:00 (01 instead of 00)
  start_date <- as.POSIXct("2016-01-01 01:00:00")
  if (map_code == "LV") start_date <- as.POSIXct("2018-01-01 01:00:00")
  if (map_code == "RS" || map_code == "SK") start_date <- as.POSIXct("2019-10-01 01:00:00")
  if (map_code == "PT") start_date <- as.POSIXct("2020-01-01 01:00:00")
  end_date <- as.POSIXct("2021-12-31 23:00:00")
  
  # Generate all timestamps starting from the exact start_date
  timestamps <- seq(from = start_date, 
                    to = as.POSIXct(paste(end_date, "23:45:00")),
                    by = paste(resolution_code, "min"))
  
  # Subset and filter existing data for this map code 
  generation_map_code <- subset(generation_combined_data_v2, MapCode == map_code &
                                  DateTime >= as.POSIXct(paste(start_date, "00:00:00")))
  generation_map_code$Artificial <- "0"
  
  # Identify timestamps that are missing in the existing data 
  timestamps_missing <- setdiff(timestamps, generation_map_code$DateTime)
  
  # Handling missing timestamps 
  if (length(timestamps_missing) > 0) {
    generation_map_code_add <- data.frame(DateTime = timestamps_missing,
                                          ResolutionCode = resolution_code,
                                          MapCode = map_code,
                                          TotalGenerationValue = NA,
                                          Artificial = "1")
    
    # Combine and sort full records for this map code
    generation_map_code_all <- rbind(generation_map_code, generation_map_code_add)
    generation_map_code_all <- generation_map_code_all[order(generation_map_code_all$DateTime), ]
    rownames(generation_map_code_all) <- NULL
    
    # Append to the complete dataset
    generation_complete <- rbind(generation_complete, generation_map_code_all)
  } else {
    generation_complete <- rbind(generation_complete, generation_map_code)
  }
  
  # Cleanup 
  rm(generation_map_code)
  rownames(generation_complete) <- NULL
}

# Check the earliest date in generation_complete
min(generation_complete$DateTime)

####Remove the non-existent hour due to the start of the DST
# Define the specific dates and time to exclude
dates_to_exclude <- as.POSIXct(c("2016-10-30 02:00:00", "2017-10-29 02:00:00", 
                                 "2018-10-28 02:00:00", "2019-10-27 02:00:00", 
                                 "2020-10-25 02:00:00", "2021-10-31 02:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Filter out the rows with the specified datetimes
generation_complete <- generation_complete[!generation_complete$DateTime %in% dates_to_exclude, ]

# Verify that the specific dates and times have been removed
any(generation_complete$DateTime %in% dates_to_exclude)  # This should return FALSE if all specified instances are removed

###Check for number of missing timestamps & total timestamps per country
# Subsetting the dataframe where Artificial is "1" and counting rows
number_of_ones <- nrow(generation_complete[generation_complete$Artificial == "1", ])
print(number_of_ones)

# Count the number of rows for each MapCode 
number_of_rows_per_mapcode <- generation_complete %>%
  group_by(MapCode) %>%
  summarise(Count = n())

# Print the results
print(number_of_rows_per_mapcode)

#############################################################################################
############################### IMPUTATION OF ICELAND DATA ##################################
#############################################################################################

######################## NORWAY HYDRO DATASET for IS ########################
#############################################################################

#################### NORWAY HYDRO - Data Subsetting #########################

# Filter for NO renewable production types
generation_combined_NO <- generation_combined_data_archive
generation_combined_NO <- generation_combined_NO[generation_combined_NO$MapCode == "NO" &
                                                   (generation_combined_NO$ProductionType %in%
                                                      c("Hydro Pumped Storage","Hydro Run-of-river and poundage","Hydro Water Reservoir","Marine")),]
  # Check for NAs in the data
  na_count_combined_NO_actual_generation_output <- sum(is.na(generation_combined_NO$ActualGenerationOutput))
  na_count_combined_NO_date_time <- sum(is.na(generation_combined_NO$DateTime))
  
  # Display na_count_combined_data_actual_generation_output
  print(na_count_combined_NO_actual_generation_output)
  print(na_count_combined_NO_date_time)
  ##Result: There are no NA values in the data set

# Convert DateTime column to POSIXct format
generation_combined_NO$DateTime <- ymd_hms(generation_combined_NO$DateTime)

  # Check for NAs in the data
  na_count_combined_NO_actual_generation_output <- sum(is.na(generation_combined_NO$ActualGenerationOutput))
  na_count_combined_NO_date_time <- sum(is.na(generation_combined_NO$DateTime))
  
  # Display na_count_combined_data_actual_generation_output
  print(na_count_combined_NO_actual_generation_output)
  print(na_count_combined_NO_date_time)
  ##Result: There are no NA values in the data set

# Create a new column for the hour component of DateTime
generation_combined_NO$Hour <- format(generation_combined_NO$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate the data by Hour and MapCode and calculate the average hourly load value
generation_hourly_NO <- aggregate(x = generation_combined_NO$ActualGenerationOutput,
                                  by = list(Hour = generation_combined_NO$Hour,
                                            MapCode = generation_combined_NO$MapCode,
                                            ProductionType = generation_combined_NO$ProductionType),
                                  FUN = mean)

# Rename the columns and display result
colnames(generation_hourly_NO) <- c("DateTime", "MapCode", "ProductionType", "TotalGenerationValue")

# Re-check to ensure no new NAs have come up
na_count_hourly_NO_total_generation_value <- sum(is.na(generation_hourly_NO$TotalGenerationValue))
na_count_combined_NO_date_time <- sum(is.na(generation_combined_NO$DateTime))

# Display na_count_combined_data_actual_generation_output
print(na_count_hourly_NO_total_generation_value)
print(na_count_combined_NO_date_time)
##Result: There are still no NA values in the data set


######################### NORWAY HYDRO - Data Prep ##########################

# Aggregate values over hydro production types per hourly timestamp
generation_hourly_NO_reduced <- generation_hourly_NO[,c("DateTime", "MapCode", "TotalGenerationValue")]
generation_hourly_NO_reduced$TotalGenerationValue <- as.numeric(generation_hourly_NO_reduced$TotalGenerationValue)
generation_hourly_NO_aggregated <- aggregate(x = generation_hourly_NO_reduced$TotalGenerationValue,
                                             by = list(Hour = generation_hourly_NO_reduced$DateTime,
                                                       MapCode = generation_hourly_NO_reduced$MapCode),
                                             FUN = sum)
rm(generation_hourly_NO_reduced)
# Rename the columns and display result
colnames(generation_hourly_NO_aggregated) <- c("DateTime", "MapCode", "TotalGenerationValue")
# Convert DateTime column to POSIXct format
generation_hourly_NO_aggregated$DateTime <- ymd_hms(generation_hourly_NO_aggregated$DateTime)

print("Head of generation_hourly_aggregated:")
head(generation_hourly_NO_aggregated)


#################### NORWAY HYDRO - COMPLETE DF CREATION ####################

#Find out which timestamps are missing by mapping to complete set of timestamps for period of analysis
generation_combined_NO <- cbind(generation_hourly_NO_aggregated[, "DateTime"], ResolutionCode = 60, generation_hourly_NO_aggregated[, 2:ncol(generation_hourly_NO_aggregated)])
colnames(generation_combined_NO) <- c("DateTime", "ResolutionCode", "MapCode", "TotalGenerationValue")

generation_complete_NO <- data.frame(DateTime = character(),
                                     ResolutionCode = numeric(),
                                     MapCode = character(),
                                     TotalGenerationValue = numeric(),
                                     Artificial = character(),
                                     stringsAsFactors = FALSE)

map_code <- "NO"

start_date <- as.POSIXct("2016-01-01 01:00:00")
end_date <- as.Date("2021-12-31 23:00:00")

resolution_code <- 60

start_date_time <- as.POSIXct(paste(start_date, "00:00:00"))

generation_NO <- generation_combined_NO[generation_combined_NO$MapCode == map_code & generation_combined_NO$DateTime >= start_date_time,]
generation_NO <- generation_NO[!is.na(generation_NO$DateTime),]
generation_NO$Artificial <- "0"

timestamps <- seq(from = start_date, 
                  to = as.POSIXct(paste(end_date, "23:45:00")),
                  by = paste(resolution_code, "min"))

timestamps_missing <- timestamps[!(timestamps %in% generation_NO$DateTime)]

print(length(timestamps_missing))
print(map_code)

# Create missing observations
if (length(timestamps_missing) > 0) { # Missing values
  generation_NO_add <- data.frame(DateTime = timestamps_missing,
                                  ResolutionCode = resolution_code,
                                  MapCode = map_code,
                                  TotalGenerationValue = "")
  generation_NO_add$Artificial <- "1"
  
  # Bind original and additional observations and sort them
  generation_NO_all <- rbind(generation_NO, generation_NO_add)
  generation_NO_all <- arrange(generation_NO_all, DateTime)
  rownames(generation_NO_all) <- NULL
  
  generation_complete_NO <- rbind(generation_complete_NO, generation_NO_all)
  rm(generation_NO_add)
  rm(generation_NO_all)
} else { # No values missing
  generation_complete_NO <- rbind(generation_complete_NO, generation_NO)
}

rm(generation_NO)
rownames(generation_complete_NO) <- NULL

# Count the number of rows with MapCoNO "NO"
number_of_rows_NO <- nrow(generation_complete_NO[generation_complete_NO$MapCode == "NO", ])
print(number_of_rows_NO)

####Remove the non-existent hour due to the start of the DST
# Define the specific dates and time to exclude
dates_to_exclude <- as.POSIXct(c("2016-10-30 02:00:00", "2017-10-29 02:00:00", 
                                 "2018-10-28 02:00:00", "2019-10-27 02:00:00", 
                                 "2020-10-25 02:00:00", "2021-10-31 02:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Filter out the rows with the specified datetimes
generation_complete_NO <- generation_complete_NO[!generation_complete_NO$DateTime %in% dates_to_exclude, ]

# Verify that the specific dates and times have been removed
any(generation_complete_NO$DateTime %in% dates_to_exclude)  # This should return FALSE if all specified instances are removed

# Count the number of rows with MapCoNO "NO"
number_of_rows_NO <- nrow(generation_complete_NO[generation_complete_NO$MapCode == "NO", ])
print(number_of_rows_NO)

# Check for rows where Artificial equals '1' in generation_complete_NO
artificial_rows_count <- generation_complete_NO %>%
  filter(Artificial == "1") %>%
  nrow()

# Print the result
if(artificial_rows_count > 0) {
  print(paste("There are", artificial_rows_count, "rows with Artificial equal to 1."))
} else {
  print("There are no rows with Artificial equal to 1.")
}
##RESULT: There are 83 rows with Artificial equal to 1


################# NORWAY HYDRO - ARTIFICIAL VALUE GENERATION ################

# Create dataframes
generation_NO_hydro <- data.frame(DateTime = character(),
                                  ResolutionCode = numeric(),
                                  MapCode = character(),
                                  TotalGenerationValue = numeric(),
                                  Artificial = character(),
                                  stringsAsFactors = FALSE)

generation_NO_artificial <- data.frame(DateTime = character(),
                                       ResolutionCode = numeric(),
                                       MapCode = character(),
                                       TotalGenerationValue = numeric(),
                                       Artificial = character(),
                                       stringsAsFactors = FALSE)

# Limit iterations to MapCode NO (Generic Code Snippet as also used for other countries)

map_codes <- "NO"

for (map_code in map_codes) {
  # Filter the dataframe generation_complete_NO for NO MapCode
  generation_complete_map_code <- generation_complete_NO
  
  # Check for and fill data gaps, where approach A is to be used
  for (i in 1:(nrow(generation_complete_map_code))) {
    if (generation_complete_map_code[i, "Artificial"] == "1") {
      
      # If data is missing directly at the first measuring point, but the following measuring points are available, use the following measuring point
      if (i == 1 && generation_complete_map_code[i+1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i+1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      # If data is missing at the last measuring point, but the measuring points before it are available, use the previous measuring point
      if (i == nrow(generation_complete_map_code) && generation_complete_map_code[i-1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i-1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      previous_value <- as.numeric(generation_complete_map_code[i - 1, "TotalGenerationValue"])
      previous_artificial <- generation_complete_map_code[i - 1, "Artificial"]
      next_value <- as.numeric(generation_complete_map_code[i + 1, "TotalGenerationValue"])
      next_artificial <- generation_complete_map_code[i + 1, "Artificial"]
      
      # Check whether previous_value or next_value have the value "" and abort the function (then go for Approach B or C)
      if (previous_artificial == "1" || next_artificial == "1") {
        next
      }
      
      print(paste0("Lücke in ", map_code, " an Index ", i, " nach Fall A."))
      generation_complete_map_code[i, "TotalGenerationValue"] <- (previous_value + next_value) / 2
      generation_complete_map_code[i, "Artificial"] <- "A"
    }
  }
  
  # Check for and fill data gaps, where approach B is to be used
  for (i in 1:(nrow(generation_complete_map_code) - 1)) {
    
    # Calculate the number of daily observations for a complete data set depending on the ResolutionCode
    daily_observations <- 24 * (60 / generation_resolution_code[(generation_resolution_code$MapCode == map_code), "ResolutionCode"])
    
    if (generation_complete_map_code[i, "Artificial"] == "1") { # i points to the first missing value of the gap
      # Search for the first available value j
      j <- i + 1 
      while (j <= nrow(generation_complete_map_code) && generation_complete_map_code[j, "Artificial"] == "1") {
        j <- j + 1
      }
      
      l <- j - 1 # l points to the last missing value of the gap
      
      # Check whether the continuous gap is too large (more than 60 days) // only 1 data gap in generation data is larger than a month (Croatia)
      if ((j - i) > (ndays_b * daily_observations)) {
        # Gap too large, proceed according to Approach C (h-Scaling factors)
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B nicht befüllbar."))
        for (k in i:l) {
          generation_complete_map_code[k, "Artificial"] <- "C"
        }
      } else {
        # Fill gap using approach B
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B."))
        for (k in i:l) {
          
          m <- k # assign a temporary variable m for the value search
          # If the gap is larger than two days, make the value search easier by going back to the first day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (m > (i + daily_observations)) {
          #m <- m - daily_observations
          #}
          #}
          
          # Determine the first previously available day
          while (m >= i) { # Make sure that only measuring points before the end of the gap are used
            m <- m - daily_observations
          }
          prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          while (prev_value == "") {
            m <- m - daily_observations
            prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          }
          
          
          n <- k # assign a temporary variable n for the value search
          # If the gap is larger than two days, make the value search easier by leading to the last day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (n < (l - daily_observations)) {
          #n <- n + daily_observations
          #}
          #}
          
          # Determine the fist subsequently available day
          while (n <= l) { # Make sure that only measuring points after the end of the gap are used
            n <- n + daily_observations
          }
          next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          while (next_value == "") {
            n <- n + daily_observations 
            next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          }
          
            # Fill gap using approach B
            generation_complete_map_code[k, "TotalGenerationValue"] <- (as.numeric(prev_value) + as.numeric(next_value)) / 2
            generation_complete_map_code[k, "Artificial"] <- "B"
          #}
        }
        #Sys.sleep(0.5)
      }
      
      # Set the index to the end of the current gap
      i <- j
    }
  }
  
  print(paste(map_code, "is finished."))
  generation_NO_hydro <- generation_NO_hydro[!(generation_NO_hydro$MapCode == map_code),]
  generation_NO_hydro <- rbind(generation_NO_hydro, generation_complete_map_code)
  #Sys.sleep(2.5)
}

#Save for archive
generation_NO_artificial <- generation_NO_hydro[!(generation_NO_hydro$Artificial == "0"),]


########## NORWAY HYDRO - ICELAND VIA MONTHLY SCALING (h)-FACTORS ###########
# See Supplementary Data and Method Section for data on h-factors

month = c("2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10",
          "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08",
          "2017-09", "2017-10", "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04", "2018-05", "2018-06",
          "2018-07", "2018-08", "2018-09", "2018-10", "2018-11", "2018-12", "2019-01", "2019-02", "2019-03", "2019-04",
          "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02",
          "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12",
          "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10",
          "2021-11", "2021-12")

h = c(0.073409714, 0.079509578, 0.081318539, 0.095337817, 0.103179035, 0.102656, 0.103538605, 0.108547876, 0.104465374,
      0.093225503, 0.087490443, 0.086642218, 0.080206936, 0.086088255, 0.100656869, 0.102662517, 0.106781649, 0.10642339,
      0.113971836, 0.111703194, 0.106232434, 0.098842257, 0.088270927, 0.08411077, 0.08043641, 0.075780551, 0.087321954,
      0.110763963, 0.119156941, 0.119344071, 0.127324846, 0.11381561, 0.106526453, 0.093415426, 0.089582111, 0.091641551,
      0.085020252, 0.098883029, 0.116432279, 0.121999395, 0.129510383, 0.122223413, 0.116079903, 0.108089614, 0.103062081,
      0.103445609, 0.089205516, 0.101701494, 0.094116081, 0.090211397, 0.088944844, 0.090205103, 0.092660509, 0.098849799,
      0.10059075, 0.097733639, 0.097143292, 0.085272339, 0.089571607, 0.077174883, 0.068663457, 0.076330153, 0.091571479,
      0.095806563, 0.101922378, 0.101417794, 0.106337585, 0.116072589, 0.111467133, 0.112100849, 0.09391226, 0.083838947)

# Create dataframe with h-factors
h_IS_gen <- data.frame(Month = month, H = h)

generation_IS_hydro <- generation_NO_hydro
generation_IS_hydro$MapCode <- "IS"

# Step 2: Extract Month & Year
generation_IS_hydro$Month <- format(generation_IS_hydro$DateTime, "%Y-%m")

# Step 3: Enter the corresponding h-factor for the measuring points in the data set
generation_IS_hydro <- merge(generation_IS_hydro, h_IS_gen, by = "Month")
generation_IS_hydro <- generation_IS_hydro[,-1]

# Step 4: Scale the NO values using the respective h-factor
for (i in 1:nrow(generation_IS_hydro)) {
  generation_IS_hydro$TotalGenerationValue[i] <- as.numeric(generation_IS_hydro$TotalGenerationValue[i]) * generation_IS_hydro$H[i]
}

# Step 5: Update the artificial variable with H for all scaled measurement points
generation_IS_hydro$Artificial <- "H"

# Step 6: Adjust the data set structure for later merging with the other data
generation_IS_hydro <- generation_IS_hydro[,-6]



##################### ITALY GEOTHERMAL DATASET for IS #######################
#############################################################################

################## ITALY GEOTHERMAL - Data Subsetting #######################

generation_combined_IT <- generation_combined_data_archive
generation_combined_IT <- generation_combined_IT[generation_combined_IT$MapCode == "IT" &
                                                   generation_combined_IT$ProductionType == "Geothermal",]

# Simpler transformation than before as only one ProductionType and ResolutionCode of 60 min
generation_hourly_IT <- generation_combined_IT
generation_hourly_IT <- generation_hourly_IT[,-c(2,4)]
colnames(generation_hourly_IT) <- c("DateTime", "MapCode", "TotalGenerationValue")

# Check for NAs in the data
na_count_hourly_IT_total_generation_value <- sum(is.na(generation_hourly_IT$TotalGenerationValue))

# Display na_count_combined_data_actual_generation_output
print(na_count_hourly_IT_total_generation_value)
##Result: There are no NA values in the data set


###################### ITALY GEOTHERMAL - Data Prep #########################

# Simpler transformation compared to NO above as only one ProductionType and ResolutionCode of 60 min
generation_hourly_IT_aggregated <- generation_hourly_IT

  # Check for NAs in the data
  na_count_hourly_IT_aggregated_actual_generation_output <- sum(is.na(generation_hourly_IT$ActualGenerationOutput))
  na_count_hourly_IT_aggregated_date_time <- sum(is.na(generation_hourly_IT$DateTime))
  
  # Display na_count_hourly_data_actual_generation_output
  print(na_count_hourly_IT_aggregated_actual_generation_output)
  print(na_count_hourly_IT_aggregated_date_time)
  ##Result: There are no NA values in the data set

# Convert DateTime column to POSIXct format
generation_hourly_IT_aggregated$DateTime <- ymd_hms(generation_hourly_IT_aggregated$DateTime)

print("Head of generation_hourly_aggregated:")
head(generation_hourly_IT_aggregated)

  # Check for NAs in the data
  na_count_hourly_IT_aggregated_actual_generation_output <- sum(is.na(generation_hourly_IT$ActualGenerationOutput))
  na_count_hourly_IT_aggregated_date_time <- sum(is.na(generation_hourly_IT$DateTime))
  
  # Display na_count_hourly_data_actual_generation_output
  print(na_count_hourly_IT_aggregated_actual_generation_output)
  print(na_count_hourly_IT_aggregated_date_time)
  ##Result: There are no NA values in the data set


################# ITALY GEOTHERMAL - COMPLETE DF CREATION ###################

generation_combined_IT <- cbind(generation_hourly_IT_aggregated[, "DateTime"], ResolutionCode = 60, generation_hourly_IT_aggregated[, 2:ncol(generation_hourly_IT_aggregated)])
colnames(generation_combined_IT) <- c("DateTime", "ResolutionCode", "MapCode", "TotalGenerationValue")

generation_complete_IT <- data.frame(DateTime = character(),
                                     ResolutionCode = numeric(),
                                     MapCode = character(),
                                     TotalGenerationValue = numeric(),
                                     Artificial = character(),
                                     stringsAsFactors = FALSE)

map_code <- "IT"

start_date <- as.POSIXct("2016-01-01 01:00:00")
end_date <- as.Date("2021-12-31 23:00:00")

resolution_code <- 60

start_date_time <- as.POSIXct(paste(start_date, "00:00:00"))

generation_IT <- generation_combined_IT[generation_combined_IT$MapCode == map_code & generation_combined_IT$DateTime >= start_date_time,]
generation_IT <- generation_IT[!is.na(generation_IT$DateTime),]
generation_IT$Artificial <- "0"

timestamps <- seq(from = start_date, 
                  to = as.POSIXct(paste(end_date, "23:45:00")),
                  by = paste(resolution_code, "min"))

timestamps_missing <- timestamps[!(timestamps %in% generation_IT$DateTime)]

print(length(timestamps_missing))
print(map_code)

# Create missing observations
if (length(timestamps_missing) > 0) { # Missing values
  generation_IT_add <- data.frame(DateTime = timestamps_missing,
                                  ResolutionCode = resolution_code,
                                  MapCode = map_code,
                                  TotalGenerationValue = "")
  generation_IT_add$Artificial <- "1"
  
  # Bind original and additional observations and sort them
  generation_IT_all <- rbind(generation_IT, generation_IT_add)
  generation_IT_all <- arrange(generation_IT_all, DateTime)
  rownames(generation_IT_all) <- NULL
  
  generation_complete_IT <- rbind(generation_complete_IT, generation_IT_all)
  rm(generation_IT_add)
  rm(generation_IT_all)
} else { # No values missing
  generation_complete_IT <- rbind(generation_complete_IT, generation_IT)
}

rm(generation_IT)
rownames(generation_complete_IT) <- NULL

# Count the number of rows with MapCoNO "NO"
number_of_rows_IT <- nrow(generation_complete_IT[generation_complete_IT$MapCode == "IT", ])
print(number_of_rows_IT)

####Remove the non-existent hour due to the start of the DST
# Define the specific dates and time to exclude
dates_to_exclude <- as.POSIXct(c("2016-10-30 02:00:00", "2017-10-29 02:00:00", 
                                 "2018-10-28 02:00:00", "2019-10-27 02:00:00", 
                                 "2020-10-25 02:00:00", "2021-10-31 02:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Filter out the rows with the specified datetimes
generation_complete_IT <- generation_complete_IT[!generation_complete_IT$DateTime %in% dates_to_exclude, ]

# Verify that the specific dates and times have been removed
any(generation_complete_IT$DateTime %in% dates_to_exclude)  # This should return FALSE if all specified instances are removed

# Count the number of rows with MapCoNO "NO"
number_of_rows_IT <- nrow(generation_complete_IT[generation_complete_IT$MapCode == "IT", ])
print(number_of_rows_IT)

# Check for rows where Artificial equals '1' in generation_complete_IT
artificial_rows_count_2 <- generation_complete_IT %>%
  filter(Artificial == "1") %>%
  nrow()

# Print the result
if(artificial_rows_count_2 > 0) {
  print(paste("There are", artificial_rows_count, "rows with Artificial equal to 1."))
} else {
  print("There are no rows with Artificial equal to 1.")
}
##RESULT: There are 83 rows with Artificial equal to 1

################# ITALY GEOTHERMAL - ARTIFICIAL VALUE GENERATION ############

generation_IT_geo <- data.frame(DateTime = character(),
                                ResolutionCode = numeric(),
                                MapCode = character(),
                                TotalGenerationValue = numeric(),
                                Artificial = character(),
                                stringsAsFactors = FALSE)

generation_IT_artificial <- data.frame(DateTime = character(),
                                       ResolutionCode = numeric(),
                                       MapCode = character(),
                                       TotalGenerationValue = numeric(),
                                       Artificial = character(),
                                       stringsAsFactors = FALSE)

#  Limit iterations to MapCode IT (Generic Code Snippet as also used for other countries)

map_codes <- "IT"

for (map_code in map_codes) {
  # Filter the dataframe generation_complete_IT for IT MapCode
  generation_complete_map_code <- generation_complete_IT
  
  # Check for and fill data gaps, where approach A is to be used
  for (i in 1:(nrow(generation_complete_map_code))) {
    if (generation_complete_map_code[i, "Artificial"] == "1") {
      
      # If data is missing directly at the first measuring point, but the following measuring points are available, use the following measuring point
      if (i == 1 && generation_complete_map_code[i+1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i+1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      # If data is missing at the last measuring point, but the measuring points before it are available, use the previous measuring point
      if (i == nrow(generation_complete_map_code) && generation_complete_map_code[i-1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i-1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      previous_value <- as.numeric(generation_complete_map_code[i - 1, "TotalGenerationValue"])
      previous_artificial <- generation_complete_map_code[i - 1, "Artificial"]
      next_value <- as.numeric(generation_complete_map_code[i + 1, "TotalGenerationValue"])
      next_artificial <- generation_complete_map_code[i + 1, "Artificial"]
      
      # Check whether previous_value or next_value have the value "1" and abort the function (then go for Approach B or C)
      if (previous_artificial == "1" || next_artificial == "1") {
        next
      }
      
      print(paste0("Lücke in ", map_code, " an Index ", i, " nach Fall A."))
      generation_complete_map_code[i, "TotalGenerationValue"] <- (previous_value + next_value) / 2
      generation_complete_map_code[i, "Artificial"] <- "A"
    }
  }
  
  # Check for and fill data gaps, where approach B is to be used
  for (i in 1:(nrow(generation_complete_map_code) - 1)) {
    
    # Calculate the number of daily observations for a complete data set depending on the ResolutionCode
    daily_observations <- 24 * (60 / generation_resolution_code[(generation_resolution_code$MapCode == map_code), "ResolutionCode"])
    
    if (generation_complete_map_code[i, "Artificial"] == "1") { # i zeigt auf den ersten fehlenden Wert der Lücke
      # Suche nach dem ersten wieder verfügbaren Wert j
      j <- i + 1 
      while (j <= nrow(generation_complete_map_code) && generation_complete_map_code[j, "Artificial"] == "1") {
        j <- j + 1
      }
      
      l <- j - 1 # l points to the last missing value of the gap
      
      # Check whether the continuous gap is too large (more than 60 days) // only 1 data gap in generation data is larger than a month (Croatia)
      if ((j - i) > (ndays_b * daily_observations)) {
        # Gap too large, proceed according to Approach C (h-Scaling factors)
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall C nicht befüllbar."))
        for (k in i:l) {
          generation_complete_map_code[k, "Artificial"] <- "C"
        }
      } else {
        # Fill gap using approach B
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B."))
        for (k in i:l) {
          
          m <- k # assign a temporary variable m for the value search
          # If the gap is larger than two days, make the value search easier by going back to the first day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (m > (i + daily_observations)) {
          #m <- m - daily_observations
          #}
          #}
          
          # Determine the first previously available day
          while (m >= i) { # Make sure that only measuring points before the end of the gap are used
            m <- m - daily_observations
          }
          prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          while (prev_value == "") {
            m <- m - daily_observations
            prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          }
          
          
          n <- k # assign a temporary variable n for the value search
          # If the gap is larger than two days, make the value search easier by leading to the last day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (n < (l - daily_observations)) {
          #n <- n + daily_observations
          #}
          #}
          
          # Determine the fist subsequently available day
          while (n <= l) { # Stelle sicher, dass nur Messpunkte nach dem Ende der Lücke genutzt werden
            n <- n + daily_observations
          }
          next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          while (next_value == "") {
            n <- n + daily_observations 
            next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          }
          
          # Fill gap using approach B
          generation_complete_map_code[k, "TotalGenerationValue"] <- (as.numeric(prev_value) + as.numeric(next_value)) / 2
          generation_complete_map_code[k, "Artificial"] <- "B"
        }
      }
      
      # Set the index to the end of the current gap
      i <- j
    }
  }
  
  print(paste(map_code, "is finished."))
  generation_IT_geo <- generation_IT_geo[!(generation_IT_geo$MapCode == map_code),]
  generation_IT_geo <- rbind(generation_IT_geo, generation_complete_map_code)
}

#Save for archive
generation_IT_artificial <- generation_IT_geo[!(generation_IT_geo$Artificial == "0"),]


################## ITALY GEOTHERMAL - ICELAND VIA h-FACTORS #################
# See Supplementary Data and Method Section for data on h-factors

month = c("2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10",
          "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08",
          "2017-09", "2017-10", "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04", "2018-05", "2018-06",
          "2018-07", "2018-08", "2018-09", "2018-10", "2018-11", "2018-12", "2019-01", "2019-02", "2019-03", "2019-04",
          "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02",
          "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12",
          "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10",
          "2021-11", "2021-12")

h = c(0.877460357, 0.898334325, 0.90335173, 0.913165624, 0.859062168, 0.755426424, 0.869366158, 0.785813731, 0.848435767,
      0.882686517, 0.905967706, 0.866672444, 0.834597852, 0.836429461, 0.853288057, 0.856107332, 0.862035612, 0.793103086,
      0.796803077, 0.778035105, 0.813085057, 0.909843176, 0.916899791, 0.937119242, 0.9495176, 0.980475901, 0.992053826,
      0.990841741, 0.971027714, 0.981760398, 0.984106697, 0.992726201, 0.98959273, 1.037180997, 1.061050574, 1.047467762,
      1.053397225, 1.074225635, 1.014601754, 0.993374822, 0.979746545, 0.957787632, 0.988945449, 0.960716405, 0.963131981,
      1.008490752, 1.086234617, 1.123367839, 1.093306515, 1.084049004, 1.021927403, 1.072799044, 1.076929066, 1.08839567,
      0.982063839, 0.983476546, 0.964400329, 0.980425604, 1.035251585, 1.046085445, 1.076791815, 1.060007301, 1.043182727,
      0.995991674, 0.98109125, 0.96796912, 1.055029749, 1.019641087, 0.9989888, 0.993887578, 1.05089877, 1.083430737)

# Step 1: Create dataframe with h-factors
h_IS_geo <- data.frame(Month = month, H = h)

generation_IS_geo <- generation_IT_geo
generation_IS_geo$MapCode <- "IS"

# Step 2: Step 2: Extract Month & Year
generation_IS_geo$Month <- format(generation_IS_geo$DateTime, "%Y-%m")

# Step 3: Step 3: Enter the corresponding h-factor for the measuring points in the data set
generation_IS_geo <- merge(generation_IS_geo, h_IS_geo, by = "Month")
generation_IS_geo <- generation_IS_geo[,-1]

# Step 4: Scale the IT values using the respective h-factor
for (i in 1:nrow(generation_IS_geo)) {
  generation_IS_geo$TotalGenerationValue[i] <- as.numeric(generation_IS_geo$TotalGenerationValue[i]) * generation_IS_geo$H[i]
}

# Step 5: Update the artificial variable with H for all scaled measurement points
generation_IS_geo$Artificial <- "H"

# Step 6: Adjust the data set structure for later merging with the other data
generation_IS_geo <- generation_IS_geo[,-6]


############################### BIND DATASETS ##############################
############################################################################

# Add up scaled hydro and scaled geothermal components to obtain IS generation values
generation_IS <- generation_IS_hydro
generation_IS$TotalGenerationValue <- as.numeric(generation_IS_hydro$TotalGenerationValue) +
  as.numeric(generation_IS_geo$TotalGenerationValue)



#############################################################################################
###################### OTHER DATA GAPS - ARTIFICIAL VALUE GENERATION ########################
#############################################################################################

#### WHAT THIS CODE SECTION DOES #### 
# Generates reasonable values for renewable generation for the timestamps that had previously been missing in the entso-e dataset
# For minor data gaps a two-pronged apporach (A & B) is used; for larger data gaps scaling (h) factors are used (see method section for details)
# Note: This section is a generic code section; artifical value creation in Iceland section follows same logic

generation_filled <- data.frame(DateTime = character(),
                            ResolutionCode = numeric(),
                            MapCode = character(),
                            TotalGenerationValue = numeric(),
                            Artificial = character(),
                            stringsAsFactors = FALSE)

generation_filled_artificial <- data.frame(DateTime = character(),
                          ResolutionCode = numeric(),
                          MapCode = character(),
                          TotalGenerationValue = numeric(),
                          Artificial = character(),
                          stringsAsFactors = FALSE)


############################## APPROACH A & B ##############################
############################################################################

map_codes <- unique(generation_complete$MapCode)
for (map_code in map_codes) {
  # Filter the dataframe for current map code
  generation_complete_map_code <- generation_complete[generation_complete$MapCode == map_code, ]
  
  generation_complete_map_code <- arrange(generation_complete_map_code, DateTime)
  
  # Check for and fill data gaps, where approach A is to be used
  for (i in 1:(nrow(generation_complete_map_code))) {
    if (generation_complete_map_code[i, "Artificial"] == "1") {
      
      # If data is missing directly at the first measuring point, but the following measuring points are available, use the following measuring point
      if (i == 1 && generation_complete_map_code[i+1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i+1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      # If data is missing at the last measuring point, but the measuring points before it are available, use the previous measuring point
      if (i == nrow(generation_complete_map_code) && generation_complete_map_code[i-1, "Artificial"] != "1") {
        generation_complete_map_code[i, "TotalGenerationValue"] <- generation_complete_map_code[i-1, "TotalGenerationValue"]
        generation_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      previous_value <- as.numeric(generation_complete_map_code[i - 1, "TotalGenerationValue"])
      previous_artificial <- generation_complete_map_code[i - 1, "Artificial"]
      next_value <- as.numeric(generation_complete_map_code[i + 1, "TotalGenerationValue"])
      next_artificial <- generation_complete_map_code[i + 1, "Artificial"]
      
      # Check whether previous_value or next_value have the value "" and abort the function (then go for Approach B or C)
      if (previous_artificial == "1" || next_artificial == "1") {
        next
      }
      
      print(paste0("Lücke in ", map_code, " an Index ", i, " nach Fall A."))
      generation_complete_map_code[i, "TotalGenerationValue"] <- (previous_value + next_value) / 2
      generation_complete_map_code[i, "Artificial"] <- "A"
    }
  }
  
  # Check for and fill data gaps, where approach B is to be used
  for (i in 1:(nrow(generation_complete_map_code) - 1)) {
    
    # Calculate the number of daily observations for a complete data set depending on the ResolutionCode
    daily_observations <- 24 * (60 / generation_resolution_code[(generation_resolution_code$MapCode == map_code), "ResolutionCode"])
    
    if (generation_complete_map_code[i, "Artificial"] == "1") { # i points to the first missing value of the gap
      # Search for the first available value j
      j <- i + 1 
      while (j <= nrow(generation_complete_map_code) && generation_complete_map_code[j, "Artificial"] == "1") {
        j <- j + 1
      }
      
      l <- j - 1 # l points to the last missing value of the gap
      
      # Check whether the continuous gap is too large (more than 60 days) // only 1 data gap in generation data is larger than a month (Croatia)
      if ((j - i) > (ndays_b * daily_observations)) {
        # Gap too large, proceed according to case C (h-Scling factors)
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B nicht befüllbar."))
        for (k in i:l) {
          generation_complete_map_code[k, "Artificial"] <- "C"
        }
      } else {
        # Fill gap using approach B
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B."))
        for (k in i:l) {
          
          m <- k # assign a temporary variable m for the value search
          # If the gap is larger than two days, make the value search easier by going back to the first day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (m > (i + daily_observations)) {
          #m <- m - daily_observations
          #}
          #}
          
          # Determine the first previously available day
          while (m >= i) { # Make sure that only measuring points before the end of the gap are used
            m <- m - daily_observations
          }
          prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          while (prev_value == "" || is.na(prev_value)) {
            m <- m - daily_observations
            if (m < 1) { break } # Ensure m does not go below 1
            prev_value <- generation_complete_map_code[m, "TotalGenerationValue"]
          }
          
          n <- k # assign a temporary variable n for the value search
          # If the gap is larger than two days, make the value search easier by leading to the last day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (n < (l - daily_observations)) {
          #n <- n + daily_observations
          #}
          #}
          
          # Determine the fist subsequently available day
          while (n <= l && n <= nrow(generation_complete_map_code)) {
            n <- n + daily_observations
          }
          next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          while (next_value == "" || is.na(next_value)) {
            n <- n + daily_observations
            if (n > nrow(generation_complete_map_code)) { break } # Ensure n does not exceed row count
            next_value <- generation_complete_map_code[n, "TotalGenerationValue"]
          }
          
          # Fill gap using approach B
          generation_complete_map_code[k, "TotalGenerationValue"] <- (as.numeric(prev_value) + as.numeric(next_value)) / 2
          generation_complete_map_code[k, "Artificial"] <- "B"
        }
      }
      
      # Set the index to the end of the current gap
      i <- j
    }
  }
  
  print(paste(map_code, "is finished."))
  generation_filled <- generation_filled[!(generation_filled$MapCode == map_code),]
  generation_filled <- rbind(generation_filled, generation_complete_map_code)
}

#Add artificially generated values for IS (via NO hydro and IT geothermal)
generation_filled <- rbind(generation_filled, generation_IS)

#Save for archive
generation_filled_archive <- generation_filled

## Result: The generation_filled data set contains the replaced values for TotalGenerationValue and the corresponding values in the Artificial column according to the instructions.


################# DATA IMPUTATION VIA H-FACTORS FOR CROATIA ################
############################################################################
# See Supplementary Data and Method Section for data on h-factors

# Creating the data frame with the h-factors for 2016
month = c("2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10",
          "2016-11", "2016-12")

h = c(1.078077115, 1.174729885, 1.72683961, 1.051564971, 0.857226813, 0.924082749, 1.041905009, 1.218175108, 1.123012988,
      1.47841875, 0.824869944, 0.588018129)

h_HR_2016 <- data.frame(Month = month, H = h)

# Creating the data frame with the h-factors for 2017 & 2018
month = c("2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08",
          "2017-09", "2017-10", "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04", "2018-05", "2018-06",
          "2018-07", "2018-08", "2018-09", "2018-10", "2018-11", "2018-12")

h = c(0.9728216, 0.827808602, 1.264299293, 0.841954486, 0.519623687, 0.542725886,
      0.941180934, 1.231557816, 1.580001327, 1.108041686, 0.56871509, 0.881461887, 1.470140817, 1.266980301, 2.268607793,
      1.776679683, 0.985160085, 0.940328818, 1.216911222, 1.33139395, 1.224190894, 1.331392149, 0.684710539, 0.667959688)

h_HR_rest <- data.frame(Month = month, H = h)

# Step 1: Create the data set to be filled as well as the reference data set and timestamp sets
generation_HR <- generation_filled[generation_filled$Artificial == "C" & generation_filled$MapCode == "HR", ]

timestamps_HR_2019 <- generation_filled$DateTime[generation_filled$MapCode == "HR" & year(generation_filled$DateTime) == 2019]

timestamps_HR_2019_reduced <- generation_filled$DateTime[generation_filled$MapCode == "HR" & year(generation_filled$DateTime) == 2019 &
                                                  generation_filled$DateTime <= ymd_hms("2019-12-31 22:00:00")]

timestamps_HR_rest <- generation_HR$DateTime[year(generation_HR$DateTime) == 2017 |
                                               (year(generation_HR$DateTime) == 2018 &
                                                  generation_HR$DateTime <= ymd_hms("2018-12-31 22:00:00"))]

timestamps_HR_2016 <- generation_HR$DateTime[year(generation_HR$DateTime) == 2016]

generation_HR_ref <- generation_filled[(generation_filled$DateTime %in% timestamps_HR_2019) & generation_filled$MapCode == "HR", ]

# Split dataframes in 2016 and 2017-2018 due to the leap year

generation_HR_2016 <- generation_HR[(generation_HR$DateTime %in% timestamps_HR_2016), ]
generation_HR_2016 <- generation_HR_2016[!(is.na(generation_HR_2016$DateTime)),]

generation_HR_rest <- generation_HR[(generation_HR$DateTime %in% timestamps_HR_rest), ]
generation_HR_rest <- generation_HR_rest[!(is.na(generation_HR_rest$DateTime)),]
  
# Correction of the February 29 error for 2016 (duplication of February 28)
rownames(generation_HR_ref) <- NULL
generation_HR_feb <- generation_HR_ref[1393:1416, ]
generation_HR_ref_2016 <- rbind(generation_HR_ref[1:1416, ], generation_HR_feb, generation_HR_ref[1417:nrow(generation_HR_ref), ])
rm(generation_HR_feb)

# Correct the summer time error by duplicating the respective hour
generation_HR_ref_2016 <- rbind(generation_HR_ref_2016[1:7274, ], generation_HR_ref_2016[7274, ], generation_HR_ref_2016[7275:nrow(generation_HR_ref_2016), ])

# Duplicate ref dataframe as used for 2017 & 2018 - Limited duplication as some data points already exist on 31.12.2018
generation_HR_ref <- rbind(generation_HR_ref, generation_HR_ref[generation_HR_ref$DateTime %in% timestamps_HR_2019_reduced,])

# Correct the summer time error by duplicating the respective hour
generation_HR_ref <- rbind(generation_HR_ref[1:7226, ], generation_HR_ref[7226, ], generation_HR_ref[7227:nrow(generation_HR_ref), ])

# Step 2: Extract month & year
generation_HR_rest$Month <- format(generation_HR_rest$DateTime, "%Y-%m")
generation_HR_2016$Month <- format(generation_HR_2016$DateTime, "%Y-%m")

# Step 3: Enter the corresponding h-factor for the measuring points in the data set
generation_HR_rest <- merge(generation_HR_rest, h_HR_rest, by = "Month")
generation_HR_rest <- generation_HR_rest[,-1]
generation_HR_2016 <- merge(generation_HR_2016, h_HR_2016, by = "Month")
generation_HR_2016 <- generation_HR_2016[,-1]

# Step 4: Scale the measured values using the respective h-factor
for (i in 1:nrow(generation_HR_rest)) {
  generation_HR_rest$TotalGenerationValue[i] <- as.numeric(generation_HR_ref$TotalGenerationValue[i]) * generation_HR_rest$H[i]
}

for (i in 1:nrow(generation_HR_2016)) {
  generation_HR_2016$TotalGenerationValue[i] <- as.numeric(generation_HR_ref_2016$TotalGenerationValue[i]) * generation_HR_2016$H[i]
}

# Merge the two data sets of the different time periods
generation_HR <- rbind(generation_HR_2016, generation_HR_rest)

# Step 5:  Update the artificial variable with "H" for all scaled measurement points
generation_HR$Artificial <- "H"

# Step 6: Update the main data set with the new data
generation_filled[generation_filled$Artificial == "C" & generation_filled$MapCode == "HR", c("TotalGenerationValue", "Artificial")] <- generation_HR[c("TotalGenerationValue", "Artificial")]

# Save filled dataset for archive
generation_filled_archive <- generation_filled
generation_filled_artificial <- generation_filled[!(generation_filled$Artificial == "0"),]


###################################################################################################################
###################################################################################################################
################################################ DATA AGGREGATION #################################################
###################################################################################################################
###################################################################################################################

###################### AGGREGATION - HOURLY & AIB ##########################
############################################################################

# Drop Artificial column & adjust data types
generation_filled <- generation_filled[,-5]
generation_filled$TotalGenerationValue <- as.numeric(generation_filled$TotalGenerationValue)

# Create a new column for the hour component of DateTime
generation_filled$Hour <- format(generation_filled$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate (sum up) over AIB countries
generation_hourly_AIB <- aggregate(x = generation_filled$TotalGenerationValue,
                             by = list(Hour = generation_filled$Hour),
                             FUN = sum)

colnames(generation_hourly_AIB) <- c("DateTime", "TotalGenerationValue")


############## AGGREGATION - DAILY, WEEKLY, MONTHLY, YEARLY ################
############################################################################

# DAILY DATA: Computed from hourly values
# Aggregate data on daily level
generation_daily_AIB <- aggregate(x = generation_hourly_AIB$TotalGenerationValue,
                            by = list(Date = as.Date(generation_hourly_AIB$DateTime)),
                            FUN = sum)
colnames(generation_daily_AIB) <- c("Date", "TotalGenerationValue")

# WEEKLY DATA: Computed from daily values
generation_weekly_AIB <- aggregate(x = generation_daily_AIB$TotalGenerationValue,
                             by = list(Date = as.Date(cut(generation_daily_AIB$Date, "week"))), 
                             FUN = sum)
colnames(generation_weekly_AIB) <- c("Date", "TotalGenerationValue")

# MONTHLY DATA: Computed from daily values
generation_monthly_AIB <- aggregate(x = generation_daily_AIB$TotalGenerationValue,
                              by = list(Date = as.Date(cut(generation_daily_AIB$Date, "month"))), 
                              FUN = sum)

colnames(generation_monthly_AIB) <- c("Date", "TotalGenerationValue")

# YEARLY DATA: Computed from daily values
generation_yearly_AIB <- aggregate(x = generation_daily_AIB$TotalGenerationValue,
                             by = list(Date = as.Date(cut(generation_daily_AIB$Date, "year"))), 
                             FUN = sum)

colnames(generation_yearly_AIB) <- c("Date", "TotalGenerationValue")

# Additional analysis: Production types by country and vice versa
gen_prodtypes <- unique(generation_combined_data_archive[, c("MapCode", "ProductionType")])
gen_prodtypes <- arrange(gen_prodtypes, ProductionType)
gen_prodtypes_CTY <- arrange(gen_prodtypes, MapCode)


###################################################################################################################
###################################################################################################################
############################################## WORKSAPCE CLEANING #################################################
###################################################################################################################
###################################################################################################################

# List of all variables currently in the environment
all_vars <- ls()

# Specify the variables to keep
vars_to_keep <- c("generation_hourly_AIB", "generation_daily_AIB", "generation_weekly_AIB",
                  "generation_monthly_AIB", "generation_yearly_AIB", "generation_filled_artificial",
                  "gen_prodtypes", "gen_prodtypes_CTY", "generation_raw_data", "ndays_b")

# Determine which variables to remove
vars_to_remove <- setdiff(all_vars, vars_to_keep)

# Remove the variables that are not needed
rm(list = vars_to_remove)
