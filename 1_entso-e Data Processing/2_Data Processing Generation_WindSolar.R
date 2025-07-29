## Builds on "Data Processing Generation_vfinal.R"; specifically by loading generation_raw_data TODO: Clear all variables besides generation_raw_data

library(dplyr)
library(lubridate)

y1 <- 2016
y2 <- 2021
ndays_b <- 60 # Originally set to 4 days; only CY shows gaps larger than 4 days - those were filled via approach B - see method section of paper.

#Set working directory to folder containing generation_raw_data
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/1_entso-e Data Processing/1_Input Data/Generation Dataset")

###################################################################################################################
###################################################################################################################
##################################################### SOLAR #######################################################
###################################################################################################################
###################################################################################################################

#############################################################################################
###################################### DATA INTAKE ##########################################
#############################################################################################

generation_combined_data <- generation_raw_data[,c("DateTime", "ResolutionCode", "MapCode", "ProductionType", "ActualGenerationOutput")]

# Filter countries according to their relevance for analysis
generation_combined_data <- generation_combined_data[!(generation_combined_data$MapCode %in% c("AL","BA","BG","GB","GE","GR","HU","LT","ME","MD","MK","PL","RO","UA","XK")),]

# Save Resolution Code as numeric
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT60M"] <- 60
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT30M"] <- 30
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT15M"] <- 15
generation_combined_data$ResolutionCode <- as.numeric(generation_combined_data$ResolutionCode)

# Filter for green energy generation type Solar
generation_combined_data <- generation_combined_data[generation_combined_data$ProductionType
                                                     == "Solar",]

# save file in separate directory
print("Head of generation_combined_data:")
head(generation_combined_data)

# Save archive copy
generation_combined_data_solar <- generation_combined_data

#############################################################################################
############# INITIAL DATA SET EXPLORATION (e.g. CHECKING FOR MISSING VALUES) ###############
#############################################################################################

# Count NA values in the TotalGenerationValue column of generation_combined_data 
na_count_combined_data_solar_actual_generation_output <- sum(is.na(generation_combined_data_solar$ActualGenerationOutput))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_solar_actual_generation_output)
##Result: There are no NA values in the data set

#Check NAs in DateTime

# Count NA values in the DateTime column of generation_combined_data_solar 
na_count_combined_data_solar_date_time <- sum(is.na(generation_combined_data_solar$DateTime))

# Display na_count_combined_data_solar_date_time
print(na_count_combined_data_solar_date_time)
##Result: There are no NA values for DateTime in the data set

#############################################################################################
############ INITIAL VALUE AGGREGATION TO HARMONIZE DATA TO HOURLY TIMESTAMPs ###############
#############################################################################################
##Note: By now, generation_combined_data only includes solar values

# Convert DateTime column to POSIXct format
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

# Re-check that there are no NAs in the TotalGenerationValue column of generation_hourly_data 
na_count_combined_data_solar_total_generation_value <- sum(is.na(generation_hourly_data$TotalGenerationValue))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_solar_total_generation_value)

# Aggregate values over the renewable production types (Solar) per hourly timestamp & MapCode \\ Generic Code Segment; here: Equal to deleting column "ProductionType"
generation_hourly_reduced <- generation_hourly_data[,c("DateTime", "MapCode", "TotalGenerationValue")]
generation_hourly_aggregated <- aggregate(x = generation_hourly_reduced$TotalGenerationValue,
                                          by = list(Hour = generation_hourly_reduced$DateTime,
                                                    MapCode = generation_hourly_reduced$MapCode),
                                          FUN = sum)

# Re-check that there are no NAs in the TotalGenerationValue column of generation_hourly_aggregated 
na_count_combined_aggregated_solar_total_generation_value <- sum(is.na(generation_hourly_aggregated$TotalGenerationValue))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_aggregated_solar_total_generation_value)

# Rename the columns and display result
colnames(generation_hourly_aggregated) <- c("DateTime", "MapCode", "TotalGenerationValue")
print("Head of generation_hourly_aggregated:")
head(generation_hourly_aggregated)

#############################################################################################
##################################### DATA IMPUTATION #######################################
#############################################################################################

############################ COMPLETE DF CREATION ###########################
#############################################################################

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
                                  DateTime >= start_date)
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

# ####OLD CODE
# #Remove duplicates per MapCode (resulting from ends of DST)
#   # Ensure all DateTime entries are in POSIXct format (without specifying timezone)
#   generation_complete$DateTime <- as.POSIXct(generation_complete$DateTime, format = "%Y-%m-%d %H:%M:%S")
#   
#   # Optionally format to string if required (not needed as keeping as POSIXct)
#   # formatted_DateTime <- format(generation_complete$DateTime, "%Y-%m-%d %H:%M:%S")
#   
#   # Re-apply POSIXct without timezone to standardize format
#   generation_complete$DateTime <- as.POSIXct(format(generation_complete$DateTime, "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")
#   
#   
#   # Split the DataFrame by MapCode
#   split_data <- split(generation_complete, generation_complete$MapCode)
#   
#   # Remove duplicates within each group
#   cleaned_data <- lapply(split_data, function(df) {
#     # Removing duplicates based on DateTime
#     df <- df[!duplicated(df$DateTime), ]
#     return(df)
#   })
#   
#   # Reassemble the DataFrame
#   generation_complete_cleaned <- do.call(rbind, cleaned_data)
#   
#   # Reset row names after reassembling
#   rownames(generation_complete_cleaned) <- NULL
# 
# generation_complete <- generation_complete_cleaned

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



######################### ARTIFICIAL VALUE GENERATION #######################
#############################################################################

generation_filled <- data.frame(DateTime = character(),
                                ResolutionCode = numeric(),
                                MapCode = character(),
                                TotalGenerationValue = numeric(),
                                Artificial = character(),
                                stringsAsFactors = FALSE)

generation_filled_artificial_solar <- data.frame(DateTime = character(),
                                           ResolutionCode = numeric(),
                                           MapCode = character(),
                                           TotalGenerationValue = numeric(),
                                           Artificial = character(),
                                           stringsAsFactors = FALSE)

####################### APPROACH A & B #######################
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

# Step 5: Update the artificial variable with "H" for all scaled measurement points
generation_HR$Artificial <- "H"

# Step 6: Update the main data set with the new data
generation_filled[generation_filled$Artificial == "C" & generation_filled$MapCode == "HR", c("TotalGenerationValue", "Artificial")] <- generation_HR[c("TotalGenerationValue", "Artificial")]

#Save artificially filled datapoints for solar for archive
generation_filled_artificial_solar <- generation_filled[!(generation_filled$Artificial == "0"),]


#############################################################################################
##################################### DATA AGGREGATION ######################################
#############################################################################################

###################### AGGREGATION - HOURLY & AIB ##########################
############################################################################

# Drop Artificial column & adjust data types
generation_filled <- generation_filled[,-5]
generation_filled$TotalGenerationValue <- as.numeric(generation_filled$TotalGenerationValue)

# Create a new column for the hour component of DateTime
generation_filled$Hour <- format(generation_filled$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate (sum up) over AIB countries
generation_hourly_AIB_solar <- aggregate(x = generation_filled$TotalGenerationValue,
                                         by = list(Hour = generation_filled$Hour),
                                         FUN = sum)

colnames(generation_hourly_AIB_solar) <- c("DateTime", "TotalGenerationValue")


############## AGGREGATION - DAILY, WEEKLY, MONTHLY, YEARLY ################
############################################################################

# DAILY DATA: Computed from hourly values
# Aggregate data on daily level
generation_daily_AIB_solar <- aggregate(x = generation_hourly_AIB_solar$TotalGenerationValue,
                                        by = list(Date = as.Date(generation_hourly_AIB_solar$DateTime)),
                                        FUN = sum)
colnames(generation_daily_AIB_solar) <- c("Date", "TotalGenerationValue")

# WEEKLY DATA: Computed from daily values
generation_weekly_AIB_solar <- aggregate(x = generation_daily_AIB_solar$TotalGenerationValue,
                                         by = list(Date = as.Date(cut(generation_daily_AIB_solar$Date, "week"))), 
                                         FUN = sum)
colnames(generation_weekly_AIB_solar) <- c("Date", "TotalGenerationValue")

# MONTHLY DATA: Computed from daily values
generation_monthly_AIB_solar <- aggregate(x = generation_daily_AIB_solar$TotalGenerationValue,
                                          by = list(Date = as.Date(cut(generation_daily_AIB_solar$Date, "month"))), 
                                          FUN = sum)

colnames(generation_monthly_AIB_solar) <- c("Date", "TotalGenerationValue")

# YEARLY DATA: Computed from daily values
generation_yearly_AIB_solar <- aggregate(x = generation_daily_AIB_solar$TotalGenerationValue,
                                         by = list(Date = as.Date(cut(generation_daily_AIB_solar$Date, "year"))), 
                                         FUN = sum)

colnames(generation_yearly_AIB_solar) <- c("Date", "TotalGenerationValue")


################################################################################################
################################## WORKSAPCE CLEANING ##########################################
################################################################################################


# List of all variables currently in the environment
all_vars <- ls()

# Specify the variables to keep
vars_to_keep <- c("generation_hourly_AIB", "generation_daily_AIB", "generation_weekly_AIB","generation_monthly_AIB", "generation_yearly_AIB",
                  "generation_hourly_AIB_solar", "generation_daily_AIB_solar", "generation_weekly_AIB_solar", "generation_monthly_AIB_solar", "generation_yearly_AIB_solar", 
                  "generation_filled_artificial","generation_filled_artificial_solar", "gen_prodtypes", "gen_prodtypes_CTY", "generation_raw_data", "ndays_b")

# Determine which variables to remove
vars_to_remove <- setdiff(all_vars, vars_to_keep)

# Remove the variables that are not needed
rm(list = vars_to_remove)


###################################################################################################################
###################################################################################################################
##################################################### WIND ########################################################
###################################################################################################################
###################################################################################################################

#############################################################################################
###################################### DATA INTAKE ##########################################
#############################################################################################

generation_combined_data <- generation_raw_data[,c("DateTime", "ResolutionCode", "MapCode", "ProductionType", "ActualGenerationOutput")]

# Filter countries according to their relevance for analysis
generation_combined_data <- generation_combined_data[!(generation_combined_data$MapCode %in% c("AL","BA","BG","GB","GE","GR","HU","LT","ME","MD","MK","PL","RO","UA","XK")),]

# Save Resolution Code as numeric
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT60M"] <- 60
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT30M"] <- 30
generation_combined_data$ResolutionCode[generation_combined_data$ResolutionCode == "PT15M"] <- 15
generation_combined_data$ResolutionCode <- as.numeric(generation_combined_data$ResolutionCode)

# Filter for green energy generation types Wind Offshore and Wind Onshore
generation_combined_data <- generation_combined_data[generation_combined_data$ProductionType
                                                     %in% c("Wind Offshore","Wind Onshore"),]

# save file in separate directory
print("Head of generation_combined_data:")
head(generation_combined_data)

# Save archive copy
generation_combined_data_wind <- generation_combined_data


#############################################################################################
############# INITIAL DATA SET EXPLORATION (e.g. CHECKING FOR MISSING VALUES) ###############
#############################################################################################

# Count NA values in the TotalGenerationValue column of generation_combined_data 
na_count_combined_data_wind_actual_generation_output <- sum(is.na(generation_combined_data_wind$ActualGenerationOutput))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_wind_actual_generation_output)
##Result: There are NA values in the data set

# Set NA values in data set to zero, as no values have been reported for these hours 
#(whilst for other hours values have been reported)

# Set the NAs to Zero (generation_combined_data here only contains wind values)
generation_combined_data$ActualGenerationOutput[is.na(generation_combined_data$ActualGenerationOutput)] <- 0

#Check
# Count NA values in the TotalGenerationValue column of generation_combined_data 
na_count_combined_data_wind_actual_generation_output <- sum(is.na(generation_combined_data$ActualGenerationOutput))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_wind_actual_generation_output)
##Result: There are no more NA values in the data set

# Count NA values in the DateTime column of generation_combined_data_wind 
na_count_combined_data_wind_date_time <- sum(is.na(generation_combined_data_wind$DateTime))

# Display na_count_combined_data_wind_date_time
print(na_count_combined_data_wind_date_time)
##Result: There are no NA values for DateTime in the data set

#############################################################################################
################################# INITIAL VALUE AGGREGATION #################################
#############################################################################################

# Convert DateTime column to POSIXct format
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

# Re-check that there are no NAs in the TotalGenerationValue column of generation_hourly_data 
na_count_combined_data_wind_total_generation_value <- sum(is.na(generation_hourly_data$TotalGenerationValue))

# Display na_count_combined_data_actual_generation_output
print(na_count_combined_data_wind_total_generation_value)

#  Aggregate values over the renewable production types (Wind On- & Offshore) per hourly timestamp & MapCode
generation_hourly_reduced <- generation_hourly_data[,c("DateTime", "MapCode", "TotalGenerationValue")]
generation_hourly_aggregated <- aggregate(x = generation_hourly_reduced$TotalGenerationValue,
                                          by = list(Hour = generation_hourly_reduced$DateTime,
                                                    MapCode = generation_hourly_reduced$MapCode),
                                          FUN = sum)

# Rename the columns and display result
colnames(generation_hourly_aggregated) <- c("DateTime", "MapCode", "TotalGenerationValue")
print("Head of generation_hourly_aggregated:")
head(generation_hourly_aggregated)


#############################################################################################
##################################### DATA IMPUTATION #######################################
#############################################################################################

############################ COMPLETE DF CREATION ###########################
#############################################################################

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

# ####OLD CODE
# #Remove duplicates per MapCode (resulting from ends of DST)
#   # Ensure all DateTime entries are in POSIXct format (without specifying timezone)
#   generation_complete$DateTime <- as.POSIXct(generation_complete$DateTime, format = "%Y-%m-%d %H:%M:%S")
#   
#   # Optionally format to string if required (not needed as keeping as POSIXct)
#   # formatted_DateTime <- format(generation_complete$DateTime, "%Y-%m-%d %H:%M:%S")
#   
#   # Re-apply POSIXct without timezone to standardize format
#   generation_complete$DateTime <- as.POSIXct(format(generation_complete$DateTime, "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")
#   
#   
#   # Split the DataFrame by MapCode
#   split_data <- split(generation_complete, generation_complete$MapCode)
#   
#   # Remove duplicates within each group
#   cleaned_data <- lapply(split_data, function(df) {
#     # Removing duplicates based on DateTime
#     df <- df[!duplicated(df$DateTime), ]
#     return(df)
#   })
#   
#   # Reassemble the DataFrame
#   generation_complete_cleaned <- do.call(rbind, cleaned_data)
#   
#   # Reset row names after reassembling
#   rownames(generation_complete_cleaned) <- NULL
# 
# generation_complete <- generation_complete_cleaned

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

########################## ARTIFICIAL VALUE CREATION #######################
############################################################################

generation_filled <- data.frame(DateTime = character(),
                            ResolutionCode = numeric(),
                            MapCode = character(),
                            TotalGenerationValue = numeric(),
                            Artificial = character(),
                            stringsAsFactors = FALSE)

generation_filled_artificial_wind <- data.frame(DateTime = character(),
                          ResolutionCode = numeric(),
                          MapCode = character(),
                          TotalGenerationValue = numeric(),
                          Artificial = character(),
                          stringsAsFactors = FALSE)

####################### APPROACH A & B #######################
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

# Step 5: Update the artificial variable with "H" for all scaled measurement points
generation_HR$Artificial <- "H"

# Step 6: Update the main data set with the new data
generation_filled[generation_filled$Artificial == "C" & generation_filled$MapCode == "HR", c("TotalGenerationValue", "Artificial")] <- generation_HR[c("TotalGenerationValue", "Artificial")]

#Save artificially filled datapoints for wind for archive
generation_filled_artificial_wind <- generation_filled[!(generation_filled$Artificial == "0"),]


#############################################################################################
##################################### DATA AGGREGATION ######################################
#############################################################################################

###################### AGGREGATION - HOURLY & AIB ##########################
############################################################################

# Drop Artificial column & adjust data types
generation_filled <- generation_filled[,-5]
generation_filled$TotalGenerationValue <- as.numeric(generation_filled$TotalGenerationValue)

# Create a new column for the hour component of DateTime
generation_filled$Hour <- format(generation_filled$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate (sum up) over AIB countries
generation_hourly_AIB_wind <- aggregate(x = generation_filled$TotalGenerationValue,
                             by = list(Hour = generation_filled$Hour),
                             FUN = sum)

colnames(generation_hourly_AIB_wind) <- c("DateTime", "TotalGenerationValue")


############## AGGREGATION - DAILY, WEEKLY, MONTHLY, YEARLY ################
############################################################################

# DAILY DATA: Computed from hourly values
# Aggregate data on daily level
generation_daily_AIB_wind <- aggregate(x = generation_hourly_AIB_wind$TotalGenerationValue,
                            by = list(Date = as.Date(generation_hourly_AIB_wind$DateTime)),
                            FUN = sum)
colnames(generation_daily_AIB_wind) <- c("Date", "TotalGenerationValue")

# WEEKLY DATA: Computed from daily values
generation_weekly_AIB_wind <- aggregate(x = generation_daily_AIB_wind$TotalGenerationValue,
                             by = list(Date = as.Date(cut(generation_daily_AIB_wind$Date, "week"))), 
                             FUN = sum)
colnames(generation_weekly_AIB_wind) <- c("Date", "TotalGenerationValue")

# MONTHLY DATA: Computed from daily values
generation_monthly_AIB_wind <- aggregate(x = generation_daily_AIB_wind$TotalGenerationValue,
                              by = list(Date = as.Date(cut(generation_daily_AIB_wind$Date, "month"))), 
                              FUN = sum)

colnames(generation_monthly_AIB_wind) <- c("Date", "TotalGenerationValue")

# YEARLY DATA: Computed from daily values
generation_yearly_AIB_wind <- aggregate(x = generation_daily_AIB_wind$TotalGenerationValue,
                             by = list(Date = as.Date(cut(generation_daily_AIB_wind$Date, "year"))), 
                             FUN = sum)

colnames(generation_yearly_AIB_wind) <- c("Date", "TotalGenerationValue")



###################################################################################################################
###################################################################################################################
################################################ SOLAR & WIND #####################################################
###################################################################################################################
###################################################################################################################

#############################################################################################
###################################### DATA AGGREGATION #####################################
#############################################################################################

###################### AGGREGATION - HOURLY & AIB ##########################
############################################################################

# Ensure same data sorting in solar and wind datasets by DateTime
generation_hourly_AIB_solar <- arrange(generation_hourly_AIB_solar, DateTime)
generation_hourly_AIB_wind <- arrange(generation_hourly_AIB_wind, DateTime)

# Add up solar and wind generation values per timestamp
generation_hourly_AIB_solarwind <- generation_hourly_AIB_solar
generation_hourly_AIB_solarwind$TotalGenerationValue <- generation_hourly_AIB_solar$TotalGenerationValue + generation_hourly_AIB_wind$TotalGenerationValue


############## AGGREGATION - DAILY, WEEKLY, MONTHLY, YEARLY ################
############################################################################

# DAILY DATA: Computed from hourly values
# Aggregate data on daily level
generation_daily_AIB_solarwind <- aggregate(x = generation_hourly_AIB_solarwind$TotalGenerationValue,
                                       by = list(Date = as.Date(generation_hourly_AIB_solarwind$DateTime)),
                                       FUN = sum)
colnames(generation_daily_AIB_solarwind) <- c("Date", "TotalGenerationValue")

# WEEKLY DATA: Computed from daily values
generation_weekly_AIB_solarwind <- aggregate(x = generation_daily_AIB_solarwind$TotalGenerationValue,
                                        by = list(Date = as.Date(cut(generation_daily_AIB_solarwind$Date, "week"))), 
                                        FUN = sum)
colnames(generation_weekly_AIB_solarwind) <- c("Date", "TotalGenerationValue")

# MONTHLY DATA: Computed from daily values
generation_monthly_AIB_solarwind <- aggregate(x = generation_daily_AIB_solarwind$TotalGenerationValue,
                                         by = list(Date = as.Date(cut(generation_daily_AIB_solarwind$Date, "month"))), 
                                         FUN = sum)

colnames(generation_monthly_AIB_solarwind) <- c("Date", "TotalGenerationValue")

# YEARLY DATA: Computed from daily values
generation_yearly_AIB_solarwind <- aggregate(x = generation_daily_AIB_solarwind$TotalGenerationValue,
                                        by = list(Date = as.Date(cut(generation_daily_AIB_solarwind$Date, "year"))), 
                                        FUN = sum)

colnames(generation_yearly_AIB_solarwind) <- c("Date", "TotalGenerationValue")

