library(dplyr)
library(lubridate)

y1 <- 2016
y2 <- 2021
ndays_b <- 60 # Originally set to 4 days; only CZ shows gaps larger than 4 days - those were filled via approach B - see method section of paper.

###################################################################################################################
###################################################################################################################
################################################## DATA INTAKE ####################################################
###################################################################################################################
###################################################################################################################

#### WHAT THIS CODE SECTIONS DOES ####
# load_raw_data: Read in all csv load files to a dataframe (countrylevel (CTY) data)
# load_combined_data: Select columns with timestamps, resolution codes (i.e. measurement intervals), country codes, load values
# load_combined_data: Filter for selected countries of analysis
# load_combined_data: Convert resolution codes to numeric (unit: minutes)

setwd("INSERTPATH/Load Dataset")

load_raw_data <- data.frame()

# loop through all csv files
for (year in y1:y2) {
  for (month in 1:12) {
    # use sprintf to force the month to be always two digits long, otherwise it would not find the files with months 01-09
    file_name <- paste0(year, "_", sprintf("%02d", month), "_ActualTotalLoad_6.1.A.csv")
    
    if (file.exists(file_name)) {
      # read in the file and combine it with the existing data
      print(file_name)
      new_data <- read.csv(file_name, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      new_data <- new_data[new_data$AreaTypeCode == 'CTY',]
      load_raw_data <- rbind(load_raw_data, new_data)
    } else {
      print(paste(file_name, "was not found"))
    } 
  }
}
rm(new_data)

names(load_raw_data)[names(load_raw_data) == 'ï..DateTime'] <- 'DateTime'

# Count NA values in the DateTime column of load_raw_data 
na_count_raw_data_date_time <- sum(is.na(load_raw_data$DateTime))

# Display na_count_load_raw_data_date_time
print(na_count_raw_data_date_time)
##Result: There are no NA values in the data set

load_combined_data <- load_raw_data[,c("DateTime", "ResolutionCode", "MapCode", "TotalLoadValue")]

# Filter countries according to their relevance for analysis
load_combined_data <- load_combined_data[!(load_combined_data$MapCode %in% c("AL","BA","BG","GB","GE","GR","HU","LT","ME","MD","MK","PL","RO","UA","XK")),]

# Save Resolution Code as numeric
load_combined_data$ResolutionCode[load_combined_data$ResolutionCode == "PT60M"] <- 60
load_combined_data$ResolutionCode[load_combined_data$ResolutionCode == "PT30M"] <- 30
load_combined_data$ResolutionCode[load_combined_data$ResolutionCode == "PT15M"] <- 15
load_combined_data$ResolutionCode <- as.numeric(load_combined_data$ResolutionCode)

# Count NA values in the TotalloadValue column of load_combined_data 
na_count_combined_data_total_load_value <- sum(is.na(load_combined_data$TotalLoadValue))

# Display na_count_combined_data_total_load_value
print(na_count_combined_data_total_load_value)
##Result: There are no NA values in the data set

# Count NA values in the DateTime column of load_combined_data 
na_count_combined_data_date_time <- sum(is.na(load_combined_data$DateTime))

# Display na_count_load_raw_data_date_time
print(na_count_combined_data_date_time)
##Result: There are no NA values in the data set

# Convert DateTime column to POSIXct format
load_combined_data$DateTime <- ymd_hms(load_combined_data$DateTime)

# Check format of DateTime
class(load_combined_data$DateTime)

# Save archive copy for later processing
load_combined_data_archive <- load_combined_data


###################################################################################################################
###################################################################################################################
##################### INITIAL DATA SET EXPLORATION (e.g. CHEKCKING FOR MISSING VALUES) ############################
###################################################################################################################
###################################################################################################################

# Count NA values in the TotalloadValue column of load_combined_data 
na_count_combined_data_total_load_value <- sum(is.na(load_combined_data$TotalLoadValue))

# Display na_count_combined_data_total_load_value
print(na_count_combined_data_total_load_value)
##Result: There are no NA values in the data set

# Count NA values in the DateTime column of load_combined_data 
na_count_combined_data_date_time <- sum(is.na(load_combined_data$DateTime))

# Display na_count_combined_data_date_time
print(na_count_combined_data_date_time)
##Result: There are NA values in the data set

###################################################################################################################
###################################################################################################################
###################### INITIAL VALUE AGGREGATION TO HARMONIZE DATA TO HOURLY TIMESTAMPs ###########################
###################################################################################################################
###################################################################################################################

#### WHAT THIS CODE SECTION DOES ####
# As analysis is run on shared minimal resolution of all countries (60min -> hourly), values are first harmonized to an hourly level
#load_hourly_data: Aggregate data in load_combined_data per hourly timestamp and country (aggregation function mean - as different resolution codes exist)

# Create a new column for the hour component of DateTime
load_combined_data$Hour <- format(load_combined_data$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate the data by Hour and MapCode
load_hourly_data <- aggregate(x = load_combined_data$TotalLoadValue,
                                    by = list(Hour = load_combined_data$Hour,
                                              MapCode = load_combined_data$MapCode),
                                    FUN = mean)

# Rename the columns and display result
colnames(load_hourly_data) <- c("DateTime", "MapCode", "TotalLoadValue")

  #Check format in DateTime
  class(load_hourly_data$DateTime)

# Convert DateTime column to POSIXct format
load_hourly_data$DateTime <- ymd_hms(load_hourly_data$DateTime)

# Save for archive
load_hourly_data_archive <- load_hourly_data

  #CHECK: No new NAs came up
  # Count NA values in the TotalloadValue column of load_hourly_data
  na_count_hourly_data_total_load_value <- sum(is.na(load_hourly_data$total_load_value))
  
  # Display na_count_hourly_data_total_load_value
  print(na_count_hourly_data_total_load_value)
  ## RESULT: NO new NAs came up
  
  # Count NA values in the DateTime column of load_hourly_data
  na_count_hourly_data_date_time <- sum(is.na(load_hourly_data$DateTime))
  
  # Display na_count_hourly_data_date_time
  print(na_count_hourly_data_date_time)
  ## RESULT: NO new NAs came up

###################################################################################################################
###################################################################################################################
################################################ DATA IMPUTATION ##################################################
###################################################################################################################
###################################################################################################################

#############################################################################################
################################# COMPLETE DF CREATION ######################################
#############################################################################################

#### WHAT THIS CODE SECTIONS DOES ####
# load_complete: Create a dataframe structure for all countries with complete timestamps for their respective period of analysis
# FOR each country do:
  # load_map_code: Create empty dataframe which is complete with all timestamps within the respective period of analysis
  # Add a column "Artificial" to track artificial value creation
  # Possible values of Artificial: 0: Total load is original value; 1: Total load must be filled artificially; 
  # 1s will later be replaced depending on used data imputation approach --> A: Approach A; B: Approach B; H: Filled via h-Scaling Factors
  # In this step, only the values 0 and 1 are assigned based on whether the timestamp existed already in the original dataset. 
  # All rows with value 1 are then handled later by artificial value creation.
  # Specifically in this step, this means that all timestamps which were missing in the entso-e data set are marked to be filled artificially.
  # It is assumed that at missing timestamps the load value must be filled artificially.
  # Update load_map_code: Fill in the existing load data of the country from load_hourly_aggregated into load_map_code
  # Add the now complete dataframe of the country to the overall dataframe "load_complete"
# END FOR

# Post-Processing for Cleanup and Standardization:
  #Delete rows from hours where DST ends
  #Check number of entries per MapCode

# Initial DataFrame construction
load_combined_data_v2 <- cbind(load_hourly_data[, "DateTime"],
                                  ResolutionCode = 60,
                                  load_hourly_data[, 2:ncol(load_hourly_data)])
colnames(load_combined_data_v2) <- c("DateTime", "ResolutionCode", "MapCode", "TotalLoadValue")

  #Check format in DateTime
  class(load_combined_data_v2$DateTime)
  
  # Count NA values in the DateTime column of load_combined_data_v2
  na_count_combined_data_v2_date_time <- sum(is.na(load_combined_data_v2$DateTime))
  
  # Display na_count_combined_data_v2_date_time
  print(na_count_combined_data_v2_date_time)
  ## RESULT: NO new NAs came up

# Convert DateTime column to POSIXct format \\ lubridate function not needed here as no new NAs are created for DateTime (see check below)
load_combined_data_v2$DateTime <- as.POSIXct(load_combined_data_v2$DateTime, format = "%Y-%m-%d %H:%M:%S")

  # Count NA values in the DateTime column of load_combined_data_v2
  na_count_combined_data_v2_date_time <- sum(is.na(load_combined_data_v2$DateTime))
  
  # Display na_count_combined_data_v2_date_time
  print(na_count_combined_data_v2_date_time)
  ## RESULT: NO new NAs came up

# Unique combinations of MapCode and ResolutionCode 
load_resolution_code <- unique(load_combined_data_v2[, c("MapCode", "ResolutionCode")])

# Initialize the complete dataset 
load_complete <- data.frame(DateTime = character(),
                                  ResolutionCode = numeric(),
                                  MapCode = character(),
                                  TotalLoadValue = numeric(),
                                  Artificial = character(),
                                  stringsAsFactors = FALSE)

# Loop over each MapCode and ResolutionCode
for (i in 1:nrow(load_resolution_code)) {
  map_code <- load_resolution_code[i, "MapCode"]
  resolution_code <- load_resolution_code[i, "ResolutionCode"]
  
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
  
    # Subset and filter existing data for this map code starting from the exact start_date
  load_map_code <- subset(load_combined_data_v2, MapCode == map_code &
                            DateTime >= start_date)
  load_map_code$Artificial <- "0"
  
  # Identify timestamps that are missing in the existing data 
  timestamps_missing <- setdiff(timestamps, load_map_code$DateTime)
  
  # Handling missing timestamps 
  if (length(timestamps_missing) > 0) {
    load_map_code_add <- data.frame(DateTime = timestamps_missing,
                                          ResolutionCode = resolution_code,
                                          MapCode = map_code,
                                          TotalLoadValue = NA,
                                          Artificial = "1")
    
    # Combine and sort full records for this map code
    load_map_code_all <- rbind(load_map_code, load_map_code_add)
    load_map_code_all <- load_map_code_all[order(load_map_code_all$DateTime), ]
    rownames(load_map_code_all) <- NULL
    
    # Append to the complete dataset
    load_complete <- rbind(load_complete, load_map_code_all)
  } else {
    load_complete <- rbind(load_complete, load_map_code)
  }
  
  # Cleanup 
  rm(load_map_code)
  rownames(load_complete) <- NULL
}

# Check the earliest date in load_complete
min(load_complete$DateTime)

# Remove the non-existent hour due to the start of the DST
  # Define the specific dates and time to exclude
  dates_to_exclude <- as.POSIXct(c("2016-10-30 02:00:00", "2017-10-29 02:00:00", 
                                   "2018-10-28 02:00:00", "2019-10-27 02:00:00", 
                                   "2020-10-25 02:00:00", "2021-10-31 02:00:00"), 
                                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Filter out the rows with the specified datetimes
  load_complete <- load_complete[!load_complete$DateTime %in% dates_to_exclude, ]
  
  # Verify that the specific dates and times have been removed
  any(load_complete$DateTime %in% dates_to_exclude)

# Check for number of missing timestamps & total timestamps per country
  # Subsetting the dataframe where Artificial is "1" and counting rows
  number_of_ones <- nrow(load_complete[load_complete$Artificial == "1", ])
  print(number_of_ones)
  
  # Count the number of rows for each MapCode 
  number_of_rows_per_mapcode <- load_complete %>%
    group_by(MapCode) %>%
    summarise(Count = n())
  
  # Print the results
  print(number_of_rows_per_mapcode)


#############################################################################################
################################ ARTIFICIAL VALUE GENERATION ################################
#############################################################################################

#### WHAT THIS CODE SECTION DOES #### 
# Generates reasonable values for renewable generation for the timestamps that had previously been missing in the entso-e dataset
# For minor data gaps a two-pronged apporach (A & B) is used; for larger data gaps scaling (h) factors are used (see method section for details)

load_filled <- data.frame(DateTime = character(),
                            ResolutionCode = numeric(),
                            MapCode = character(),
                            TotalLoadValue = numeric(),
                            Artificial = character(),
                            stringsAsFactors = FALSE)

load_filled_artificial <- data.frame(DateTime = character(),
                          ResolutionCode = numeric(),
                          MapCode = character(),
                          TotalLoadValue = numeric(),
                          Artificial = character(),
                          stringsAsFactors = FALSE)

############################## APPROACH A & B ##############################
############################################################################

# Iterate over every MapCode
map_codes <- unique(load_complete$MapCode)
for (map_code in map_codes) {
  # Filter the dataframe for current map code
  load_complete_map_code <- load_complete[load_complete$MapCode == map_code, ]
  
  load_complete_map_code <- arrange(load_complete_map_code, DateTime)
  
  # Check for and fill data gaps, where approach A is to be used
  for (i in 1:(nrow(load_complete_map_code))) {
    if (load_complete_map_code[i, "Artificial"] == "1") {
      
      # If data is missing directly at the first measuring point, but the following measuring points are available, use the following measuring point
      if (i == 1 && load_complete_map_code[i+1, "Artificial"] != "1") {
        load_complete_map_code[i, "TotalLoadValue"] <- load_complete_map_code[i+1, "TotalLoadValue"]
        load_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      # If data is missing at the last measuring point, but the measuring points before it are available, use the previous measuring point
      if (i == nrow(load_complete_map_code) && load_complete_map_code[i-1, "Artificial"] != "1") {
        load_complete_map_code[i, "TotalLoadValue"] <- load_complete_map_code[i-1, "TotalLoadValue"]
        load_complete_map_code[i, "Artificial"] <- "A"
        next
      }
      
      previous_value <- as.numeric(load_complete_map_code[i - 1, "TotalLoadValue"])
      previous_artificial <- load_complete_map_code[i - 1, "Artificial"]
      next_value <- as.numeric(load_complete_map_code[i + 1, "TotalLoadValue"])
      next_artificial <- load_complete_map_code[i + 1, "Artificial"]
      
      # Check whether previous_value or next_value have the value "" and abort the function (then go for Approach B or C)
      if (previous_artificial == "1" || next_artificial == "1") {
        next
      }
      
      print(paste0("Lücke in ", map_code, " an Index ", i, " nach Fall A."))
      load_complete_map_code[i, "TotalLoadValue"] <- (previous_value + next_value) / 2
      load_complete_map_code[i, "Artificial"] <- "A"
    }
  }
  
  # Check for and fill data gaps, where approach B is to be used
  for (i in 1:(nrow(load_complete_map_code) - 1)) {
    
    # Calculate the number of daily observations for a complete data set depending on the ResolutionCode
    daily_observations <- 24 * (60 / load_resolution_code[(load_resolution_code$MapCode == map_code), "ResolutionCode"])
    
    if (load_complete_map_code[i, "Artificial"] == "1") { # i points to the first missing value of the gap
      # Search for the first available value j
      j <- i + 1 
      while (j <= nrow(load_complete_map_code) && load_complete_map_code[j, "Artificial"] == "1") {
        j <- j + 1
      }
      
      l <- j - 1 # l points to the last missing value of the gap
      
      # Check whether the continuous gap is too large (more than 60 days) // only 1 data gap in load data is larger than a month (Croatia)
      if ((j - i) > (ndays_b * daily_observations)) {
        # Gap too large, proceed according to case C (h-Scling factors)
        print(paste0("Lücke in ", map_code, " von Index ", i, " bis ", l, " nach Fall B nicht befüllbar."))
        for (k in i:l) {
          load_complete_map_code[k, "Artificial"] <- "C"
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
          prev_value <- load_complete_map_code[m, "TotalLoadValue"]
          while (prev_value == "" || is.na(prev_value)) {
            m <- m - daily_observations
            if (m < 1) { break } # Ensure m does not go below 1
            prev_value <- load_complete_map_code[m, "TotalLoadValue"]
          }
          
          n <- k # assign a temporary variable n for the value search
          # If the gap is larger than two days, make the value search easier by leading to the last day of the gap
          #if (length(l-i) >= 3* daily_observations) {
          #while (n < (l - daily_observations)) {
          #n <- n + daily_observations
          #}
          #}
          
          # Determine the fist subsequently available day
          while (n <= l && n <= nrow(load_complete_map_code)) {
            n <- n + daily_observations
          }
          next_value <- load_complete_map_code[n, "TotalLoadValue"]
          while (next_value == "" || is.na(next_value)) {
            n <- n + daily_observations
            if (n > nrow(load_complete_map_code)) { break } # Ensure n does not exceed row count
            next_value <- load_complete_map_code[n, "TotalLoadValue"]
          }
          
          # Fill gap using approach B
          load_complete_map_code[k, "TotalLoadValue"] <- (as.numeric(prev_value) + as.numeric(next_value)) / 2
          load_complete_map_code[k, "Artificial"] <- "B"
        }
      }
      
      # Set the index to the end of the current gap
      i <- j
    }
  }
  
  print(paste(map_code, "is finished."))
  load_filled <- load_filled[!(load_filled$MapCode == map_code),]
  load_filled <- rbind(load_filled, load_complete_map_code)
}


################# DATA IMPUTATION VIA H-FACTORS FOR CYPRUS #################
############################################################################
# See Method Section for data on h-factors

# TotalLoadValues that need to be created
rows_count <- load_filled %>%
  filter(MapCode == "CY", Artificial == "C") %>%
  summarise(count = n())
print(paste("Number of rows with MapCode CY and Artificial C:", rows_count$count))

# Creating the data frame with the h-factors
h_CY <- data.frame(
  Month = 1:12,
  H = c(0.895691769, 0.89253589, 0.91736895, 0.98004483, 0.966059123, 1.061806178, 0.926116669, 1.001325283, 0.932291239, 0.996582748, 0.953985488, 1.106881564)
)

# Step 1: Create the data set to be filled as well as the reference data set and timestamp sets
load_CY <- load_filled[load_filled$Artificial == "C" & load_filled$MapCode == "CY", ]
timestamps_CY <- load_CY$DateTime
timestamps_CY <- timestamps_CY + lubridate::years(1)
load_CY_ref <- load_filled[(load_filled$DateTime %in% timestamps_CY) & load_filled$MapCode == "CY", ]

# Correction of the February 29 error for 2016 (duplication of February 28)
rownames(load_CY_ref) <- NULL
load_CY_feb <- load_CY_ref[1393:1416, ]
load_CY_ref <- rbind(load_CY_ref[1:1416, ], load_CY_feb, load_CY_ref[1417:nrow(load_CY_ref), ])
rm(load_CY_feb)

  # Check number of rows
  rows_count <- load_CY%>%
    summarise(count = n())
  print(paste("Number of rows in load_CY:", rows_count$count))
  
  rows_count <- load_CY_ref%>%
    summarise(count = n())
  print(paste("Number of rows in load_CY_ref:", rows_count$count))

# Step 2: Extract month & year
load_CY$Month <- as.numeric(format(load_CY$DateTime, "%m"))

# Step 3: Enter the corresponding h-factor for the measuring points in the data set
load_CY <- merge(load_CY, h_CY, by = "Month")
load_CY <- load_CY[,-1]

# Step 4: Scale the measured values using the respective h-factor
load_CY$Load2017 <- load_CY_ref$TotalLoadValue
load_CY$TotalLoadValue <- load_CY$H * as.numeric(load_CY$Load2017)  

# Step 5:  Update the artificial variable with "H" for all scaled measurement points
load_CY$Artificial <- "H"

# Step 6: Update the main data set with the new data
load_filled[load_filled$Artificial == "C" & load_filled$MapCode == "CY", c("TotalLoadValue", "Artificial")] <- load_CY[c("TotalLoadValue", "Artificial")]


###################### ICELAND VALUES VIA H-FACTORS ########################
############################################################################
# See Method Section for data on h-factors

month = c("2016-01", "2016-02", "2016-03", "2016-04", "2016-05", "2016-06", "2016-07", "2016-08", "2016-09", "2016-10",
          "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2017-06", "2017-07", "2017-08",
          "2017-09", "2017-10", "2017-11", "2017-12", "2018-01", "2018-02", "2018-03", "2018-04", "2018-05", "2018-06",
          "2018-07", "2018-08", "2018-09", "2018-10", "2018-11", "2018-12", "2019-01", "2019-02", "2019-03", "2019-04",
          "2019-05", "2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02",
          "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12",
          "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09", "2021-10",
          "2021-11", "2021-12")

h = c(0.111861171, 0.120433431, 0.131498094, 0.143870251, 0.170439099, 0.184330185, 0.188318243, 0.177981134, 0.174925586,
      0.14548084, 0.125405355, 0.128069777, 0.120440368, 0.120789919, 0.133938746, 0.144959389, 0.168734046, 0.183779356,
      0.194181812, 0.183903274, 0.174278338, 0.152966906, 0.131482244, 0.122906283, 0.120443132, 0.118164633, 0.122220005,
      0.146669026, 0.180711396, 0.186110081, 0.199745306, 0.189503987, 0.175149986, 0.153928169, 0.142677378, 0.128039086,
      0.120403441, 0.129030335, 0.134149261, 0.156938052, 0.171571312, 0.179141282, 0.188075137, 0.179256588, 0.168794259,
      0.144175594, 0.124556223, 0.125791677, 0.126516466, 0.124296444, 0.129619107, 0.144460674, 0.155784021, 0.176160294,
      0.175539249, 0.173869715, 0.162481005, 0.143746699, 0.132116609, 0.123629328, 0.109187312, 0.108388293, 0.128573336,
      0.138579374, 0.159862065, 0.179982462, 0.188815599, 0.180408236, 0.175170882, 0.158533127, 0.143159241, 0.12512334)

# Create dataframe with h-factors
h_IS_load <- data.frame(Month = month, H = h)

load_IS <- load_filled[load_filled$MapCode == "NO",]
load_IS$MapCode <- "IS"

rows_count <- load_IS%>%
  summarise(count = n())
print(paste("Number of rows in load_IS:", rows_count$count))

# Step 2: Extract Month & Year
load_IS$Month <- format(load_IS$DateTime, "%Y-%m")

# Step 3: Enter the corresponding h-factor for the measuring points in the data set
load_IS <- merge(load_IS, h_IS_load, by = "Month")
load_IS <- load_IS[,-1]

# Step 4: Scale the NO values using the respective h-factor
for (i in 1:nrow(load_IS)) {
  load_IS$TotalLoadValue[i] <- as.numeric(load_IS$TotalLoadValue[i]) * load_IS$H[i]
}

# Step 5: Update the artificial variable with H for all scaled measurement points
load_IS$Artificial <- "H"

# Step 6: Adjust the data set structure for later merging with the other data
load_IS <- load_IS[,-6]
load_filled <- rbind(load_filled, load_IS)

# Save entire filled dataset for archive
load_filled_archive <- load_filled
load_filled_artificial <- load_filled[!(load_filled$Artificial == "0"),]

###################################################################################################################
###################################################################################################################
################################################ DATA AGGREGATION #################################################
###################################################################################################################
###################################################################################################################

###################### AGGREGATION - HOURLY & AIB ##########################
############################################################################

# Count the number of rows for each MapCode 
number_of_rows_per_mapcode <- load_filled %>%
  group_by(MapCode) %>%
  summarise(Count = n())

# Print the results
print(number_of_rows_per_mapcode)

# Drop Artificial column & adjust data types
load_filled <- load_filled[,-5]
load_filled$TotalLoadValue <- as.numeric(load_filled$TotalLoadValue)

# Create a new column for the hour component of DateTime
load_filled$Hour <- format(load_filled$DateTime, "%Y-%m-%d %H:00:00")

# Aggregate (sum up) over AIB countries
load_hourly_AIB <- aggregate(x = load_filled$TotalLoadValue,
                             by = list(Hour = load_filled$DateTime),
                             FUN = sum)

colnames(load_hourly_AIB) <- c("DateTime", "TotalLoadValue")

  # Check number of rows
  rows_count <- load_hourly_AIB%>%
    summarise(count = n())
  print(paste("Number of rows in load_hourly_AIB:", rows_count$count))


############## AGGREGATION - DAILY, WEEKLY, MONTHLY, YEARLY ################
############################################################################

# DAILY DATA: Computed from hourly values
# Aggregate data on daily level
load_daily_AIB <- aggregate(x = load_hourly_AIB$TotalLoadValue,
                             by = list(Date = as.Date(load_hourly_AIB$DateTime)),
                             FUN = sum)
colnames(load_daily_AIB) <- c("Date", "TotalLoadValue")

# WEEKLY DATA: Computed from daily values
load_weekly_AIB <- aggregate(x = load_daily_AIB$TotalLoadValue,
                              by = list(Date = as.Date(cut(load_daily_AIB$Date, "week"))), 
                              FUN = sum)
colnames(load_weekly_AIB) <- c("Date", "TotalLoadValue")

# MONTHLY DATA: Computed from daily values
load_monthly_AIB <- aggregate(x = load_daily_AIB$TotalLoadValue,
                               by = list(Date = as.Date(cut(load_daily_AIB$Date, "month"))), 
                               FUN = sum)

colnames(load_monthly_AIB) <- c("Date", "TotalLoadValue")

# YEARLY DATA: Computed from daily values
load_yearly_AIB <- aggregate(x = load_daily_AIB$TotalLoadValue,
                              by = list(Date = as.Date(cut(load_daily_AIB$Date, "year"))), 
                              FUN = sum)

colnames(load_yearly_AIB) <- c("Date", "TotalLoadValue")

