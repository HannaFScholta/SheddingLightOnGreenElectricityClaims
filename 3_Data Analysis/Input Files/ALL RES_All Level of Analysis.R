###################################################################################################################
###################################################################################################################
####################################### ALL RES - ALL LEVELS OF ANALYSIS ##########################################
###################################################################################################################
###################################################################################################################

# Define the dataset names
data_types <- c("ALL RES")

#############################################################################################
############################### Hourly level of analysis ####################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")

#Read-in hourly_coverage file
file_path <- file.path(input_path, file_name)
hloa <- read_excel(file_path, sheet = "hourly_coverages")

############################### DATA CLEANING ###############################
#############################################################################

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


############################### DATA ANALYSIS ###############################
#############################################################################

########################### FOR INDIVIDUAL YEARS ############################

# Years to loop through
years <- c("2016", "2017", "2018", "2019", "2020", "2021")

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
  # Loop through each year
  for (year in years) {
    # Create variable names
    all_intervals_var <- paste0("hloa_", year, "_allIntervals_quantity_abs")
    uncovered_intervals_var <- paste0("hloa_", year, "_uncoveredIntervals_quantity_abs")
    rel_dec_var <- paste0("hloa_", year, "_uncoveredIntervals_quantity_rel_dec")
    covered_rel_dec_var <- paste0("hloa_", year, "_coveredIntervals_quantity_rel_dec")
    
    # Count all intervals
    all_intervals <- hloa %>%
      filter(`Year Index` == year) %>%
      count()
    
    # Store count result
    assign(all_intervals_var, as.numeric(all_intervals[[1, 1]]))
    
    # Filter and summarize uncovered intervals
    uncovered_intervals <- hloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!uncovered_intervals_var := sum(Coverage < 0, na.rm = TRUE))
    
    # Extract numeric value
    uncovered_intervals <- as.numeric(uncovered_intervals[[1, 1]])
    
    # Store uncovered intervals result
    assign(uncovered_intervals_var, uncovered_intervals)
    
    # Calculate relative quantity
    rel_dec <- uncovered_intervals / as.numeric(all_intervals[[1, 1]])
    
    # Store relative quantity result
    assign(rel_dec_var, rel_dec)
    
    # Calculate covered intervals relative quantity
    covered_rel_dec <- 1 - rel_dec
    
    # Store covered intervals relative quantity result
    assign(covered_rel_dec_var, covered_rel_dec)
  }


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_undercoverage_var <- paste0("hloa_", year, "_uncoveredIntervals_sumUndercoverage_abs")
    sum_godemand_var <- paste0("hloa_", year, "_uncoveredIntervals_sumGODemand_abs")
    avg_undercoverage_rel2demand_var <- paste0("hloa_", year, "_uncoveredIntervals_avgUndercoverage_rel2demand_dec")
    
    # Sum undercoverage
    sum_undercoverage <- hloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_undercoverage_var := sum(`Coverage`, na.rm = TRUE))
    
    # Extract numeric value
    sum_undercoverage <- as.numeric(sum_undercoverage[[1, 1]])
    
    # Store result
    assign(sum_undercoverage_var, sum_undercoverage)
    
    # Sum GO Demand
    sum_godemand <- hloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average undercoverage relative to demand
    avg_undercoverage_rel2demand <- sum_undercoverage / sum_godemand * (-1)
    
    # Store result
    assign(avg_undercoverage_rel2demand_var, avg_undercoverage_rel2demand)
  }


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year) 
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_gosupply_var <- paste0("hloa_", year, "_allIntervals_sumGOSupply_abs")
    sum_godemand_var <- paste0("hloa_", year, "_allIntervals_sumGODemand_abs")
    avg_coverage_rel2demand_var <- paste0("hloa_", year, "_allIntervals_avgCoverage_rel2demand_dec")
    
    # Sum GO Supply
    sum_gosupply <- hloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_gosupply_var := sum(`GO Supply`, na.rm = TRUE))
    
    # Extract numeric value
    sum_gosupply <- as.numeric(sum_gosupply[[1, 1]])
    
    # Store result
    assign(sum_gosupply_var, sum_gosupply)
    
    # Sum GO Demand
    sum_godemand <- hloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average coverage relative to demand
    avg_coverage_rel2demand <- sum_gosupply / sum_godemand
    
    # Store result
    assign(avg_coverage_rel2demand_var, avg_coverage_rel2demand)
  }


############################ FOR TOTAL TIMEFRAME ############################

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)

  hloa_2016to21_uncoveredIntervals_quantity_abs <- hloa %>%
    filter(`Coverage` <0)%>%
    summarize(hloa_2016to21_uncoveredIntervals_quantity_abs = sum(Coverage <0, na.rm = TRUE))
  hloa_2016to21_uncoveredIntervals_quantity_abs <- as.numeric(hloa_2016to21_uncoveredIntervals_quantity_abs[[1,1]])
  
  hloa_2016to21_allIntervals_quantity_abs <- hloa %>%
    count()
  hloa_2016to21_uncoveredIntervals_quantity_rel_dec <- hloa_2016to21_uncoveredIntervals_quantity_abs/hloa_2016to21_allIntervals_quantity_abs
  hloa_2016to21_uncoveredIntervals_quantity_rel_dec <- as.numeric(hloa_2016to21_uncoveredIntervals_quantity_rel_dec[[1,1]])
  
  hloa_2016to21_coveredIntervals_quantity_rel_dec <- 1-hloa_2016to21_uncoveredIntervals_quantity_rel_dec


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- hloa %>%
    filter(`Coverage` <0)%>%
    summarize(hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs = sum(`Coverage`, na.rm = TRUE))
  
  hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- as.numeric(hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs[[1,1]])
  
  hloa_2016to21_uncoveredIntervals_sumGODemand_abs <- hloa %>%
    filter(`Coverage` <0)%>%
    summarize(hloa_2016to21_uncoveredIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))
  
  hloa_2016to21_uncoveredIntervals_sumGODemand_abs <- as.numeric(hloa_2016to21_uncoveredIntervals_sumGODemand_abs[[1,1]])
  
  hloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec <- hloa_2016to21_uncoveredIntervals_sumUndercoverage_abs/hloa_2016to21_uncoveredIntervals_sumGODemand_abs*(-1)


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year) 
  hloa_2016to21_allIntervals_sumGOSupply_abs <- hloa %>%
    summarize(hloa_2016to21_allIntervals_sumGOSupply_abs = sum(`GO Supply`, na.rm = TRUE))
  
  hloa_2016to21_allIntervals_sumGOSupply_abs <- as.numeric(hloa_2016to21_allIntervals_sumGOSupply_abs[[1,1]])
  
  hloa_2016to21_allIntervals_sumGODemand_abs <- hloa %>%
    summarize(hloa_2016to21_allIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))
  
  hloa_2016to21_allIntervals_sumGODemand_abs <- as.numeric(hloa_2016to21_allIntervals_sumGODemand_abs[[1,1]])
  
  hloa_2016to21_allIntervals_avgCoverage_rel2demand_dec <- hloa_2016to21_allIntervals_sumGOSupply_abs/hloa_2016to21_allIntervals_sumGODemand_abs


#############################################################################################
################################ DAILY LEVEL OF ANALYSIS ####################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
dloa <- read_excel(file_path, sheet = "daily_coverages")

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

############################### DATA ANALYSIS ###############################
#############################################################################

########################### FOR INDIVIDUAL YEARS ############################

# Years to loop through
years <- c("2016", "2017", "2018", "2019", "2020", "2021")

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
  # Loop through each year
  for (year in years) {
    # Create variable names
    all_intervals_var <- paste0("dloa_", year, "_allIntervals_quantity_abs")
    uncovered_intervals_var <- paste0("dloa_", year, "_uncoveredIntervals_quantity_abs")
    rel_dec_var <- paste0("dloa_", year, "_uncoveredIntervals_quantity_rel_dec")
    covered_rel_dec_var <- paste0("dloa_", year, "_coveredIntervals_quantity_rel_dec")
    
    # Count all intervals
    all_intervals <- dloa %>%
      filter(`Year Index` == year) %>%
      count()
    
    # Store count result
    assign(all_intervals_var, as.numeric(all_intervals[[1, 1]]))
    
    # Filter and summarize uncovered intervals
    uncovered_intervals <- dloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!uncovered_intervals_var := sum(Coverage < 0, na.rm = TRUE))
    
    # Extract numeric value
    uncovered_intervals <- as.numeric(uncovered_intervals[[1, 1]])
    
    # Store uncovered intervals result
    assign(uncovered_intervals_var, uncovered_intervals)
    
    # Calculate relative quantity
    rel_dec <- uncovered_intervals / as.numeric(all_intervals[[1, 1]])
    
    # Store relative quantity result
    assign(rel_dec_var, rel_dec)
    
    # Calculate covered intervals relative quantity
    covered_rel_dec <- 1 - rel_dec
    
    # Store covered intervals relative quantity result
    assign(covered_rel_dec_var, covered_rel_dec)
  }


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_undercoverage_var <- paste0("dloa_", year, "_uncoveredIntervals_sumUndercoverage_abs")
    sum_godemand_var <- paste0("dloa_", year, "_uncoveredIntervals_sumGODemand_abs")
    avg_undercoverage_rel2demand_var <- paste0("dloa_", year, "_uncoveredIntervals_avgUndercoverage_rel2demand_dec")
    
    # Sum undercoverage
    sum_undercoverage <- dloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_undercoverage_var := sum(`Coverage`, na.rm = TRUE))
    
    # Extract numeric value
    sum_undercoverage <- as.numeric(sum_undercoverage[[1, 1]])
    
    # Store result
    assign(sum_undercoverage_var, sum_undercoverage)
    
    # Sum GO Demand
    sum_godemand <- dloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average undercoverage relative to demand
    avg_undercoverage_rel2demand <- sum_undercoverage / sum_godemand * (-1)
    
    # Store result
    assign(avg_undercoverage_rel2demand_var, avg_undercoverage_rel2demand)
  }


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_gosupply_var <- paste0("dloa_", year, "_allIntervals_sumGOSupply_abs")
    sum_godemand_var <- paste0("dloa_", year, "_allIntervals_sumGODemand_abs")
    avg_coverage_rel2demand_var <- paste0("dloa_", year, "_allIntervals_avgCoverage_rel2demand_dec")
    
    # Sum GO Supply
    sum_gosupply <- dloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_gosupply_var := sum(`GO Supply`, na.rm = TRUE))
    
    # Extract numeric value
    sum_gosupply <- as.numeric(sum_gosupply[[1, 1]])
    
    # Store result
    assign(sum_gosupply_var, sum_gosupply)
    
    # Sum GO Demand
    sum_godemand <- dloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average coverage relative to demand
    avg_coverage_rel2demand <- sum_gosupply / sum_godemand
    
    # Store result
    assign(avg_coverage_rel2demand_var, avg_coverage_rel2demand)
  }

############################ FOR TOTAL TIMEFRAME ############################

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
dloa_2016to21_uncoveredIntervals_quantity_abs <- dloa %>%
  filter(`Coverage` <0)%>%
  summarize(dloa_2016to21_uncoveredIntervals_quantity_abs = sum(Coverage <0, na.rm = TRUE))
dloa_2016to21_uncoveredIntervals_quantity_abs <- as.numeric(dloa_2016to21_uncoveredIntervals_quantity_abs[[1,1]])


dloa_2016to21_allIntervals_quantity_abs <- dloa %>%
  count()
dloa_2016to21_uncoveredIntervals_quantity_rel_dec <- dloa_2016to21_uncoveredIntervals_quantity_abs/dloa_2016to21_allIntervals_quantity_abs
dloa_2016to21_uncoveredIntervals_quantity_rel_dec <- as.numeric(dloa_2016to21_uncoveredIntervals_quantity_rel_dec[[1,1]])


dloa_2016to21_coveredIntervals_quantity_rel_dec <- 1-dloa_2016to21_uncoveredIntervals_quantity_rel_dec


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- dloa %>%
  filter(`Coverage` <0)%>%
  summarize(dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs = sum(`Coverage`, na.rm = TRUE))

dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- as.numeric(dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs[[1,1]])

dloa_2016to21_uncoveredIntervals_sumGODemand_abs <- dloa %>%
  filter(`Coverage` <0)%>%
  summarize(dloa_2016to21_uncoveredIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

dloa_2016to21_uncoveredIntervals_sumGODemand_abs <- as.numeric(dloa_2016to21_uncoveredIntervals_sumGODemand_abs[[1,1]])

dloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec <- dloa_2016to21_uncoveredIntervals_sumUndercoverage_abs/dloa_2016to21_uncoveredIntervals_sumGODemand_abs*(-1)


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year)
dloa_2016to21_allIntervals_sumGOSupply_abs <- dloa %>%
  summarize(dloa_2016to21_allIntervals_sumGOSupply_abs = sum(`GO Supply`, na.rm = TRUE))

dloa_2016to21_allIntervals_sumGOSupply_abs <- as.numeric(dloa_2016to21_allIntervals_sumGOSupply_abs[[1,1]])

dloa_2016to21_allIntervals_sumGODemand_abs <- dloa %>%
  summarize(dloa_2016to21_allIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

dloa_2016to21_allIntervals_sumGODemand_abs <- as.numeric(dloa_2016to21_allIntervals_sumGODemand_abs[[1,1]])

dloa_2016to21_allIntervals_avgCoverage_rel2demand_dec <- dloa_2016to21_allIntervals_sumGOSupply_abs/dloa_2016to21_allIntervals_sumGODemand_abs


#############################################################################################
################################ WEEKLY LEVEL OF ANALYSIS ###################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
wloa <- read_excel(file_path, sheet = "weekly_coverages")

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


############################### DATA ANALYSIS ###############################
#############################################################################

########################### FOR INDIVIDUAL YEARS ############################

# Years to loop through
years <- c("2016", "2017", "2018", "2019", "2020", "2021")

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
  # Loop through each year
  for (year in years) {
    # Create variable names
    all_intervals_var <- paste0("wloa_", year, "_allIntervals_quantity_abs")
    uncovered_intervals_var <- paste0("wloa_", year, "_uncoveredIntervals_quantity_abs")
    rel_dec_var <- paste0("wloa_", year, "_uncoveredIntervals_quantity_rel_dec")
    covered_rel_dec_var <- paste0("wloa_", year, "_coveredIntervals_quantity_rel_dec")
    
    # Count all intervals
    all_intervals <- wloa %>%
      filter(`Year Index` == year) %>%
      count()
    
    # Store count result
    assign(all_intervals_var, as.numeric(all_intervals[[1, 1]]))
    
    # Filter and summarize uncovered intervals
    uncovered_intervals <- wloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!uncovered_intervals_var := sum(Coverage < 0, na.rm = TRUE))
    
    # Extract numeric value
    uncovered_intervals <- as.numeric(uncovered_intervals[[1, 1]])
    
    # Store uncovered intervals result
    assign(uncovered_intervals_var, uncovered_intervals)
    
    # Calculate relative quantity
    rel_dec <- uncovered_intervals / as.numeric(all_intervals[[1, 1]])
    
    # Store relative quantity result
    assign(rel_dec_var, rel_dec)
    
    # Calculate covered intervals relative quantity
    covered_rel_dec <- 1 - rel_dec
    
    # Store covered intervals relative quantity result
    assign(covered_rel_dec_var, covered_rel_dec)
  }


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_undercoverage_var <- paste0("wloa_", year, "_uncoveredIntervals_sumUndercoverage_abs")
    sum_godemand_var <- paste0("wloa_", year, "_uncoveredIntervals_sumGODemand_abs")
    avg_undercoverage_rel2demand_var <- paste0("wloa_", year, "_uncoveredIntervals_avgUndercoverage_rel2demand_dec")
    
    # Sum undercoverage
    sum_undercoverage <- wloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_undercoverage_var := sum(`Coverage`, na.rm = TRUE))
    
    # Extract numeric value
    sum_undercoverage <- as.numeric(sum_undercoverage[[1, 1]])
    
    # Store result
    assign(sum_undercoverage_var, sum_undercoverage)
    
    # Sum GO Demand
    sum_godemand <- wloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average undercoverage relative to demand
    avg_undercoverage_rel2demand <- sum_undercoverage / sum_godemand * (-1)
    
    # Store result
    assign(avg_undercoverage_rel2demand_var, avg_undercoverage_rel2demand)
  }


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_gosupply_var <- paste0("wloa_", year, "_allIntervals_sumGOSupply_abs")
    sum_godemand_var <- paste0("wloa_", year, "_allIntervals_sumGODemand_abs")
    avg_coverage_rel2demand_var <- paste0("wloa_", year, "_allIntervals_avgCoverage_rel2demand_dec")
    
    # Sum GO Supply
    sum_gosupply <- wloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_gosupply_var := sum(`GO Supply`, na.rm = TRUE))
    
    # Extract numeric value
    sum_gosupply <- as.numeric(sum_gosupply[[1, 1]])
    
    # Store result
    assign(sum_gosupply_var, sum_gosupply)
    
    # Sum GO Demand
    sum_godemand <- wloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average coverage relative to demand
    avg_coverage_rel2demand <- sum_gosupply / sum_godemand
    
    # Store result
    assign(avg_coverage_rel2demand_var, avg_coverage_rel2demand)
  }

############################ FOR TOTAL TIMEFRAME ############################

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
wloa_2016to21_uncoveredIntervals_quantity_abs <- wloa %>%
  filter(`Coverage` <0)%>%
  summarize(wloa_2016to21_uncoveredIntervals_quantity_abs = sum(Coverage <0, na.rm = TRUE))
wloa_2016to21_uncoveredIntervals_quantity_abs <- as.numeric(wloa_2016to21_uncoveredIntervals_quantity_abs[[1,1]])

wloa_2016to21_allIntervals_quantity_abs <- wloa %>%
  count()
wloa_2016to21_uncoveredIntervals_quantity_rel_dec <- wloa_2016to21_uncoveredIntervals_quantity_abs/wloa_2016to21_allIntervals_quantity_abs
wloa_2016to21_uncoveredIntervals_quantity_rel_dec <- as.numeric(wloa_2016to21_uncoveredIntervals_quantity_rel_dec[[1,1]])

wloa_2016to21_coveredIntervals_quantity_rel_dec <- 1-wloa_2016to21_uncoveredIntervals_quantity_rel_dec


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- wloa %>%
  filter(`Coverage` <0)%>%
  summarize(wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs = sum(`Coverage`, na.rm = TRUE))

wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- as.numeric(wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs[[1,1]])

wloa_2016to21_uncoveredIntervals_sumGODemand_abs <- wloa %>%
  filter(`Coverage` <0)%>%
  summarize(wloa_2016to21_uncoveredIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

wloa_2016to21_uncoveredIntervals_sumGODemand_abs <- as.numeric(wloa_2016to21_uncoveredIntervals_sumGODemand_abs[[1,1]])

wloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec <- wloa_2016to21_uncoveredIntervals_sumUndercoverage_abs/wloa_2016to21_uncoveredIntervals_sumGODemand_abs*(-1)


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year) 
wloa_2016to21_allIntervals_sumGOSupply_abs <- wloa %>%
  summarize(wloa_2016to21_allIntervals_sumGOSupply_abs = sum(`GO Supply`, na.rm = TRUE))

wloa_2016to21_allIntervals_sumGOSupply_abs <- as.numeric(wloa_2016to21_allIntervals_sumGOSupply_abs[[1,1]])

wloa_2016to21_allIntervals_sumGODemand_abs <- wloa %>%
  summarize(wloa_2016to21_allIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

wloa_2016to21_allIntervals_sumGODemand_abs <- as.numeric(wloa_2016to21_allIntervals_sumGODemand_abs[[1,1]])

wloa_2016to21_allIntervals_avgCoverage_rel2demand_dec <- wloa_2016to21_allIntervals_sumGOSupply_abs/wloa_2016to21_allIntervals_sumGODemand_abs


#############################################################################################
################################ MONTHLY LEVEL OF ANALYSIS ##################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
mloa <- read_excel(file_path, sheet = "monthly_coverages")

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


############################### DATA ANALYSIS ###############################
#############################################################################

########################### FOR INDIVIDUAL YEARS ############################

# Years to loop through
years <- c("2016", "2017", "2018", "2019", "2020", "2021")

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
  # Loop through each year
  for (year in years) {
    # Create variable names
    all_intervals_var <- paste0("mloa_", year, "_allIntervals_quantity_abs")
    uncovered_intervals_var <- paste0("mloa_", year, "_uncoveredIntervals_quantity_abs")
    rel_dec_var <- paste0("mloa_", year, "_uncoveredIntervals_quantity_rel_dec")
    covered_rel_dec_var <- paste0("mloa_", year, "_coveredIntervals_quantity_rel_dec")
    
    # Count all intervals
    all_intervals <- mloa %>%
      filter(`Year Index` == year) %>%
      count()
    
    # Store count result
    assign(all_intervals_var, as.numeric(all_intervals[[1, 1]]))
    
    # Filter and summarize uncovered intervals
    uncovered_intervals <- mloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!uncovered_intervals_var := sum(Coverage < 0, na.rm = TRUE))
    
    # Extract numeric value
    uncovered_intervals <- as.numeric(uncovered_intervals[[1, 1]])
    
    # Store uncovered intervals result
    assign(uncovered_intervals_var, uncovered_intervals)
    
    # Calculate relative quantity
    rel_dec <- uncovered_intervals / as.numeric(all_intervals[[1, 1]])
    
    # Store relative quantity result
    assign(rel_dec_var, rel_dec)
    
    # Calculate covered intervals relative quantity
    covered_rel_dec <- 1 - rel_dec
    
    # Store covered intervals relative quantity result
    assign(covered_rel_dec_var, covered_rel_dec)
  }


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_undercoverage_var <- paste0("mloa_", year, "_uncoveredIntervals_sumUndercoverage_abs")
    sum_godemand_var <- paste0("mloa_", year, "_uncoveredIntervals_sumGODemand_abs")
    avg_undercoverage_rel2demand_var <- paste0("mloa_", year, "_uncoveredIntervals_avgUndercoverage_rel2demand_dec")
    
    # Sum undercoverage
    sum_undercoverage <- mloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_undercoverage_var := sum(`Coverage`, na.rm = TRUE))
    
    # Extract numeric value
    sum_undercoverage <- as.numeric(sum_undercoverage[[1, 1]])
    
    # Store result
    assign(sum_undercoverage_var, sum_undercoverage)
    
    # Sum GO Demand
    sum_godemand <- mloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average undercoverage relative to demand
    avg_undercoverage_rel2demand <- sum_undercoverage / sum_godemand * (-1)
    
    # Store result
    assign(avg_undercoverage_rel2demand_var, avg_undercoverage_rel2demand)
  }


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year) 
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_gosupply_var <- paste0("mloa_", year, "_allIntervals_sumGOSupply_abs")
    sum_godemand_var <- paste0("mloa_", year, "_allIntervals_sumGODemand_abs")
    avg_coverage_rel2demand_var <- paste0("mloa_", year, "_allIntervals_avgCoverage_rel2demand_dec")
    
    # Sum GO Supply
    sum_gosupply <- mloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_gosupply_var := sum(`GO Supply`, na.rm = TRUE))
    
    # Extract numeric value
    sum_gosupply <- as.numeric(sum_gosupply[[1, 1]])
    
    # Store result
    assign(sum_gosupply_var, sum_gosupply)
    
    # Sum GO Demand
    sum_godemand <- mloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average coverage relative to demand
    avg_coverage_rel2demand <- sum_gosupply / sum_godemand
    
    # Store result
    assign(avg_coverage_rel2demand_var, avg_coverage_rel2demand)
  }

############################ FOR TOTAL TIMEFRAME ############################

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
mloa_2016to21_uncoveredIntervals_quantity_abs <- mloa %>%
  filter(`Coverage` <0)%>%
  summarize(mloa_2016to21_uncoveredIntervals_quantity_abs = sum(Coverage <0, na.rm = TRUE))
mloa_2016to21_uncoveredIntervals_quantity_abs <- as.numeric(mloa_2016to21_uncoveredIntervals_quantity_abs[[1,1]])

mloa_2016to21_allIntervals_quantity_abs <- mloa %>%
  count()
mloa_2016to21_uncoveredIntervals_quantity_rel_dec <- mloa_2016to21_uncoveredIntervals_quantity_abs/mloa_2016to21_allIntervals_quantity_abs
mloa_2016to21_uncoveredIntervals_quantity_rel_dec <- as.numeric(mloa_2016to21_uncoveredIntervals_quantity_rel_dec[[1,1]])

mloa_2016to21_coveredIntervals_quantity_rel_dec <- 1-mloa_2016to21_uncoveredIntervals_quantity_rel_dec


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- mloa %>%
  filter(`Coverage` <0)%>%
  summarize(mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs = sum(`Coverage`, na.rm = TRUE))

mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- as.numeric(mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs[[1,1]])

mloa_2016to21_uncoveredIntervals_sumGODemand_abs <- mloa %>%
  filter(`Coverage` <0)%>%
  summarize(mloa_2016to21_uncoveredIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

mloa_2016to21_uncoveredIntervals_sumGODemand_abs <- as.numeric(mloa_2016to21_uncoveredIntervals_sumGODemand_abs[[1,1]])

mloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec <- mloa_2016to21_uncoveredIntervals_sumUndercoverage_abs/mloa_2016to21_uncoveredIntervals_sumGODemand_abs*(-1)


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year) 
mloa_2016to21_allIntervals_sumGOSupply_abs <- mloa %>%
  summarize(mloa_2016to21_allIntervals_sumGOSupply_abs = sum(`GO Supply`, na.rm = TRUE))

mloa_2016to21_allIntervals_sumGOSupply_abs <- as.numeric(mloa_2016to21_allIntervals_sumGOSupply_abs[[1,1]])

mloa_2016to21_allIntervals_sumGODemand_abs <- mloa %>%
  summarize(mloa_2016to21_allIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

mloa_2016to21_allIntervals_sumGODemand_abs <- as.numeric(mloa_2016to21_allIntervals_sumGODemand_abs[[1,1]])

mloa_2016to21_allIntervals_avgCoverage_rel2demand_dec <- mloa_2016to21_allIntervals_sumGOSupply_abs/mloa_2016to21_allIntervals_sumGODemand_abs


#############################################################################################
############################### QUARTERLY LEVEL OF ANALYSIS #################################
#############################################################################################

#Definition of input path and file name
input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation"
file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")

#Einlesen der hourly_coverage Datei
file_path <- file.path(input_path, file_name)
qloa <- read_excel(file_path, sheet = "quarterly_coverages")

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

############################### DATA ANALYSIS ###############################
#############################################################################

########################### FOR INDIVIDUAL YEARS ############################

# Years to loop through
years <- c("2016", "2017", "2018", "2019", "2020", "2021")

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
  # Loop through each year
  for (year in years) {
    # Create variable names
    all_intervals_var <- paste0("qloa_", year, "_allIntervals_quantity_abs")
    uncovered_intervals_var <- paste0("qloa_", year, "_uncoveredIntervals_quantity_abs")
    rel_dec_var <- paste0("qloa_", year, "_uncoveredIntervals_quantity_rel_dec")
    covered_rel_dec_var <- paste0("qloa_", year, "_coveredIntervals_quantity_rel_dec")
    
    # Count all intervals
    all_intervals <- qloa %>%
      filter(`Year Index` == year) %>%
      count()
    
    # Store count result
    assign(all_intervals_var, as.numeric(all_intervals[[1, 1]]))
    
    # Filter and summarize uncovered intervals
    uncovered_intervals <- qloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!uncovered_intervals_var := sum(Coverage < 0, na.rm = TRUE))
    
    # Extract numeric value
    uncovered_intervals <- as.numeric(uncovered_intervals[[1, 1]])
    
    # Store uncovered intervals result
    assign(uncovered_intervals_var, uncovered_intervals)
    
    # Calculate relative quantity
    rel_dec <- uncovered_intervals / as.numeric(all_intervals[[1, 1]])
    
    # Store relative quantity result
    assign(rel_dec_var, rel_dec)
    
    # Calculate covered intervals relative quantity
    covered_rel_dec <- 1 - rel_dec
    
    # Store covered intervals relative quantity result
    assign(covered_rel_dec_var, covered_rel_dec)
  }


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_undercoverage_var <- paste0("qloa_", year, "_uncoveredIntervals_sumUndercoverage_abs")
    sum_godemand_var <- paste0("qloa_", year, "_uncoveredIntervals_sumGODemand_abs")
    avg_undercoverage_rel2demand_var <- paste0("qloa_", year, "_uncoveredIntervals_avgUndercoverage_rel2demand_dec")
    
    # Sum undercoverage
    sum_undercoverage <- qloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_undercoverage_var := sum(`Coverage`, na.rm = TRUE))
    
    # Extract numeric value
    sum_undercoverage <- as.numeric(sum_undercoverage[[1, 1]])
    
    # Store result
    assign(sum_undercoverage_var, sum_undercoverage)
    
    # Sum GO Demand
    sum_godemand <- qloa %>%
      filter(`Year Index` == year) %>%
      filter(`Coverage` < 0) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average undercoverage relative to demand
    avg_undercoverage_rel2demand <- sum_undercoverage / sum_godemand * (-1)
    
    # Store result
    assign(avg_undercoverage_rel2demand_var, avg_undercoverage_rel2demand)
  }


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year)
  # Loop through each year
  for (year in years) {
    # Create variable names
    sum_gosupply_var <- paste0("qloa_", year, "_allIntervals_sumGOSupply_abs")
    sum_godemand_var <- paste0("qloa_", year, "_allIntervals_sumGODemand_abs")
    avg_coverage_rel2demand_var <- paste0("qloa_", year, "_allIntervals_avgCoverage_rel2demand_dec")
    
    # Sum GO Supply
    sum_gosupply <- qloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_gosupply_var := sum(`GO Supply`, na.rm = TRUE))
    
    # Extract numeric value
    sum_gosupply <- as.numeric(sum_gosupply[[1, 1]])
    
    # Store result
    assign(sum_gosupply_var, sum_gosupply)
    
    # Sum GO Demand
    sum_godemand <- qloa %>%
      filter(`Year Index` == year) %>%
      summarize(!!sum_godemand_var := sum(`GO Demand`, na.rm = TRUE))
    
    # Extract numeric value
    sum_godemand <- as.numeric(sum_godemand[[1, 1]])
    
    # Store result
    assign(sum_godemand_var, sum_godemand)
    
    # Calculate average coverage relative to demand
    avg_coverage_rel2demand <- sum_gosupply / sum_godemand
    
    # Store result
    assign(avg_coverage_rel2demand_var, avg_coverage_rel2demand)
  }

############################ FOR TOTAL TIMEFRAME ############################

### Quantity of Intervals with negative coverage (absolute & relative) and Quantity of Intervals with positive Coverage (relative)
qloa_2016to21_uncoveredIntervals_quantity_abs <- qloa %>%
  filter(`Coverage` <0)%>%
  summarize(qloa_2016to21_uncoveredIntervals_quantity_abs = sum(Coverage <0, na.rm = TRUE))
qloa_2016to21_uncoveredIntervals_quantity_abs <- as.numeric(qloa_2016to21_uncoveredIntervals_quantity_abs[[1,1]])

qloa_2016to21_allIntervals_quantity_abs <- qloa %>%
  count()
qloa_2016to21_uncoveredIntervals_quantity_rel_dec <- qloa_2016to21_uncoveredIntervals_quantity_abs/qloa_2016to21_allIntervals_quantity_abs
qloa_2016to21_uncoveredIntervals_quantity_rel_dec <- as.numeric(qloa_2016to21_uncoveredIntervals_quantity_rel_dec[[1,1]])

qloa_2016to21_coveredIntervals_quantity_rel_dec <- 1-qloa_2016to21_uncoveredIntervals_quantity_rel_dec


### Average Undercoverage of the uncovered intervals depending on level of analysis relative to GO Demand of these intervals per year (decimal; relative to GO Demand - SUM Undercoverage/SUM GO Demand per year)
qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- qloa %>%
  filter(`Coverage` <0)%>%
  summarize(qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs = sum(`Coverage`, na.rm = TRUE))

qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs <- as.numeric(qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs[[1,1]])

qloa_2016to21_uncoveredIntervals_sumGODemand_abs <- qloa %>%
  filter(`Coverage` <0)%>%
  summarize(qloa_2016to21_uncoveredIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

qloa_2016to21_uncoveredIntervals_sumGODemand_abs <- as.numeric(qloa_2016to21_uncoveredIntervals_sumGODemand_abs[[1,1]])

qloa_2016to21_uncoveredIntervals_avgUndercoverage_rel2demand_dec <- qloa_2016to21_uncoveredIntervals_sumUndercoverage_abs/qloa_2016to21_uncoveredIntervals_sumGODemand_abs*(-1)


### Cross-Check - should be over 1: Average coverage of all intervals depending on level of analysis relative to GO Demand per year (decimal; relative to GO Demand - SUM Coverage/SUM GO Demand per year)
qloa_2016to21_allIntervals_sumGOSupply_abs <- qloa %>%
  summarize(qloa_2016to21_allIntervals_sumGOSupply_abs = sum(`GO Supply`, na.rm = TRUE))

qloa_2016to21_allIntervals_sumGOSupply_abs <- as.numeric(qloa_2016to21_allIntervals_sumGOSupply_abs[[1,1]])

qloa_2016to21_allIntervals_sumGODemand_abs <- qloa %>%
  summarize(qloa_2016to21_allIntervals_sumGODemand_abs = sum(`GO Demand`, na.rm = TRUE))

qloa_2016to21_allIntervals_sumGODemand_abs <- as.numeric(qloa_2016to21_allIntervals_sumGODemand_abs[[1,1]])

qloa_2016to21_allIntervals_avgCoverage_rel2demand_dec <- qloa_2016to21_allIntervals_sumGOSupply_abs/qloa_2016to21_allIntervals_sumGODemand_abs