###################################################################################################################
###################################################################################################################
############################### MLOA & QLOA - DISTRIBUTIONS OVER MONTHS AND QUARTERS ##############################
###################################################################################################################
###################################################################################################################

#Set correct Working Directory
setwd("C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/3_Data Analysis/2_R Results")

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

# Define the dataset names
data_types <- c("ALL RES")

for (data_type in data_types) {
  # Define the input and output path as well as file name
  input_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/2_entso-e & AIB Data Merging & Coverage Calculation/"
  file_name <- paste0("20240627 Data Merging & Coverage Calculation_", data_type, "_vPub.xlsx")
  output_path <- "C:/Users/ga58qeg/OneDrive - TUM/Shedding Light on Green Claims - Supplementary Data/3_Data Analysis/2_R Results"
  
  #Read in monthly_coverage and quarterly_coverage File
  file_path <- file.path(input_path, file_name)
  mloa <- read_excel(file_path, sheet = "monthly_coverages")
  qloa <- read_excel(file_path, sheet = "quarterly_coverages")
  
  #############################################################################################
  ####################################### DATA CLEANING #######################################
  #############################################################################################
  
  ################################# MONTHLY LEVEL OF ANALYSIS #################################
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
  
  ################################# QUARTERLY LEVEL OF ANALYSIS ################################
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
  
  #############################################################################################
  ####################################### DATA ANALYSIS #######################################
  ############################################################################################# 
  
  ################################# MONTHLY LEVEL OF ANALYSIS #################################
  
  ##1: Amount of uncovered intervals relative to amount of all intervals by month of year and year (decimal)
    #As Dataframe
      mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_dec <- mloa %>%
        group_by(`Month Index`,`Year Index`) %>%
        summarise(Negative_Proportion = mean(Coverage < 0))
      
      #View(mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_dec)
      
      output_file_name <- paste0(data_type, "_DF11.csv")
      write.csv(mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)
      
    #As Plot (Barchart)
      barchart_mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_perc <- ggplot(mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_dec, aes(x = factor(`Month Index`), y = Negative_Proportion, fill = factor(`Year Index`))) +
        geom_bar(stat = "identity", fill = "grey", width = 0.6) +
        labs(
          title = "Distribution of uncovered months per year (considering the years 2016 to 2021)",
          x = "Month Index",
          y = "Uncovered time intervals [in % of total intervals]"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()+
        facet_wrap(~`Year Index`, scales = "free_y") #Create separate plots for each year
      
      #print(barchart_mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_perc)
      
      #Save the plot as an image in high resolution
      output_file_name <- paste0(data_type, "_P7rtDF11.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_mloa_uncoveredIntervals_quantity_byMonthOfYearAndYear_rel_perc, width = 8, height = 6, dpi = 300)# Save the plot as a high-resolution image
    
  ##2: Amount of uncovered intervals relative to amount of all intervals by month of year (decimal); spanning over complete timeframe of analysis
    #As Dataframe
      mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_dec <- mloa %>%
        group_by(`Month Index`) %>%
        summarise(Negative_Proportion = mean(Coverage < 0))
      
      #View(mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_dec)
      
      output_file_name <- paste0(data_type, "_DF12.csv")
      write.csv(mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)
    
    #As Plot (Barchart)
      barchart_mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_perc <- ggplot(mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_dec, aes(x = factor(`Month Index`), y = Negative_Proportion)) +
        geom_bar(stat = "identity", fill = "grey", width = 0.6) +
        labs(
          title = "Distribution of uncovered months over the year (considering the years 2016 to 2021)",
          x = "Month Index",
          y = "Uncovered time intervals [in % of total intervals]"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
      
      #print(barchart_mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_perc)
      
      #Save the plot as an image in high resolution
      output_file_name <- paste0(data_type, "_P8rtDF12.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_mloa_uncoveredIntervals_quantity_byMonthOfYear_rel_perc, width = 8, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
  
  ##3: Average Undercoverage throughout the uncovered intervals by Month of Year, Average Undercoverage (percent); relative value, not calculated by timestamp but calculated via SUM Shortage/SUM GO Demand per Month of Year
    # Step 1: 
      mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- mloa %>%
      group_by(`Month Index`) %>%
      summarise(
        SumCoverage = sum(ifelse(`Coverage` < 0, `Coverage`, 0)),
        SumGODemand = sum(ifelse(`Coverage` < 0, `GO Demand`, 0)),
        AvgUndercoverage = (SumCoverage / SumGODemand) * 100  
      ) %>%
      ungroup()  # Remove grouping
    
      #View(mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_DF13.csv")
      write.csv(mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, file.path(output_path, output_file_name), row.names = TRUE)
      
    # Step 2: Creating a ggplot geom_point
      pointchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- ggplot(mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, aes(x = `Month Index`, y = AvgUndercoverage)) +
        geom_point() +
        labs(
          title = "Average Undercoverage by Month of Year (2016 to 2021)",
          x = "Month Index",
          y = "Average Undercoverage (%)"
        )
      
      #print(pointchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_P9rtDF13.png")
      ggsave(file.path(output_path, output_file_name), plot = pointchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
    # Step 3: Creating a ggplot geom_bar
      barchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- ggplot(mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, aes(x = `Month Index`, y = AvgUndercoverage)) +
        geom_bar(stat = "identity", fill = "grey") +
        labs(
          title = "Average Undercoverage by Month of Year (2016 to 2021)",
          x = "Month Index",
          y = "Average Undercoverage (%)"
        ) +
        theme_minimal()
      
      #print(barchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_P10rtDF13.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_mloa_uncoveredIntervals_avgUndercoverage_byMonthofYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
  
  ################################# QUARTERLY LEVEL OF ANALYSIS ################################
  
  ##1: Amount of uncovered intervals relative to amount of all intervals by quarter of year and year (decimal)
    #As Dataframe
      qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_dec <- qloa %>%
        group_by(`Quarter Index`,`Year Index`) %>%
        summarise(Negative_Proportion = mean(Coverage < 0))
      
      #View(qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_dec)
      
      output_file_name <- paste0(data_type, "_DF15.csv")
      write.csv(qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)
    
    #As Plot (Barchart)
      barchart_qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_perc <- ggplot(qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_dec, aes(x = factor(`Quarter Index`), y = Negative_Proportion, fill = factor(`Year Index`))) +
        geom_bar(stat = "identity", fill = "grey", width = 0.6) +
        labs(
          title = "Distribution of uncovered quarters per year (considering the years 2016 to 2021)",
          x = "Quarter Index",
          y = "Uncovered time intervals [in % of total intervals]"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()+
        facet_wrap(~`Year Index`, scales = "free_y") #Create separate plots for each year
      
      print(barchart_qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_perc)
      
      # Save the plot as an image
      output_file_name <- paste0(data_type, "_P11rtDF15.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_qloa_uncoveredIntervals_quantity_byQuarterofYearAndYear_rel_perc, width = 8, height = 6, dpi = 300)# Save the plot as a high-resolution image
    
  
  ##2: Amount of uncovered intervals relative to amount of all intervals by quarter of year (decimal); spanning over complete timeframe of analysis
    #As Dataframe
      qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_dec <- qloa %>%
        group_by(`Quarter Index`) %>%
        summarise(Negative_Proportion = mean(Coverage < 0))
      
      #View(qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_dec)
      
      output_file_name <- paste0(data_type, "_DF16.csv")
      write.csv(qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_dec, file.path(output_path, output_file_name), row.names = TRUE)
      
    #As Plot (Barchart)
      barchart_qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_perc <- ggplot(qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_dec, aes(x = factor(`Quarter Index`), y = Negative_Proportion)) +
        geom_bar(stat = "identity", fill = "grey", width = 0.6) +
        labs(
          title = "Distribution of uncovered quarters over the year (considering the years 2016 to 2021)",
          x = "Quarter Index",
          y = "Uncovered time intervals [in % of total intervals]"
        ) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
      
      print(barchart_qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_perc)
      
      # Save the plot as an image
      output_file_name <- paste0(data_type, "_P12rtDF16.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_qloa_uncoveredIntervals_quantity_byQuarterOfYear_rel_perc, width = 8, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
  
  ##3: Average Undercoverage throughout the uncovered intervals by Quarter of Year, Average Undercoverage (percent); relative value, not calculated by timestamp but calculated via SUM Shortage/SUM GO Demand per Quarter of Year
    # Step 1: 
        qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- qloa %>%
        group_by(`Quarter Index`) %>%
        summarise(
          SumCoverage = sum(ifelse(`Coverage` < 0, `Coverage`, 0)),
          SumGODemand = sum(ifelse(`Coverage` < 0, `GO Demand`, 0)),
          AvgUndercoverage = (SumCoverage / SumGODemand) * 100  
        ) %>%
        ungroup()  # Remove grouping
      
      #View(qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_DF17.csv")
      write.csv(qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, file.path(output_path, output_file_name), row.names = TRUE)
      
    # Step 2: Creating a ggplot geom_point
      pointchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- ggplot(qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, aes(x = `Quarter Index`, y = AvgUndercoverage)) +
        geom_point() +
        labs(
          title = "Average Undercoverage by Quarter of Year (2016 to 2021)",
          x = "Quarter Index",
          y = "Average Undercoverage (%)"
        )
      
      #print(pointchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_P13rtDF17.png")
      ggsave(file.path(output_path, output_file_name), plot = pointchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
      
    # Step 3: Creating a ggplot geom_bar
      barchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc <- ggplot(qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, aes(x = `Quarter Index`, y = AvgUndercoverage)) +
        geom_bar(stat = "identity", fill = "grey") +
        labs(
          title = "Average Undercoverage by Quarter of Year (2016 to 2021)",
          x = "Quarter Index",
          y = "Average Undercoverage (%)"
        ) +
        theme_minimal()
      
      #print(barchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc)
      
      output_file_name <- paste0(data_type, "_P14rtDF17.png")
      ggsave(file.path(output_path, output_file_name), plot = barchart_qloa_uncoveredIntervals_avgUndercoverage_byQuarterOfYear_viaSumShortageDividedBySumGODemand_rel2demand_perc, width = 10, height = 6, dpi = 300)# Save the plot as a high-resolution image
}